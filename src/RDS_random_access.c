/****************************************************************************
 ****************************************************************************
 *   Random access to the elements of a serialized atomic vector or array   *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************
 ****************************************************************************/
#include "XVector.h"
#include "IRanges_interface.h"
#include "S4Vectors_interface.h"

#include <limits.h>  // for INT_MAX

static int verbose = 0;


#define	IS_ATOMIC_TYPE(type) \
	((type) == LGLSXP  || (type) == INTSXP || (type) == REALSXP || \
	 (type) == CPLXSXP || (type) == RAWSXP || (type) == STRSXP)

/* Equivalent to the DATAPTR() macro defined in Rinternals.h
   For some reason I don't understand, I can't use the DATAPTR() macro. */
static void *dataptr(SEXP x)
{
	switch (TYPEOF(x)) {
	    case LGLSXP:
		return LOGICAL(x);
	    case INTSXP:
		return INTEGER(x);
	    case REALSXP:
		return REAL(x);
	    case CPLXSXP:
		return COMPLEX(x);
	    case RAWSXP:
		return RAW(x);
	}
	error("XVector internal error in dataptr(): "
	      "%s type not supported", CHAR(type2str(TYPEOF(x))));
}

static size_t type2atomsize(SEXPTYPE type)
{
	switch (type) {
	    case LGLSXP:
	    case INTSXP:
		return sizeof(int);
	    case REALSXP:
		return sizeof(double);
	    case CPLXSXP:
		return sizeof(Rcomplex);
	    case RAWSXP:
		return sizeof(Rbyte);
	}
	error("XVector internal error in type2atomsize(): "
	      "undefined atom size for type %s", CHAR(type2str(type)));
}


/****************************************************************************
 * A simple RDS parser
 *
 * The current implementation assumes that:
 *  - sizeof(int) = 4
 *  - sizeof(double) = sizeof(long long int) = 8
 *  - platform is little endian
 */

static void printf_margin(int indent)
{
	int i;

	for (i = 0; i < indent; i++)
		printf("  ");
	return;
}

#define	PRINTIFVERBOSE1(msg) \
{ \
	if (verbose) { \
		printf_margin(indent); \
		printf(msg); \
		printf("\n"); \
	} \
}
#define	PRINTIFVERBOSE2(format, value) \
{ \
	if (verbose) { \
		printf_margin(indent); \
		printf(format, value); \
		printf("\n"); \
	} \
}

static SEXPTYPE RDStype2Rtype(unsigned char type)
{
	/* NULL type is 0xfe in RDS, not NILSXP */
	return type == 0xfe ? NILSXP : type;
}

static const char *RDS_read_bytes(SEXP filexp, size_t n, int parse_only,
		unsigned char *buf)
{
	int n2, n3;
	static char errmsg_buf[40];

	/* Because the 'buf_size' argument in _filexp_read() must be an
	   int, we cannot read more than INT_MAX bytes per call to
	   _filexp_read(). */
	while (n > 0) {
		n2 = n <= INT_MAX ? n : INT_MAX;
		if (parse_only) {
			_filexp_seek(filexp, n2, SEEK_CUR);
		} else {
			n3 = _filexp_read(filexp, (char *) buf, n2);
			if (n3 != n2) {
				snprintf(errmsg_buf, sizeof(errmsg_buf),
				    "read error or unexpected end of file");
				return errmsg_buf;
			}
			buf += n2;
		}
		n -= n2;
	}
	return NULL;
}

static void RDS_read_chars(SEXP filexp, size_t n, int parse_only,
		CharAE *string_buf)
{
	const char *errmsg;

	if (!parse_only && n > string_buf->_buflength)
		CharAE_extend(string_buf, n);
	errmsg = RDS_read_bytes(filexp, n, parse_only,
				(unsigned char *) string_buf->elts);
	if (errmsg != NULL)
		error(errmsg);
	if (!parse_only)
		CharAE_set_nelt(string_buf, n);
	return;
}

static void swap_4_bytes(unsigned char *bytes)
{
	unsigned int *tmp;

	tmp = (unsigned int *) bytes;
	*tmp = (*tmp << 24) |
	       ((*tmp & 0xff00) << 8) |
	       ((*tmp & 0xff0000) >> 8) |
	       (*tmp >> 24);
	return;
}

static const char *RDS_read_ints(SEXP filexp, size_t n, int parse_only,
		int *buf)
{
	const char *errmsg;
	size_t i;

	/* Integer values are *always* 4 bytes in an RDS file, even if
	   sizeof(int) != 4 on the machine running this code! */
	errmsg = RDS_read_bytes(filexp, n * 4, parse_only,
				(unsigned char *) buf);
	if (errmsg != NULL)
		return errmsg;
	/* FIXME: Don't swap bytes if platform is big endian */
	if (!parse_only)
		for (i = 0; i < n; i++)
			swap_4_bytes((unsigned char *) (buf + i));
	return NULL;
}

static void swap_8_bytes(unsigned char *bytes)
{
	unsigned long long int *tmp;

	tmp = (unsigned long long int *) bytes;
	*tmp = (*tmp << 56) |
	       ((*tmp & 0xff00) << 40) |
	       ((*tmp & 0xff0000) << 24) |
	       ((*tmp & 0xff000000) << 8) |
	       ((*tmp & 0xff00000000) >> 8) |
	       ((*tmp & 0xff0000000000) >> 24) |
	       ((*tmp & 0xff000000000000) >> 40) |
	       (*tmp >> 56);
	return;
}

static const char *RDS_read_doubles(SEXP filexp, size_t n, int parse_only,
		double *buf)
{
	const char *errmsg;
	size_t i;

	/* Double values are *always* 8 bytes in an RDS file, even if
	   sizeof(double) != 8 on the machine running this code! */
	errmsg = RDS_read_bytes(filexp, n * 8, parse_only,
				(unsigned char *) buf);
	if (errmsg != NULL)
		return errmsg;
	/* FIXME: Don't swap bytes if platform is big endian */
	if (!parse_only)
		for (i = 0; i < n; i++)
			swap_8_bytes((unsigned char *) (buf + i));
	return NULL;
}

static R_xlen_t RDS_read_vector_length(SEXP filexp)
{
	const char *errmsg;
	const unsigned char LONG_LENGTH_bytes[4] = {0xff, 0xff, 0xff, 0xff};
	unsigned char buf[8];
	int *length;
	long long int *long_length;

	errmsg = RDS_read_bytes(filexp, 4, 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, LONG_LENGTH_bytes, 4) != 0) {
		swap_4_bytes(buf);
		length = (int *) buf;
		return (R_xlen_t) *length;
	}
	errmsg = RDS_read_bytes(filexp, 8, 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	swap_8_bytes(buf);
	long_length = (long long int *) buf;
	return (R_xlen_t) *long_length;
}

SEXP get_typeof_and_length_as_list(SEXP filexp, SEXPTYPE type)
{
	R_xlen_t length;
	SEXP ans, ans_elt, ans_names, ans_names_elt;

	length = type == NILSXP ? 0 : RDS_read_vector_length(filexp);

	ans = PROTECT(NEW_LIST(2));

	/* Set "typeof" element. */
	ans_elt = PROTECT(ScalarString(type2str(type)));
	SET_VECTOR_ELT(ans, 0, ans_elt);
	UNPROTECT(1);
	/* Set "length" element. */
	if (length <= INT_MAX)
		ans_elt = PROTECT(ScalarInteger((int) length));
	else
		ans_elt = PROTECT(ScalarReal((double) length));
	SET_VECTOR_ELT(ans, 1, ans_elt);
	UNPROTECT(1);

	ans_names = PROTECT(NEW_CHARACTER(2));
	ans_names_elt = PROTECT(mkChar("typeof"));
	SET_STRING_ELT(ans_names, 0, ans_names_elt);
	UNPROTECT(1);
	ans_names_elt = PROTECT(mkChar("length"));
	SET_STRING_ELT(ans_names, 1, ans_names_elt);
	UNPROTECT(1);
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);

	UNPROTECT(1);
	return ans;
}

/* Encoded strings not supported. */
static int RDS_read_string(SEXP filexp, int parse_only, CharAE *string_buf)
{
	const char *errmsg;
	const unsigned char NA_STRING_bytes[4] = {0xff, 0xff, 0xff, 0xff};
	unsigned char buf[4];
	R_xlen_t ans_len;

	errmsg = RDS_read_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (buf[0] != 0 || buf[2] != 0 || buf[3] != 0x09)
		error("unsupported RDS file");
	if (buf[1] == 0) {
		errmsg = RDS_read_bytes(filexp, sizeof(buf), 0, buf);
		if (errmsg != NULL)
			error(errmsg);
		if (memcmp(buf, NA_STRING_bytes, sizeof(buf)) != 0)
			error("unsupported RDS file");
		return 1;
	}
	if (buf[1] != 0x04)
		error("unsupported string header");
	ans_len = RDS_read_vector_length(filexp);
	RDS_read_chars(filexp, (size_t) ans_len, parse_only, string_buf);
	return 0;
}

/* Return R_NilValue if parse_only != 0. */
static SEXP RDS_read_character_vector(SEXP filexp, int parse_only,
		CharAE *string_buf, int indent)
{
	R_xlen_t ans_len, i;
	int is_na;
	SEXP ans, ans_elt;

	PRINTIFVERBOSE1("start reading character vector");
	ans_len = RDS_read_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(NEW_CHARACTER(ans_len));
	for (i = 0; i < ans_len; i++) {
		is_na = RDS_read_string(filexp, parse_only, string_buf);
		if (parse_only)
			continue;
		if (is_na) {
			SET_STRING_ELT(ans, i, NA_STRING);
		} else {
			PROTECT(ans_elt = new_CHARSXP_from_CharAE(string_buf));
			SET_STRING_ELT(ans, i, ans_elt);
			UNPROTECT(1);
		}
	}
	if (!parse_only)
		UNPROTECT(1);
	PRINTIFVERBOSE1("done reading character vector");
	return ans;
}

/* Return R_NilValue if parse_only != 0. */
static SEXP RDS_read_atomic_vector(SEXP filexp, SEXPTYPE type,
		int parse_only, int indent)
{
	R_xlen_t ans_len;
	SEXP ans;
	const char *errmsg;

	PRINTIFVERBOSE2("start reading %s vector", CHAR(type2str(type)));
	ans_len = RDS_read_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(allocVector(type, ans_len));
	switch (type) {
	    case LGLSXP:
	    case INTSXP:
		errmsg = RDS_read_ints(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case REALSXP:
		errmsg = RDS_read_doubles(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case CPLXSXP:
		errmsg = RDS_read_doubles(filexp, (size_t) ans_len * 2,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case RAWSXP:
		errmsg = RDS_read_bytes(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    default:
		error("XVector internal error in RDS_read_atomic_vector(): "
		      "unexpected type: %s", CHAR(type2str(type)));
	}
	if (errmsg != NULL)
		error(errmsg);
	if (!parse_only)
		UNPROTECT(1);
	PRINTIFVERBOSE2("done reading %s vector", CHAR(type2str(type)));
	return ans;
}

static SEXP RDS_read_object(SEXP filexp, int mode, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *symbols_buf, int indent);

/* Return R_NilValue if parse_only != 0. */
static SEXP RDS_read_list(SEXP filexp, int parse_only,
		CharAE *string_buf, CharAEAE *symbols_buf, int indent)
{
	R_xlen_t ans_len, i;
	SEXP ans, ans_elt;

	PRINTIFVERBOSE1("start reading list object");
	ans_len = RDS_read_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(NEW_LIST(ans_len));
	for (i = 0; i < ans_len; i++) {
		ans_elt = RDS_read_object(filexp, parse_only, R_NilValue,
					  string_buf, symbols_buf,
					  indent + 1);
		if (parse_only)
			continue;
		PROTECT(ans_elt);
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	if (!parse_only)
		UNPROTECT(1);
	PRINTIFVERBOSE1("done reading list object");
	return ans;
}

static int RDS_read_attrib_separator(SEXP filexp)
{
	const char *errmsg;
	const unsigned char EOA_bytes[4] = {0x00, 0x00, 0x00, 0xfe},
			    ATTRIB_SEP_bytes[4] = {0x00, 0x00, 0x04, 0x02};
	unsigned char buf[4];

	errmsg = RDS_read_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, EOA_bytes, sizeof(buf)) == 0)
		return 0;
	if (memcmp(buf, ATTRIB_SEP_bytes, sizeof(buf)) != 0)
		error("unrecognized attribute header");
	return 1;
}

/* Store symbol (as 0-terminated string) in one of 'symbols_buf' elements.
   Return the "key" of this element i.e. its 0-based index in 'symbols_buf'. */
static unsigned int RDS_read_symbol(SEXP filexp, CharAEAE *symbols_buf,
		int indent)
{
	const char *errmsg;
	const unsigned char NEW_SYMBOL_bytes[4] = {0x00, 0x00, 0x00, 0x01};
	unsigned char buf[4];
	unsigned int key;
	CharAE *namebuf;

	PRINTIFVERBOSE1("start reading symbol");
	errmsg = RDS_read_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, NEW_SYMBOL_bytes, sizeof(buf)) == 0) {
		/* New symbol. */
		namebuf = new_CharAE(0);
		if (RDS_read_string(filexp, 0, namebuf))
			error("invalid symbol (NA)");
		CharAE_insert_at(namebuf, CharAE_get_nelt(namebuf), '\0');
		key = CharAEAE_get_nelt(symbols_buf);
		CharAEAE_insert_at(symbols_buf, key, namebuf);
	} else {
		/* Known symbol (i.e. already in 'symbols_buf'). */
		key = (((unsigned int) buf[0]) << 16) |
		      (((unsigned int) buf[1]) << 8) |
		       ((unsigned int) buf[2]);
		if (buf[3] != 0xff || key == 0)
			error("unsupported symbol specifier");
		key--;
	}
	PRINTIFVERBOSE2("done reading symbol [%s]",
			symbols_buf->elts[key]->elts);
	return key;
}

/* Always parse the full attributes.
   In mode 0: Load and set the attributes on 'object', return R_NilValue.
   In mode 1: (parse-only mode) Don't load anything, don't set anything on
              'object' (which should be R_NilValue), and return R_NilValue;
   In mode 2: Load the attributes and do NOT set them on 'object' ('object'
	      is ignored), but dump them in the 'attribs_dump' environment. */
static void RDS_read_attribs(SEXP filexp, int mode,
		SEXP object, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *symbols_buf, int indent)
{
	unsigned int key;
	SEXP attrval;
	const char *symbol;

	PRINTIFVERBOSE1("start reading object attributes");
	while (RDS_read_attrib_separator(filexp)) {
		key = RDS_read_symbol(filexp, symbols_buf, indent + 1);
		attrval = RDS_read_object(filexp, mode == 1, R_NilValue,
					  string_buf, symbols_buf,
					  indent + 1);
		if (mode == 1)
			continue;
		PROTECT(attrval);
		symbol = symbols_buf->elts[key]->elts;
		if (mode == 0)
			setAttrib(object, install(symbol), attrval);
		else // mode 2
			defineVar(install(symbol), attrval, attribs_dump);
		UNPROTECT(1);
	}
	PRINTIFVERBOSE1("done reading object attributes");
	return;
}

static SEXP RDS_read_object(SEXP filexp, int mode, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *symbols_buf, int indent)
{
	const char *errmsg;
	unsigned char obj_header[4];
	int has_attribs;
	SEXPTYPE type;
	SEXP ans;

	PRINTIFVERBOSE1("start reading object header");
	errmsg = RDS_read_bytes(filexp, sizeof(obj_header), 0, obj_header);
	if (errmsg != NULL)
		error(errmsg);
	if (obj_header[0] != 0 || obj_header[1] != 0)
		error("unsupported RDS file");
	PRINTIFVERBOSE1("done reading object header");
	if (obj_header[2] == 0) {
		/* Object has no attributes. */
		if (mode == 3)
			return R_NilValue;  // early bail out
		has_attribs = 0;
	} else if (obj_header[2] == 0x02 || obj_header[2] == 0x03) {
		/* Object has attributes (code 0x03 seems to be specific
		   to factors). */
		if (mode == 3)
			mode = 2;
		has_attribs = 1;
	} else {
		error("unexpected 3rd byte in object header");
	}
	type = RDStype2Rtype(obj_header[3]);
	PRINTIFVERBOSE2("object type: %s", CHAR(type2str(type)));
	if (mode == 4)
		return get_typeof_and_length_as_list(filexp, type);
	if (type == NILSXP) {
		ans = R_NilValue;
	} else if (type == STRSXP) {
		ans = RDS_read_character_vector(filexp, mode != 0,
						string_buf, indent);
	} else if (IS_ATOMIC_TYPE(type)) {
		ans = RDS_read_atomic_vector(filexp, type, mode != 0,
					     indent);
	} else if (type == VECSXP) {
		ans = RDS_read_list(filexp, mode != 0,
				    string_buf, symbols_buf, indent);
	} else {
		error("RDS parser does not support type: %s",
		      CHAR(type2str(type)));
	}
	if (has_attribs) {
		if (!isNull(ans))
			PROTECT(ans);
		RDS_read_attribs(filexp, mode, ans, attribs_dump,
				 string_buf, symbols_buf, indent);
		if (!isNull(ans))
			UNPROTECT(1);
	}
	return ans;
}

static void RDS_read_file_header(SEXP filexp)
{
	const char *errmsg;
	const unsigned char RDS_header[14] = {0x58, 0x0a,
					      0x00, 0x00, 0x00, 0x02,
					      0x00, 0x03, 0x04, 0x02,
					      0x00, 0x02, 0x03, 0x00};
	unsigned char file_header[sizeof(RDS_header)];
	int indent;

	indent = 0;
	PRINTIFVERBOSE1("start reading file header");
	errmsg = RDS_read_bytes(filexp, sizeof(file_header), 0, file_header);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(file_header, RDS_header, sizeof(file_header)) != 0)
		error("does not look like an RDS file");
	PRINTIFVERBOSE1("done reading file header");
	return;
}


/****************************************************************************
 * RDS_read_file()
 *
 * --- .Call ENTRY POINT ---
 * Read/parse an RDS file. Only support a serialized atomic vector or a NULL
 * or a list (possibly nested) made of the formers. Support attributes (if
 * made of the formers).
 * Args:
 *   filexp: External pointer to a FILE pointer.
 *   mode:   Control what parts of the object to load. In modes 0, 1, 2 the
 *           full object gets parsed:
 *             mode 0: Load everything and return the full object.
 *             mode 1: (parse-only mode) Don't load anything and return
 *                     R_NilValue.
 *             mode 2: Load only the attributes and dump them in the
 *                     'attribs_dump' environment.
 *           Mode 3 is like mode 2 but with early bailout if the object header
 *           indicates that the object has no attributes. So in this mode the
 *           object gets fully parsed only if it has attributes. Otherwise
 *           only its header gets parsed.
 *           In mode 4 only the object header and length get parsed.
 *   attribs_dump: Environment used in modes 2 and 3 to dump the attributes.
 */
SEXP RDS_read_file(SEXP filexp, SEXP mode, SEXP attribs_dump)
{
	int mode0;
	CharAE *string_buf;
	CharAEAE *symbols_buf;

	RDS_read_file_header(filexp);
	mode0 = INTEGER(mode)[0];
	string_buf = new_CharAE(0);
	symbols_buf = new_CharAEAE(0, 0);
	return RDS_read_object(filexp, mode0, attribs_dump,
			       string_buf, symbols_buf, 1);
}


/****************************************************************************
 * RDS_extract_subvector()
 */

static SEXPTYPE extract_top_level_object_type(SEXP filexp)
{
	const char *errmsg;
	unsigned char obj_header[4];
	SEXPTYPE x_type;

	RDS_read_file_header(filexp);
	errmsg = RDS_read_bytes(filexp, sizeof(obj_header), 0, obj_header);
	if (errmsg != NULL)
		error(errmsg);
	x_type = RDStype2Rtype(obj_header[3]);
	if (!IS_ATOMIC_TYPE(x_type) || x_type == STRSXP)
		error("extracting elements from a serialized object of "
                      "type %s is not supported", CHAR(type2str(x_type)));
	return x_type;
}

static const char *get_pos(int pos_type, const void *pos,
		R_xlen_t i, long long int *pos_elt)
{
	int tmp0, is_na;
	double tmp1;
	long long int tmp2;
	static char errmsg_buf[80];

	switch (pos_type) {
	    case 0:  // 'pos' contains int values
		tmp0 = ((const int *) pos)[i];
		is_na = tmp0 == NA_INTEGER;
		*pos_elt = (long long int) tmp0;
		break;
	    case 1:  // 'pos' contains double values
		tmp1 = ((const double *) pos)[i];
		is_na = ISNAN(tmp1);
		*pos_elt = (long long int) tmp1;
		break;
	    case 2:  // 'pos' contains long long int values
		tmp2 = ((const long long int *) pos)[i];
		is_na = tmp2 == NA_LLINT;
		*pos_elt = tmp2;
		break;
	    default:
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "XVector internal error in get_pos(): "
			 "unsupported 'pos' type");
		return errmsg_buf;
	}
	if (is_na) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'pos' cannot contain NAs");
		return errmsg_buf;
	}
	return NULL;
}

static void RDS_read_atom_at_offset(SEXP filexp,
		long long int offset, SEXP ans, R_xlen_t i)
{
	size_t n;
	const char *errmsg;

	if (offset < 0)
		error("positions of elements to extract must be sorted");
	n = offset * type2atomsize(TYPEOF(ans));
	errmsg = RDS_read_bytes(filexp, n, 1, NULL);
	if (errmsg != NULL)
		error(errmsg);
	switch (TYPEOF(ans)) {
	    case LGLSXP:
		errmsg = RDS_read_ints(filexp, 1, 0, LOGICAL(ans) + i);
		break;
	    case INTSXP:
		errmsg = RDS_read_ints(filexp, 1, 0, INTEGER(ans) + i);
		break;
	    case REALSXP:
		errmsg = RDS_read_doubles(filexp, 1, 0, REAL(ans) + i);
		break;
	    case CPLXSXP:
		errmsg = RDS_read_doubles(filexp, 2, 0,
					  (double *) (COMPLEX(ans) + i));
		break;
	    case RAWSXP:
		errmsg = RDS_read_bytes(filexp, 1, 0, RAW(ans) + i);
		break;
	    default:
		error("XVector internal error in RDS_read_atom_at_offset(): "
		      "unexpected type: %s", CHAR(type2str(TYPEOF(ans))));
	}
	if (errmsg != NULL)
		error(errmsg);
	return;
}

static const char *RDS_read_atoms_at_positions(SEXP filexp,
		R_xlen_t x_len, int pos_type, const void *pos, SEXP ans)
{
	long long int pos_elt, prev_pos_elt, offset;
	R_xlen_t i;
	const char *errmsg;
	static char errmsg_buf[40];

	prev_pos_elt = 0;
	for (i = 0; i < XLENGTH(ans); i++) {
		errmsg = get_pos(pos_type, pos, i, &pos_elt);
		if (errmsg != NULL)
			return errmsg;
		if (pos_elt < 1 || pos_elt > x_len) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "'pos' contains invalid positions");
			return errmsg_buf;
		}
		offset = pos_elt - prev_pos_elt - 1;
		RDS_read_atom_at_offset(filexp, offset, ans, i);
		prev_pos_elt = pos_elt;
	}
	return NULL;
}

/* --- .Call ENTRY POINT ---
 * Random access to the elements of a serialized atomic vector.
 * Character vectors not supported.
 * Args:
 *   filexp: External pointer to a FILE pointer.
 *   pos:    An integer, double, or LLint vector containing valid element
 *           positions in the serialized vector. The positions must be 1-based.
 *           So no NAs and all values must be >= 1 and <= vector length.
 *           In addition 'pos' must be sorted.
 */
SEXP RDS_extract_subvector(SEXP filexp, SEXP pos)
{
	SEXPTYPE x_type;
	R_xlen_t x_len, pos_len;
	int pos_type;
	const void *pos_dataptr;
	SEXP ans;
	const char *errmsg;

	/* Get type and length of serialized atomic vector. */
	x_type = extract_top_level_object_type(filexp);
	x_len = RDS_read_vector_length(filexp);

	/* Get 'pos' length and pointer to data. */
	if (IS_INTEGER(pos)) {
		pos_type = 0;
		pos_len = XLENGTH(pos);
		pos_dataptr = INTEGER(pos);
	} else if (IS_NUMERIC(pos)) {
		pos_type = 1;
		pos_len = XLENGTH(pos);
		pos_dataptr = REAL(pos);
	} else if (is_LLint(pos)) {
		pos_type = 2;
		pos_len = get_LLint_length(pos);
		pos_dataptr = get_LLint_dataptr(pos);
	} else {
		error("'pos' must be an integer, double, or LLint vector");
	}

	ans = PROTECT(allocVector(x_type, pos_len));
	errmsg = RDS_read_atoms_at_positions(filexp, x_len,
					     pos_type, pos_dataptr, ans);
	UNPROTECT(1);
	if (errmsg != NULL)
		error(errmsg);
	return ans;
}


/****************************************************************************
 * RDS_extract_subarray()
 */

/* --- .Call ENTRY POINT ---
 * Random access to the elements of a serialized array.
 * Character arrays not supported.
 * Args:
 *   filexp: External pointer to a FILE pointer.
 *   dim:    The dimensions of the array. Typically extracted earlier with
 *           RDS_read_file(filexp, 3, attribs_dump).
 *   index:  A list of subscripts as positive integer vectors. One vector of
 *           subscripts per array dimension. Each subscript must be sorted.
 */
SEXP RDS_extract_subarray(SEXP filexp, SEXP dim, SEXP index)
{
	SEXPTYPE x_type;
	R_xlen_t x_len, dimprod;
	int ndim, i;
	SEXP subscript, ans;

	/* Get type and length of serialized array. */
	x_type = extract_top_level_object_type(filexp);
	x_len = RDS_read_vector_length(filexp);

	/* Check 'dim'. */
	if (!IS_INTEGER(dim))
		error("'dim' must be an integer vector");
	ndim = LENGTH(dim);
	dimprod = 1;
	for (i = 0; i < ndim; i++)
		dimprod *= INTEGER(dim)[i];
	if (dimprod > x_len)  // this is dangerous
		error("supplied 'dim' implies that serialized array "
		      "has more elements than it effectively has");
	if (dimprod < x_len)  // this is not
		warning("supplied 'dim' implies that serialized array "
		      "has less elements than it effectively has");

	/* Check 'index'. */
	if (!isVectorList(index))  // IS_LIST() is broken
		error("'index' must be a list");
	if (LENGTH(index) != ndim)
		error("'index' must have the same length as 'dim'");
	for (i = 0; i < ndim; i++) {
		subscript = VECTOR_ELT(index, i);
		if (!IS_INTEGER(subscript))
			error("all subscripts in list 'index' must be "
			      "integer vectors");
	}
	return R_NilValue;
}

