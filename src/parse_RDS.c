/****************************************************************************
 ****************************************************************************
 *                      A simple and fast RDS parser                        *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************
 ****************************************************************************/

/*
  The current implementation assumes that:
    - sizeof(int) = 4
    - sizeof(double) = sizeof(long long int) = 8
    - platform is little endian
*/

#include "XVector.h"
#include "IRanges_interface.h"
#include "S4Vectors_interface.h"

#include <limits.h>  /* for INT_MAX */

static int verbose = 0;

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

static const char *read_RDS_bytes(SEXP filexp, size_t n, int parse_only,
		unsigned char *buf)
{
	int n2, n3;
	static char errmsg_buf[100];

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

static void read_RDS_chars(SEXP filexp, size_t n, int parse_only,
		CharAE *string_buf)
{
	const char *errmsg;

	if (!parse_only && n > string_buf->_buflength)
		CharAE_extend(string_buf, n);
	errmsg = read_RDS_bytes(filexp, n, parse_only,
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

static const char *read_RDS_ints(SEXP filexp, size_t n, int parse_only,
		int *buf)
{
	const char *errmsg;
	size_t i;

	/* Integer values are *always* 4 bytes in an RDS file, even if
	   sizeof(int) != 4 on the machine running this code! */
	errmsg = read_RDS_bytes(filexp, n * 4, parse_only,
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

static const char *read_RDS_doubles(SEXP filexp, size_t n, int parse_only,
		double *buf)
{
	const char *errmsg;
	size_t i;

	/* Double values are *always* 8 bytes in an RDS file, even if
	   sizeof(double) != 8 on the machine running this code! */
	errmsg = read_RDS_bytes(filexp, n * 8, parse_only,
				(unsigned char *) buf);
	if (errmsg != NULL)
		return errmsg;
	/* FIXME: Don't swap bytes if platform is big endian */
	if (!parse_only)
		for (i = 0; i < n; i++)
			swap_8_bytes((unsigned char *) (buf + i));
	return NULL;
}

static R_xlen_t read_RDS_vector_length(SEXP filexp)
{
	const char *errmsg;
	unsigned char buf[8], LONGLENGTH_bytes[4] = {0xff, 0xff, 0xff, 0xff};
	int *length;
	long long int *long_length;

	errmsg = read_RDS_bytes(filexp, 4, 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, LONGLENGTH_bytes, 4) != 0) {
		swap_4_bytes(buf);
		length = (int *) buf;
		return (R_xlen_t) *length;
	}
	errmsg = read_RDS_bytes(filexp, 8, 0, buf);
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

	length = type == NILSXP ? 0 : read_RDS_vector_length(filexp);

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
static int read_RDS_string(SEXP filexp, int parse_only, CharAE *string_buf)
{
	const char *errmsg;
	unsigned char buf[4], NA_STRING_bytes[4] = {0xff, 0xff, 0xff, 0xff};
	R_xlen_t ans_len;

	errmsg = read_RDS_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (buf[0] != 0 || buf[2] != 0 || buf[3] != 0x09)
		error("unsupported RDS file");
	if (buf[1] == 0) {
		errmsg = read_RDS_bytes(filexp, sizeof(buf), 0, buf);
		if (errmsg != NULL)
			error(errmsg);
		if (memcmp(buf, NA_STRING_bytes, sizeof(buf)) != 0)
			error("unsupported RDS file");
		return 1;
	}
	if (buf[1] != 0x04)
		error("unsupported string header");
	ans_len = read_RDS_vector_length(filexp);
	read_RDS_chars(filexp, (size_t) ans_len, parse_only, string_buf);
	return 0;
}

/* Return R_NilValue if parse_only != 0. */
static SEXP read_RDS_character_vector(SEXP filexp, int parse_only,
		CharAE *string_buf, int indent)
{
	R_xlen_t ans_len, i;
	int is_na;
	SEXP ans, ans_elt;

	PRINTIFVERBOSE1("start reading character vector");
	ans_len = read_RDS_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(NEW_CHARACTER(ans_len));
	for (i = 0; i < ans_len; i++) {
		is_na = read_RDS_string(filexp, parse_only, string_buf);
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
static SEXP read_RDS_atomic_vector(SEXP filexp, SEXPTYPE type,
		int parse_only, int indent)
{
	R_xlen_t ans_len;
	SEXP ans;
	const char *errmsg;

	PRINTIFVERBOSE2("start reading %s vector", CHAR(type2str(type)));
	ans_len = read_RDS_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(allocVector(type, ans_len));
	switch (type) {
	    case LGLSXP:
	    case INTSXP:
		errmsg = read_RDS_ints(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case REALSXP:
		errmsg = read_RDS_doubles(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case CPLXSXP:
		errmsg = read_RDS_doubles(filexp, (size_t) ans_len * 2,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    case RAWSXP:
		errmsg = read_RDS_bytes(filexp, (size_t) ans_len,
				parse_only, parse_only ? NULL : dataptr(ans));
		break;
	    default:
		error("XVector internal error in read_RDS_atomic_vector(): "
		      "unexpected type: %s", CHAR(type2str(type)));
	}
	if (errmsg != NULL)
		error(errmsg);
	if (!parse_only)
		UNPROTECT(1);
	PRINTIFVERBOSE2("done reading %s vector", CHAR(type2str(type)));
	return ans;
}

static SEXP read_RDS_object(SEXP filexp, int mode, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *attrnames_buf, int indent);

/* Return R_NilValue if parse_only != 0. */
static SEXP read_RDS_list(SEXP filexp, int parse_only,
		CharAE *string_buf, CharAEAE *attrnames_buf, int indent)
{
	R_xlen_t ans_len, i;
	SEXP ans, ans_elt;

	PRINTIFVERBOSE1("start reading list object");
	ans_len = read_RDS_vector_length(filexp);
	PRINTIFVERBOSE2("object length: %td", ans_len);
	ans = parse_only ? R_NilValue : PROTECT(NEW_LIST(ans_len));
	for (i = 0; i < ans_len; i++) {
		ans_elt = read_RDS_object(filexp, parse_only, R_NilValue,
					  string_buf, attrnames_buf,
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

static int read_RDS_attrib_separator(SEXP filexp)
{
	const char *errmsg;
	unsigned char buf[4], EOA_bytes[4] = {0x00, 0x00, 0x00, 0xfe},
			      ATTRIB_sep[4] = {0x00, 0x00, 0x04, 0x02};

	errmsg = read_RDS_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, EOA_bytes, sizeof(buf)) == 0)
		return 0;
	if (memcmp(buf, ATTRIB_sep, sizeof(buf)) != 0)
		error("unrecognized attribute header");
	return 1;
}

/* Store attrib name (as 0-terminated string) in one of 'attrnames_buf'
   elements. Return the "key" of this element i.e. its 0-based index in
   'attrnames_buf'. */
static unsigned int read_RDS_attrname(SEXP filexp, CharAEAE *attrnames_buf,
		int indent)
{
	const char *errmsg;
	unsigned char buf[4], NEWATTRIB_bytes[4] = {0x00, 0x00, 0x00, 0x01};
	unsigned int key;
	CharAE *namebuf;

	PRINTIFVERBOSE1("start reading attribute name");
	errmsg = read_RDS_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, NEWATTRIB_bytes, sizeof(buf)) == 0) {
		/* New attrib name. */
		namebuf = new_CharAE(0);
		if (read_RDS_string(filexp, 0, namebuf))
			error("invalid attrib name (NA)");
		CharAE_insert_at(namebuf, CharAE_get_nelt(namebuf), '\0');
		key = CharAEAE_get_nelt(attrnames_buf);
		CharAEAE_insert_at(attrnames_buf, key, namebuf);
	} else {
		/* Known attrib name (i.e. already in 'attrnames_buf'). */
		key = (((unsigned int) buf[0]) << 16) |
		      (((unsigned int) buf[1]) << 8) |
		       ((unsigned int) buf[2]);
		if (buf[3] != 0xff || key == 0)
			error("unsupported attrib name specifier");
		key--;
	}
	PRINTIFVERBOSE2("done reading attribute name [%s]",
			attrnames_buf->elts[key]->elts);
	return key;
}

/* Always parses the full attributes.
   In mode 0: Load and set the attributes on 'object', return R_NilValue.
   In mode 1: (parse-only mode) Don't load anything, don't set anything on
              'object' (which should be R_NilValue), and return R_NilValue;
   In mode 2: Load the attributes and do NOT set them on 'object' ('object'
	      is ignored), but dump them in the 'attribs_dump' environment. */
static void read_RDS_attribs(SEXP filexp, int mode,
		SEXP object, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *attrnames_buf, int indent)
{
	unsigned int key;
	SEXP attrval;
	const char *attrname;

	PRINTIFVERBOSE1("start reading object attributes");
	while (read_RDS_attrib_separator(filexp)) {
		key = read_RDS_attrname(filexp, attrnames_buf, indent + 1);
		attrval = read_RDS_object(filexp, mode == 1, R_NilValue,
					  string_buf, attrnames_buf,
					  indent + 1);
		if (mode == 1)
			continue;
		PROTECT(attrval);
		attrname = attrnames_buf->elts[key]->elts;
		if (mode == 0)
			setAttrib(object, install(attrname), attrval);
		else /* mode 2 */
			defineVar(install(attrname), attrval, attribs_dump);
		UNPROTECT(1);
	}
	PRINTIFVERBOSE1("done reading object attributes");
	return;
}

/* In modes 0-2, read_RDS_object() parses the full object but the specific
   mode controls what parts of the object to load and to return as an SEXP:
     mode 0: Load everything and return the full object.
     mode 1: (parse-only mode) Don't load anything and return R_NilValue.
     mode 2: Load only the attributes and dump them in the 'attribs_dump'
             environment.
   Mode 3 is like mode 2 but with early bailout if the object header indicates
   that the object has no attributes. So in this mode the object gets fully
   parsed only if it has attributes. Otherwise only its header gets parsed.
   In mode 4 only the object header and length get parsed.
 */
static SEXP read_RDS_object(SEXP filexp, int mode, SEXP attribs_dump,
		CharAE *string_buf, CharAEAE *attrnames_buf, int indent)
{
	const char *errmsg;
	unsigned char header[4];
	int has_attribs;
	SEXPTYPE type;
	SEXP ans;

	PRINTIFVERBOSE1("start reading object header");
	errmsg = read_RDS_bytes(filexp, sizeof(header), 0, header);
	if (errmsg != NULL)
		error(errmsg);
	if (header[0] != 0 || header[1] != 0)
		error("unsupported RDS file");
	PRINTIFVERBOSE1("done reading object header");
	if (header[2] == 0) {
		/* Object has no attributes. */
		if (mode == 3)
			return R_NilValue;  /* early bail out */
		has_attribs = 0;
	} else if (header[2] == 0x02) {
		/* Object has attributes. */
		if (mode == 3)
			mode = 2;
		has_attribs = 1;
	} else {
		error("unexpected 3rd byte in object header");
	}
	type = RDStype2Rtype(header[3]);
	PRINTIFVERBOSE2("object type: %s", CHAR(type2str(type)));
	if (mode == 4)
		return get_typeof_and_length_as_list(filexp, type);
	if (type == NILSXP) {
		ans = R_NilValue;
	} else if (type == STRSXP) {
		ans = read_RDS_character_vector(filexp, mode != 0,
						string_buf, indent);
	} else if (IS_ATOMIC_TYPE(type)) {
		ans = read_RDS_atomic_vector(filexp, type, mode != 0,
					     indent);
	} else if (type == VECSXP) {
		ans = read_RDS_list(filexp, mode != 0,
				    string_buf, attrnames_buf, indent);
	} else {
		error("RDS parser does not support type: %s",
		      CHAR(type2str(type)));
	}
	if (has_attribs) {
		if (!isNull(ans))
			PROTECT(ans);
		read_RDS_attribs(filexp, mode, ans, attribs_dump,
				 string_buf, attrnames_buf, indent);
		if (!isNull(ans))
			UNPROTECT(1);
	}
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP read_RDS_file(SEXP filexp, SEXP mode, SEXP attribs_dump)
{
	const char *errmsg;
	unsigned char buf[14], RDS_header[14] = {0x58, 0x0a,
						 0x00, 0x00, 0x00, 0x02,
						 0x00, 0x03, 0x04, 0x02,
						 0x00, 0x02, 0x03, 0x00};
	int indent, mode0;
	CharAE *string_buf;
	CharAEAE *attrnames_buf;

	indent = 0;
	PRINTIFVERBOSE1("start reading file header");
	errmsg = read_RDS_bytes(filexp, sizeof(buf), 0, buf);
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, RDS_header, sizeof(buf)) != 0)
		error("does not look like an RDS file");
	PRINTIFVERBOSE1("done reading file header");
	mode0 = INTEGER(mode)[0];
	string_buf = new_CharAE(0);
	attrnames_buf = new_CharAEAE(0, 0);
	return read_RDS_object(filexp, mode0, attribs_dump,
			       string_buf, attrnames_buf, indent + 1);
}

