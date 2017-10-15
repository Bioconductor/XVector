/****************************************************************************
 ****************************************************************************
 *                      A simple and fast RDS parser                        *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************
 ****************************************************************************/
#include "XVector.h"
#include "IRanges_interface.h"
#include "S4Vectors_interface.h"

#include <limits.h>  /* for INT_MAX */

#define	IS_ATOMIC_TYPE(type) \
	((type) == LGLSXP  || (type) == INTSXP || (type) == REALSXP || \
	 (type) == CPLXSXP || (type) == RAWSXP || (type) == STRSXP)

/* For some reason I don't understand, I can't use the DATAPTR() macro
   defined in Rinternals.h */
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
	error("XVector internal error: "
	      "atom size undefined for type %s", CHAR(type2str(type)));
}

static const char *read_RDS_bytes(SEXP filexp,
		unsigned char *buf, int buf_size)
{
	int n, i;
	static char errmsg_buf[100];

	n = _filexp_read(filexp, (char *) buf, buf_size);
	if (n == buf_size)
		return NULL;
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "read error or unexpected end of file");
	return errmsg_buf;
}

static void read_RDS_bytes_in_CharAE(SEXP filexp, int n, CharAE *buf)
{
	const char *errmsg;

	if (n > buf->_buflength)
		CharAE_extend(buf, n);
	errmsg = read_RDS_bytes(filexp, (unsigned char *) buf->elts, n);
	if (errmsg != NULL)
		error(errmsg);
	CharAE_set_nelt(buf, n);
	return;
}

static void swap_4_bytes(char *bytes)
{
	unsigned int *tmp;

	tmp = (unsigned int *) bytes;
	*tmp = (*tmp << 24) |
	       ((*tmp & 0xff00) << 8) |
	       ((*tmp & 0xff0000) >> 8) |
	       (*tmp >> 24);
	return;
}

static void swap_8_bytes(char *bytes)
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

static const char *read_RDS_ints(SEXP filexp, int *buf, int buf_len)
{
	int buf_size, i;
	const char *errmsg;

	buf_size = buf_len * sizeof(int);
	errmsg = read_RDS_bytes(filexp, (unsigned char *) buf, buf_size);
	if (errmsg != NULL)
		return errmsg;
	/* FIXME: Don't swap bytes if platform is big endian */
	for (i = 0; i < buf_len; i++)
		swap_4_bytes((char *) (buf + i));
	return NULL;	
}

static const char *read_RDS_doubles(SEXP filexp, double *buf, int buf_len)
{
	int buf_size, i;
	const char *errmsg;

	buf_size = buf_len * sizeof(double);
	errmsg = read_RDS_bytes(filexp, (unsigned char *) buf, buf_size);
	if (errmsg != NULL)
		return errmsg;
	/* FIXME: Don't swap bytes if platform is big endian */
	for (i = 0; i < buf_len; i++)
		swap_8_bytes((char *) (buf + i));
	return NULL;	
}

/* Long vectors NOT supported yet! */
static int read_RDS_vector_length(SEXP filexp)
{
	const char *errmsg;
	int vector_length;

	errmsg = read_RDS_ints(filexp, &vector_length, 1);
	if (errmsg != NULL)
		error(errmsg);
	return vector_length;
}

/* String encoding not supported. */
static int read_RDS_string(SEXP filexp, int parse_only, CharAE *stringbuf)
{
	const char *errmsg;
	unsigned char buf[4], NA_STRING_bytes[4] = {0xff, 0xff, 0xff, 0xff};
	int ans_len;

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (buf[0] != 0 || buf[2] != 0 || buf[3] != 0x09)
		error("unsupported RDS file");
	if (buf[1] == 0) {
		errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
		if (errmsg != NULL)
			error(errmsg);
		if (memcmp(buf, NA_STRING_bytes, sizeof(buf)) != 0)
			error("unsupported RDS file");
		return 1;
	}
	if (buf[1] != 0x04)
		error("unsupported string header");
	ans_len = read_RDS_vector_length(filexp);
	if (parse_only)
		_filexp_seek(filexp, ans_len, SEEK_CUR);
	else
		read_RDS_bytes_in_CharAE(filexp, ans_len, stringbuf);
	return 0;
}

/* Return R_NilValue if parse_only != 0. */
static SEXP read_RDS_character_vector(SEXP filexp, int parse_only,
		CharAE *stringbuf)
{
	int ans_len, i;
	SEXP ans, ans_elt;

	ans_len = read_RDS_vector_length(filexp);
	if (parse_only) {
		for (i = 0; i < ans_len; i++)
			read_RDS_string(filexp, 1, stringbuf);
		return R_NilValue;
	}
	PROTECT(ans = NEW_CHARACTER(ans_len));
	for (i = 0; i < ans_len; i++) {
		if (read_RDS_string(filexp, 0, stringbuf)) {
			SET_STRING_ELT(ans, i, NA_STRING);
		} else {
			PROTECT(ans_elt = new_CHARSXP_from_CharAE(stringbuf));
			SET_STRING_ELT(ans, i, ans_elt);
			UNPROTECT(1);
		}
	}
	UNPROTECT(1);
	return ans;
}

/* Return R_NilValue if parse_only != 0. */
static SEXP read_RDS_atomic_vector(SEXP filexp, SEXPTYPE type, int parse_only)
{
	int ans_len;
	long long int offset;
	SEXP ans;
	const char *errmsg;

	ans_len = read_RDS_vector_length(filexp);
	offset = ans_len * type2atomsize(type);
	if (parse_only) {
		_filexp_seek(filexp, offset, SEEK_CUR);
		return R_NilValue;
	}
	PROTECT(ans = allocVector(type, ans_len));
	switch (type) {
	    case LGLSXP:
	    case INTSXP:
		errmsg = read_RDS_ints(filexp, dataptr(ans), ans_len);
		break;
	    case REALSXP:
		errmsg = read_RDS_doubles(filexp, dataptr(ans), ans_len);
		break;
	    case CPLXSXP:
		errmsg = read_RDS_doubles(filexp, dataptr(ans), ans_len * 2);
		break;
	    case RAWSXP:
		errmsg = read_RDS_bytes(filexp, dataptr(ans), ans_len);
		break;
	    default:
		error("XVector internal error in read_RDS_atomic_vector(): "
		      "unexpected type: %s", CHAR(type2str(type)));
	}
	if (errmsg != NULL)
		error(errmsg);
	UNPROTECT(1);
	return ans;
}

static SEXP read_RDS_object(SEXP filexp, int mode,
		CharAE *stringbuf, CharAEAE *attrnamesbuf);

/* Return R_NilValue if parse_only != 0. */
static SEXP read_RDS_list(SEXP filexp, int parse_only,
		CharAE *stringbuf, CharAEAE *attrnamesbuf)
{
	int ans_len, i;
	SEXP ans, ans_elt;

	ans_len = read_RDS_vector_length(filexp);
	PROTECT(ans = NEW_LIST(ans_len));
	for (i = 0; i < ans_len; i++) {
		PROTECT(ans_elt = read_RDS_object(filexp, parse_only,
						  stringbuf, attrnamesbuf));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

static int read_RDS_attrib_separator(SEXP filexp)
{
	const char *errmsg;
	unsigned char buf[4], EOA_bytes[4] = {0x00, 0x00, 0x00, 0xfe},
			      ATTRIB_sep[4] = {0x00, 0x00, 0x04, 0x02};

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, EOA_bytes, sizeof(buf)) == 0)
		return 0;
	if (memcmp(buf, ATTRIB_sep, sizeof(buf)) != 0)
		error("unrecognized attribute header");
	return 1;
}

/* If parse_only is 0: store the attrib name (as a 0-terminated string) in
   one of 'attrnamesbuf' elements. Return the element key i.e. its 0-based
   index in 'attrnamesbuf'.
   If parse_only != 0: don't store anything and return a meaningless key. */
static unsigned int read_RDS_attrname(SEXP filexp, int parse_only,
		CharAEAE *attrnamesbuf)
{
	const char *errmsg;
	unsigned char buf[4], NEWATTRIB_bytes[4] = {0x00, 0x00, 0x00, 0x01};
	unsigned int key;
	CharAE *namebuf;

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, NEWATTRIB_bytes, sizeof(buf)) != 0) {
		/* Known attrib name (i.e. already in 'attrnamesbuf'). */
		key = (((unsigned int) buf[0]) << 16) |
		      (((unsigned int) buf[1]) << 8) |
		       ((unsigned int) buf[2]);
		if (buf[3] != 0xff || key == 0)
			error("unsupported attrib name specifier");
		return key - 1;
	}
	/* New attrib name. */
	namebuf = new_CharAE(0);
	if (read_RDS_string(filexp, parse_only, namebuf))
		error("invalid attrib name (NA)");
	if (parse_only)
		return 0;
	CharAE_insert_at(namebuf, CharAE_get_nelt(namebuf), '\0');
	key = CharAEAE_get_nelt(attrnamesbuf);
	CharAEAE_insert_at(attrnamesbuf, key, namebuf);
	return key;
}

/* Always parses the full attributes.
   In mode 0: Load and set the attributes on 'object', return R_NilValue.
   In mode 1: (parse-only mode) Don't load anything, don't set anything on
              'object' (which should be R_NilValue), return R_NilValue;
   In mode 2: Load the attributes but do NOT set them on 'object', return
              them as a list. */
static SEXP read_RDS_attribs(SEXP filexp, SEXP object, int mode,
		CharAE *stringbuf, CharAEAE *attrnamesbuf)
{
	unsigned int key;
	SEXP attribs, attrval;
	const char *attrname;

	while (read_RDS_attrib_separator(filexp)) {
		key = read_RDS_attrname(filexp, mode == 1, attrnamesbuf);
		attrval = read_RDS_object(filexp, mode == 1,
					  stringbuf, attrnamesbuf);
		if (mode == 1)
			continue;
		attrname = attrnamesbuf->elts[key]->elts;
		if (mode == 0) {
			setAttrib(object, install(attrname), attrval);
			continue;
		}
		error("mode == 2 not supported yet");
	}
	return R_NilValue;
}

/* In modes 0-2, read_RDS_object() parses the full object but 'mode' controls
   what parts of the object to load and return as an SEXP:
     mode 0: Load everything and return the full object.
     mode 1: (parse-only mode) Don't load anything and return R_NilValue.
     mode 2: Load only the attributes and return them as a list (or a NULL
             if the object has no attributes).
   Mode 3 is like mode 2 but with early bailout if the header of the object
   indicates that it has not attributes. This is the only mode where the
   object is not guaranteed to be fully parsed (only its header gets parsed
   if the object has no attributes). */
static SEXP read_RDS_object(SEXP filexp, int mode,
		CharAE *stringbuf, CharAEAE *attrnamesbuf)
{
	const char *errmsg;
	unsigned char header[4], type;
	int has_attribs;
	SEXP ans, attribs;

	errmsg = read_RDS_bytes(filexp, header, sizeof(header));
	if (errmsg != NULL)
		error(errmsg);
	if (header[0] != 0 || header[1] != 0)
		error("unsupported RDS file");
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
	type = header[3];
	if (type == 0xfe) {  /* NULL type is 0xfe in RDS, not NILSXP */
		ans = R_NilValue;
	} else if (type == STRSXP) {
		ans = read_RDS_character_vector(filexp, mode != 0, stringbuf);
	} else if (IS_ATOMIC_TYPE(type)) {
		ans = read_RDS_atomic_vector(filexp, type, mode != 0);
	} else if (type == VECSXP) {
		ans = read_RDS_list(filexp, mode != 0,
				    stringbuf, attrnamesbuf);
	} else {
		error("RDS parser does not support type: %s",
		      CHAR(type2str(type)));
	}
	if (has_attribs) {
		if (!isNull(ans))
			PROTECT(ans);
		attribs = read_RDS_attribs(filexp, ans, mode,
					   stringbuf, attrnamesbuf);
		if (!isNull(ans))
			UNPROTECT(1);
		if (mode == 2)
			return attribs;
	}
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP read_RDS_file(SEXP filexp, SEXP mode)
{
	const char *errmsg;
	unsigned char buf[14], RDS_header[14] = {0x58, 0x0a,
						 0x00, 0x00, 0x00, 0x02,
						 0x00, 0x03, 0x04, 0x02,
						 0x00, 0x02, 0x03, 0x00};
	int mode0;
	CharAE *stringbuf;
	CharAEAE *attrnamesbuf;

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, RDS_header, sizeof(buf)) != 0)
		error("does not look like an RDS file");
	mode0 = INTEGER(mode)[0];
	stringbuf = new_CharAE(0);
	attrnamesbuf = new_CharAEAE(0, 0);
	return read_RDS_object(filexp, mode0, stringbuf, attrnamesbuf);
}

