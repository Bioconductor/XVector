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

static const char *read_RDS_bytes(SEXP filexp, char *buf, int buf_size)
{
	int n, i;
	static char errmsg_buf[200];

	n = _filexp_read(filexp, buf, buf_size);
	//printf("%d/%d\t", n, buf_size);
	//for (i = 0; i < n; i++)
	//	printf("%02x ", buf[i]);
	//printf("\n");
	if (n == buf_size)
		return NULL;
	snprintf(errmsg_buf, sizeof(errmsg_buf),
		 "read error or unexpected end of file");
	return errmsg_buf;
}

static const char *read_RDS_bytes_in_CharAE(SEXP filexp, int n, CharAE *buf)
{
	if (n > buf->_buflength)
		CharAE_extend(buf, n);
	return read_RDS_bytes(filexp, buf->elts, n);
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
	errmsg = read_RDS_bytes(filexp, (char *) buf, buf_size);
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
	errmsg = read_RDS_bytes(filexp, (char *) buf, buf_size);
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

/* String encoding not supported */
static SEXP read_RDS_string(SEXP filexp, int attribs_only, CharAE *databuf)
{
	const char *errmsg;
	char buf[4];
	int ans_len;

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (buf[0] != 0 || buf[2] != 0 || buf[3] != 0x09)
		error("unsupported RDS file");
	if (buf[1] == 0) {
		char NA_STRING_bytes[4] = {0xff, 0xff, 0xff, 0xff};
		errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
		if (errmsg != NULL)
			error(errmsg);
		if (memcmp(buf, NA_STRING_bytes, sizeof(buf)) != 0)
			error("unsupported RDS file");
		return NA_STRING;
	}
	if (buf[1] != 0x04)
		error("unsupported string header");
	ans_len = read_RDS_vector_length(filexp);
	if (attribs_only) {
		_filexp_seek(filexp, ans_len, SEEK_CUR);
		return R_NilValue;
	}
	errmsg = read_RDS_bytes_in_CharAE(filexp, ans_len, databuf);
	if (errmsg != NULL)
		error(errmsg);
	return mkCharLen(databuf->elts, ans_len);
}

/* Return R_NilValue if attribs_only != 0. */
static SEXP read_RDS_character_vector(SEXP filexp, int attribs_only,
				      CharAE *databuf)
{
	int ans_len, i;
	SEXP ans, ans_elt;

	ans_len = read_RDS_vector_length(filexp);
	if (attribs_only) {
		for (i = 0; i < ans_len; i++)
			read_RDS_string(filexp, 1, databuf);
		return R_NilValue;
	}
	PROTECT(ans = NEW_CHARACTER(ans_len));
	for (i = 0; i < ans_len; i++) {
		PROTECT(ans_elt = read_RDS_string(filexp, 0, databuf));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* Return R_NilValue if attribs_only != 0. */
static SEXP read_RDS_atomic_vector(SEXP filexp, SEXPTYPE type,
				   int attribs_only)
{
	int ans_len;
	long long int offset;
	SEXP ans;
	const char *errmsg;

	ans_len = read_RDS_vector_length(filexp);
	offset = ans_len * type2atomsize(type);
	if (attribs_only) {
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

/* Return R_NilValue if attribs_only != 0. */
static SEXP read_RDS_list(SEXP filexp, int attribs_only, CharAE *databuf)
{
	SEXP ans;

	error("reading a list not ready yet");
	return ans;
}

/* If 'attribs_only' is 0, then set the attributes on 'object' and returns
   NULL. Otherwise, don't set the attributes on 'object' and return the
   attributes as a list. */
static SEXP read_RDS_attribs(SEXP filexp, SEXP object, int attribs_only)
{
	SEXP attribs;

	error("reading attributes not ready yet");
	return attribs;
}

static SEXP read_RDS_object(SEXP filexp, int attribs_only,
			    SEXP attrib_names_cache, CharAE *databuf)
{
	const char *errmsg;
	char header[4];
	int has_attribs;
	SEXPTYPE type;
	SEXP ans, attribs;

	errmsg = read_RDS_bytes(filexp, header, sizeof(header));
	if (errmsg != NULL)
		error(errmsg);
	if (header[0] != 0 || header[1] != 0)
		error("unsupported RDS file");
	if (header[2] == 0) {
		if (attribs_only)
			return R_NilValue;
		has_attribs = 0;
	} else if (header[2] == 0x02) {
		has_attribs = 1;
	} else {
		error("unexpected object header[2]");
	}
	type = header[3];
	if (type == NILSXP) {
		ans = R_NilValue;
	} else if (type == STRSXP) {
		ans = read_RDS_character_vector(filexp, attribs_only, databuf);
	} else if (IS_ATOMIC_TYPE(type)) {
		ans = read_RDS_atomic_vector(filexp, type, attribs_only);
	} else if (type == VECSXP) {
		ans = read_RDS_list(filexp, attribs_only, databuf);
	} else {
		error("RDS parser does not support type: %s",
		      CHAR(type2str(type)));
	}
	if (!isNull(ans))
		PROTECT(ans);
	if (has_attribs) {
		attribs = read_RDS_attribs(filexp, ans, attribs_only);
		if (attribs_only)
			return attribs;
	}
	if (!isNull(ans))
		UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP parse_RDS_file(SEXP filexp, SEXP attribs_only, SEXP attrib_names_cache)
{
	const char *errmsg;
	char buf[14],
	     RDS_header[14] = {0x58, 0x0a,
			       0x00, 0x00, 0x00, 0x02,
			       0x00, 0x03, 0x04, 0x02,
			       0x00, 0x02, 0x03, 0x00};
	CharAE *databuf;

	errmsg = read_RDS_bytes(filexp, buf, sizeof(buf));
	if (errmsg != NULL)
		error(errmsg);
	if (memcmp(buf, RDS_header, sizeof(buf)) != 0)
		error("does not look like an RDS file");
	databuf = new_CharAE(0);
	return read_RDS_object(filexp, LOGICAL(attribs_only)[0],
			       attrib_names_cache, databuf);
}

