/****************************************************************************
 ****************************************************************************
 *                           I/O low-level utils                            *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************
 ****************************************************************************/
#include "XVector.h"



/****************************************************************************
 *                              ZFile structs                               *
 ****************************************************************************/
#include <stdlib.h> /* for malloc(), free() */

#include <zlib.h>
//#ifndef _WIN32
//#include <bzlib.h>
//#endif

#define UNCOMPRESSED		0
#define GZ_TYPE			1
#define BZ2_TYPE		2 /* detected but not supported */
#define XZ_TYPE			3 /* detected but not supported */

typedef struct zfile {
	const char *path;
	const char *expath;
	const char *mode; /* "r", "w", or "a" */
	int ztype;
	int subtype;
	void *file;
} ZFile;

/****************************************************************************
 * Input functions.
 */

static void *iZFile_open(const char *expath, int ztype, int subtype)
{
	void *file;

	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		file = gzopen(expath, "r");
		break;
	    case BZ2_TYPE:
		error("cannot open file '%s'\n"
		      "  bzip2-compressed files are not supported", expath);
//#ifndef _WIN32
//		file = BZ2_bzopen(expath, "rb");
//#else
//		error("cannot open file '%s'\n"
//		      "  bzip2-compressed files are not supported "
//		      "on Windows, sorry!", expath);
//#endif
		break;
	    case XZ_TYPE:
		error("cannot open file '%s'\n"
		      "  LZMA-compressed files are not supported", expath);
//#ifndef _WIN32
		//requires #include <lzma.h>
		//opening/closing this type of file seems quite
		//complicated
//#else
//		error("cannot open file '%s'\n"
//		      "  LZMA-compressed files are not supported "
//		      "on Windows, sorry!", expath);
//#endif
		break;
	    default:
		error(INTERNAL_ERR_IN "iZFile_open(): "
		      "invalid ztype value %d", ztype);
	}
	return file;
}

static void iZFile_close(const ZFile *zfile)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		gzclose((gzFile) file);
		break;
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		BZ2_bzclose((BZFILE *) file);
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "iZFile_close(): "
		      "invalid ztype value %d", ztype);
	}
	return;
}

/*
  Here is how gzread() is declared in zlib.h:

    int gzread(gzFile file, voidp buf, unsigned len);

  And also, according to zlib.h:

    gzread returns the number of uncompressed bytes actually read, less
    than len for end of file, or -1 for error.

  But gzread returns an int and len is an unsigned int so can be > INT_MAX.
  So how can gzread return the number of uncompressed bytes actually read
  when len is INT_MAX? Sounds like poor interface design to me.
  So for iZFile_read(), we set the type of buf_size to int, not unsigned int.
*/
static int iZFile_read(const ZFile *zfile, char *buf, int buf_size)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		return gzread((gzFile) file, buf, (unsigned int) buf_size);
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "iZFile_read(): "
		      "invalid ztype value %d", ztype);
	}
	return 0;
}

/*
 * Similar to fgets()/gzgets(), except that it returns a code instead of
 * NULL/Z_NULL or a pointer to the buffer. The returned code can be:
 *    2: if reading stopped after an EOF or a newline,
 *    1: if reading stopped because buffer was full and no EOF or newline was
 *       read in,
 *    0: if end of file occurred while no character was read,
 *   -1: on read error.
 */
static int iZFile_gets(const ZFile *zfile,
		char *buf, int buf_size, int *EOL_in_buf)
{
	int max_buf_len, ztype;
	void *file;
	//int i;
	//char *buf_p;

	max_buf_len = buf_size - 1;
	buf[max_buf_len] = 'N'; // any non '\0' would do

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		if (gzgets((gzFile) file, buf, buf_size) == Z_NULL) {
			//if (ferror(file) != 0 || feof(file) == 0)
			//	return -1;
			return 0;
		}
		break;
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		for (i = 0, buf_p = buf; i < max_buf_len; i++, buf_p++) {
//			if (BZ2_bzread((BZFILE *) file, buf_p, 1) == 0) {
//				if (i == 0)
//					return 0;
//				break;
//			}
//			if (*buf_p == '\n') {
//				buf_p++;
//				break;
//			}
//		}
//		*buf_p = '\0';
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "iZFile_gets(): "
		      "invalid ztype value %d", ztype);
	}

	*EOL_in_buf = buf[max_buf_len] == 'N' || buf[max_buf_len - 1] == '\n';
	return *EOL_in_buf ? 2 : 1;
}

static void iZFile_seek(ZFile *zfile, long long int offset, int whence)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		gzseek((gzFile) file, (z_off_t) offset, whence);
		break;
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		error(INTERNAL_ERR_IN "iZFile_seek(): "
//		      "iZFile_seek() not supported on bz2-compressed files");
//		zfile->file = file;
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "iZFile_seek(): "
		      "invalid ztype value %d", ztype);
	}
	return;
}

static void iZFile_rewind(ZFile *zfile)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
	    case GZ_TYPE:
		gzrewind((gzFile) file);
		break;
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		/* No rewind in the bzip2 library. So we close and re-open
//		   the file. */
//		BZ2_bzclose((BZFILE *) file);
//		file = BZ2_bzopen(zfile->expath, "rb");
//		if (file == NULL)
//			error(INTERNAL_ERR_IN "iZFile_rewind(): "
//			      "cannot re-open file '%s'", zfile->expath);
//		zfile->file = file;
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "iZFile_rewind(): "
		      "invalid ztype value %d", ztype);
	}
	return;
}

/****************************************************************************
 * Output functions.
 */

static void *oZFile_open(const char *expath, const char *mode,
		int ztype, int subtype)
{
	void *file;

	switch (ztype) {
	    case UNCOMPRESSED:
		file = fopen(expath, mode);
		break;
	    case GZ_TYPE:
		file = gzopen(expath, mode);
		break;
	    case BZ2_TYPE:
		error("cannot open file '%s'\n"
		      "  bzip2-compressed files are not supported", expath);
//#ifndef _WIN32
//		file = BZ2_bzopen(expath, mode);
//#else
//		error("cannot open file '%s'\n"
//		      "  bzip2-compressed files are not supported "
//		      "on Windows, sorry!", expath);
//#endif
		break;
	    case XZ_TYPE:
		error("cannot open file '%s'\n"
		      "  LZMA-compressed files are not supported", expath);
//#ifndef _WIN32
		//requires #include <lzma.h>
		//opening/closing this type of file seems quite
		//complicated
//#else
//		error("cannot open file '%s'\n"
//		      "  LZMA-compressed files are not supported "
//		      "on Windows, sorry!", expath);
//#endif
		break;
	    default:
		error(INTERNAL_ERR_IN "oZFile_open(): "
		      "invalid ztype value %d", ztype);
	}
	return file;
}

static void oZFile_close(const ZFile *zfile)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
		fclose((FILE *) file);
		break;
	    case GZ_TYPE:
		gzclose((gzFile) file);
		break;
//#ifndef _WIN32
//	    case BZ2_TYPE:
//		BZ2_bzclose((BZFILE *) file);
//		break;
//#endif
	    default:
		error(INTERNAL_ERR_IN "oZFile_close(): "
		      "invalid ztype value %d", ztype);
	}
	return;
}

static int oZFile_puts(const ZFile *zfile, const char *s)
{
	int ztype, n;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
		if ((n = fputs(s, (FILE *) file)) >= 0)
			return n;
		break;
	    case GZ_TYPE:
		if ((n = gzputs((gzFile) file, s)) >= 0)
			return n;
		break;
	    default:
		error(INTERNAL_ERR_IN "oZFile_puts(): "
		      "invalid ztype value %d", ztype);
	}
	error("write error");
}

static void oZFile_putc(const ZFile *zfile, int c)
{
	int ztype;
	void *file;

	ztype = zfile->ztype;
	file = zfile->file;
	switch (ztype) {
	    case UNCOMPRESSED:
		if (fputc(c, (FILE *) file) != EOF)
			return;
		break;
	    case GZ_TYPE:
		if (gzputc((gzFile) file, c) != -1)
			return;
		break;
	    default:
		error(INTERNAL_ERR_IN "oZFile_putc(): "
		      "invalid ztype value %d", ztype);
	}
	error("write error");
}

/****************************************************************************
 * Initialization/close.
 */

/* Code taken from do_url() in R/src/main/connections.c */
static void detect_file_ztype(const char *expath, int *ztype, int *subtype)
{
	FILE *fp;
	char buf[7];
	int res;

	*ztype = UNCOMPRESSED;
	*subtype = 0;
	if ((fp = fopen(expath, "rb")) == NULL)
		return;
	memset(buf, 0, 7);
	res = fread(buf, 5, 1, fp);
	fclose(fp);
	if (res != 1)
		return;
	if (buf[0] == '\x1f' && buf[1] == '\x8b')
		*ztype = GZ_TYPE;
	else if (strncmp(buf, "BZh", 3) == 0)
		*ztype = BZ2_TYPE;
	else if (buf[0] == '\xFD' && strncmp(buf+1, "7zXZ", 4) == 0)
		*ztype = XZ_TYPE;
	else if ((buf[0] == '\xFF') && strncmp(buf+1, "LZMA", 4) == 0) {
		*ztype = XZ_TYPE;
		*subtype = 1;
	} else if (memcmp(buf, "]\0\0\200\0", 5) == 0) {
		*ztype = XZ_TYPE;
		*subtype = 1;
	}
	return;
}

static int compress2ztype(const char *compress)
{
	if (strcmp(compress, "no") == 0)
		return UNCOMPRESSED;
	if (strcmp(compress, "gzip") == 0)
		return GZ_TYPE;
	if (strcmp(compress, "bzip2") == 0)
		return BZ2_TYPE;
	if (strcmp(compress, "xz") == 0)
		return XZ_TYPE;
	error(INTERNAL_ERR_IN "compress2ztype(): "
	      "invalid type of compression: %s", compress);
}

static ZFile new_ZFile(const char *path, const char *expath,
		const char *mode, const char *compress, int level)
{
	ZFile zfile;
	int ztype, subtype = 0;
	void *file;

	if (strcmp(mode, "r") == 0) {
		/* Open file for reading. */
		detect_file_ztype(expath, &ztype, &subtype);
		file = iZFile_open(expath, ztype, subtype);
	} else {
		/* Open file for writing or appending. */
		ztype = compress2ztype(compress);
		file = oZFile_open(expath, mode, ztype, subtype);
	}
	if (file == NULL)
		error("cannot open file '%s'", expath);
	zfile.path = path;
	zfile.expath = expath;
	zfile.mode = mode;
	zfile.ztype = ztype;
	zfile.subtype = subtype;
	zfile.file = file;
	return zfile;
}

static void ZFile_close(const ZFile *zfile)
{
	const char *mode;

	mode = zfile->mode;
	if (strcmp(mode, "r") == 0) {
		iZFile_close(zfile);
	} else {
		oZFile_close(zfile);
	}
	return;
}



/****************************************************************************
 *            Low-level manipulation of "file external pointers"            *
 ****************************************************************************/

#define CHECK_USER_INTERRUPT(ncall) \
{ \
	static int i = 0; \
	if (i++ >= (ncall)) { \
		R_CheckUserInterrupt(); \
		i = 0; \
	} \
}

int _filexp_read(SEXP filexp, char *buf, int buf_size)
{
	CHECK_USER_INTERRUPT(2000);
	return iZFile_read(R_ExternalPtrAddr(filexp), buf, buf_size);
}

int _filexp_gets(SEXP filexp, char *buf, int buf_size, int *EOL_in_buf)
{
	CHECK_USER_INTERRUPT(2000);
	return iZFile_gets(R_ExternalPtrAddr(filexp),
			   buf, buf_size, EOL_in_buf);
}

void _filexp_seek(SEXP filexp, long long int offset, int whence)
{
	CHECK_USER_INTERRUPT(100);
	iZFile_seek(R_ExternalPtrAddr(filexp), offset, whence);
	return;
}

void _filexp_rewind(SEXP filexp)
{
	CHECK_USER_INTERRUPT(100);
	iZFile_rewind(R_ExternalPtrAddr(filexp));
	return;
}

int _filexp_puts(SEXP filexp, const char *s)
{
	CHECK_USER_INTERRUPT(2000);
	return oZFile_puts(R_ExternalPtrAddr(filexp), s);
}

void _filexp_putc(SEXP filexp, int c)
{
	CHECK_USER_INTERRUPT(100000);
	oZFile_putc(R_ExternalPtrAddr(filexp), c);
	return;
}

static SEXP new_filexp(SEXP filepath,
		const char *mode, const char *compress, int level)
{
	SEXP filepath0, ans, string;
	const char *expath;
	ZFile zfile, *extptraddr;

	if (!IS_CHARACTER(filepath) || LENGTH(filepath) != 1)
		error("'filepath' must be a single string");
	filepath0 = STRING_ELT(filepath, 0);
	if (filepath0 == NA_STRING)
		error("'filepath' is NA");
	expath = R_ExpandFileName(translateChar(filepath0));
	zfile = new_ZFile(CHAR(filepath0), expath, mode, compress, level);
	extptraddr = (ZFile *) malloc(sizeof(ZFile));
	if (extptraddr == NULL) {
		ZFile_close(&zfile);
		error(INTERNAL_ERR_IN "new_filexp(): malloc() failed");
	}
	*extptraddr = zfile;
	PROTECT(ans = R_MakeExternalPtr(extptraddr, R_NilValue, R_NilValue));
	PROTECT(string = mkString(expath));
	setAttrib(ans, install("expath"), string);
	UNPROTECT(2);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * Returns an external pointer.
 * From R:
 *   x <- .Call("new_input_filexp", "path/to/some/file", PACKAGE="XVector")
 *   reg.finalizer(x,
 *       function(e) .Call("finalize_filexp", e, PACKAGE="XVector"),
 *       onexit=TRUE)
 */
SEXP new_input_filexp(SEXP filepath)
{
	return new_filexp(filepath, "r", NULL, 0);
}

/* --- .Call ENTRY POINT --- */
SEXP rewind_filexp(SEXP filexp)
{
	_filexp_rewind(filexp);
	return R_NilValue;
}

/* --- .Call ENTRY POINT ---
 * Returns an external pointer.
 */
SEXP new_output_filexp(SEXP filepath, SEXP append,
		SEXP compress, SEXP compression_level)
{
	const char *mode, *compress0;
	int level;

	mode = LOGICAL(append)[0] ? "a" : "w";
	compress0 = CHAR(STRING_ELT(compress, 0));
	level = INTEGER(compression_level)[0];
	return new_filexp(filepath, mode, compress0, level);
}

/* --- .Call ENTRY POINT ---
 * Closes the file pointed to by 'filexp'.
 */
SEXP finalize_filexp(SEXP filexp)
{
	ZFile *zfile;

	zfile = R_ExternalPtrAddr(filexp);
	if (zfile != NULL) {
		ZFile_close(zfile);
		free(zfile);
		R_SetExternalPtrAddr(filexp, NULL);
	}
	return R_NilValue;
}



/****************************************************************************
 *                               Other stuff                                *
 ****************************************************************************/

/*
 * Doesn't actually delete anything but returns the length the 'buf' char
 * array would have after deletion of the LF ("\n") or CRLF ("\r\n") chars
 * located at its end.
 * If 'buf_len' is -1, then 'buf' must be a C string (i.e. null-terminated).
 */
int _delete_trailing_LF_or_CRLF(const char *buf, int buf_len)
{
	if (buf_len == -1)
		buf_len = strlen(buf);
	if (buf_len == 0)
		return 0;
	if (buf[--buf_len] != '\n')
		return ++buf_len;
	if (buf_len == 0)
		return 0;
	if (buf[--buf_len] != '\r')
		return ++buf_len;
	return buf_len;
}

