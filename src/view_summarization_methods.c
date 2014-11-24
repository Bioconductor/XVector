#include "XVector.h"
#include "IRanges_interface.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)


/****************************************************************************
 * Low-level operations on Ints_holder (sequences of ints) and
 * Doubles_holder (sequences of doubles) structures.
 */

static Ints_holder get_view_from_Ints_holder(const Ints_holder *X,
		int view_start, int view_width)
{
	Ints_holder X_view;
	int view_offset, tmp;

	view_offset = view_start - 1;
	/* Trim the view if it's "out of limits". */
	if (view_offset < 0) {
		view_width += view_offset;
		view_offset = 0;
	}
	if (view_width > (tmp = X->length - view_offset))
		view_width = tmp;
	X_view.ptr = X->ptr + view_offset;
	X_view.length = view_width;
	return X_view;
}

static Doubles_holder get_view_from_Doubles_holder(const Doubles_holder *X,
		int view_start, int view_width)
{
	Doubles_holder X_view;
	int view_offset, tmp;

	view_offset = view_start - 1;
	/* Trim the view if it's "out of limits". */
	if (view_offset < 0) {
		view_width += view_offset;
		view_offset = 0;
	}
	if (view_width > (tmp = X->length - view_offset))
		view_width = tmp;
	X_view.ptr = X->ptr + view_offset;
	X_view.length = view_width;
	return X_view;
}

/*
 * Returns NA if 'X' is empty. Note that this differs from what
 * 'min(integer(0))' does: the latter returns 'Inf' (which is a double) and
 * issues a warning.
 * See C function imin() in the R source code (src/main/summary.c) for the
 * details.
 */
static int get_min_from_Ints_holder(const Ints_holder *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if (val == NA_INTEGER || x < val)
			val = x;
	}
	return val;
}

/*
 * Returns NA if 'X' contains NAs and/or NaNs and 'narm' is FALSE. Note that
 * this differs from what min() does on a standard double vector: the latter
 * will return NA if the input contains NAs, and NaN if it contains NaNs but
 * no NAs.
 * See C function rmin() in the R source code (src/main/summary.c) for the
 * details.
 */
static double get_min_from_Doubles_holder(const Doubles_holder *X, int narm)
{
	int xlen, i;
	double val, x;

	xlen = X->length;
	val = R_PosInf;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (ISNAN(x)) { /* NA or NaN */
			if (narm)
				continue;
			return NA_REAL;
		}
		if (val == R_PosInf || x < val)
			val = x;
	}
	return val;
}

/*
 * Returns NA if 'X' is empty. Note that this differs from what
 * 'max(integer(0))' does: the latter returns '-Inf' (which is a double) and
 * issues a warning.
 * See C function imax() in the R source code (src/main/summary.c) for the
 * details.
 */
static int get_max_from_Ints_holder(const Ints_holder *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if (val == NA_INTEGER || x > val)
			val = x;
	}
	return val;
}

/*
 * Returns NA if 'X' contains NAs and/or NaNs and 'narm' is FALSE. Note that
 * this differs from what max() does on a standard double vector: the latter
 * will return NA if the input contains NAs, and NaN if it contains NaNs but
 * no NAs.
 * See C function rmax() in the R source code (src/main/summary.c) for the
 * details.
 */
static double get_max_from_Doubles_holder(const Doubles_holder *X, int narm)
{
	int xlen, i;
	double val, x;

	xlen = X->length;
	val = R_NegInf;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (ISNAN(x)) { /* NA or NaN */
			if (narm)
				continue;
			return NA_REAL;
		}
		if (val == R_NegInf || x > val)
			val = x;
	}
	return val;
}

static int get_sum_from_Ints_holder(const Ints_holder *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = 0;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if ((x > 0 && INT_MAX - x < val)
		 || (x < 0 && R_INT_MIN - x > val)) {
			warning("Integer overflow");
			return NA_INTEGER;
		}
		val += x;
	}
	return val;
}

/*
 * Mimics exactly what sum() does on a standard double vector.
 * See C function rsum() in the R source code (src/main/summary.c) for the
 * details.
 */
static double get_sum_from_Doubles_holder(const Doubles_holder *X, int narm)
{
	int xlen, i;
	double val, x;

	xlen = X->length;
	val = 0.00;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (narm && ISNAN(x)) /* expensive ISNAN(x) in 2nd place */
			continue;
		val += x;
	}
	return val;
}

/* TODO: Compare the code below with what which.min() does on a standard
 * integer vector. */
static int get_which_min_from_Ints_holder(const Ints_holder *X, int narm)
{
	int xlen, cur_min, which_min, i, x;

	xlen = X->length;
	which_min = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_min == NA_INTEGER || x < cur_min) {
			cur_min = x;
			which_min = i + 1;
		}
	}
	return which_min;
}

/* The code below does something *close* but not identical to what which.min()
 * does on a standard double vector.
 * TODO: See do_first_min() C function in the R source code
 * (src/main/summary.c) for what standard which.min() does and maybe adjust
 * the code below. */
static int get_which_min_from_Doubles_holder(const Doubles_holder *X, int narm)
{
	int xlen, i, which_min;
	double cur_min, x;

	xlen = X->length;
	which_min = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (ISNAN(x)) { /* NA or NaN */
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_min == NA_INTEGER || x < cur_min) {
			cur_min = x;
			which_min = i + 1;
		}
	}
	return which_min;
}

/* TODO: Compare the code below with what which.max() does on a standard
 * integer vector. */
static int get_which_max_from_Ints_holder(const Ints_holder *X, int narm)
{
	int xlen, cur_max, which_max, i, x;

	xlen = X->length;
	which_max = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_max == NA_INTEGER || x > cur_max) {
			cur_max = x;
			which_max = i + 1;
		}
	}
	return which_max;
}

/* The code below does something *close* but not identical to what which.max()
 * does on a standard double vector.
 * TODO: See do_first_min() C function in the R source code
 * (src/main/summary.c) for what standard which.max() does and maybe adjust
 * the code below. */
static int get_which_max_from_Doubles_holder(const Doubles_holder *X, int narm)
{
	int xlen, i, which_max;
	double cur_max, x;

	xlen = X->length;
	which_max = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->ptr[i];
		if (ISNAN(x)) { /* NA or NaN */
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_max == NA_INTEGER || x > cur_max) {
			cur_max = x;
			which_max = i + 1;
		}
	}
	return which_max;
}


/****************************************************************************
 * XIntegerViews_summary1() and XDoubleViews_summary1() .Call entry points
 * for fast view summary methods: viewMins, viewMaxs, viewSums.
 */

SEXP XIntegerViews_summary1(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	Ints_holder S, S_view;
	IRanges_holder ranges_holder;
	const char *funname;
	int (*fun)(const Ints_holder *, int);
	int ans_len, v, view_start, view_width, *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _hold_XInteger(subject);
	ranges_holder = hold_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewMins") == 0)
		fun = &get_min_from_Ints_holder;
	else if (strcmp(funname, "viewMaxs") == 0)
		fun = &get_max_from_Ints_holder;
	else if (strcmp(funname, "viewSums") == 0)
		fun = &get_sum_from_Ints_holder;
	else
		error("XVector internal error in XIntegerViews_summary1(): "
		      "invalid method \"%s\"", funname);
	ans_len = get_length_from_IRanges_holder(&ranges_holder);
	PROTECT(ans = NEW_INTEGER(ans_len));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_len; v++, ans_elt++) {
		view_start = get_start_elt_from_IRanges_holder(&ranges_holder, v);
		view_width = get_width_elt_from_IRanges_holder(&ranges_holder, v);
		S_view = get_view_from_Ints_holder(&S, view_start, view_width);
		*ans_elt = fun(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}

SEXP XDoubleViews_summary1(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	Doubles_holder S, S_view;
	IRanges_holder ranges_holder;
	const char *funname;
	double (*fun)(const Doubles_holder *, int);
	int ans_len, v, view_start, view_width;
	double *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _hold_XDouble(subject);
	ranges_holder = hold_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewMins") == 0)
		fun = &get_min_from_Doubles_holder;
	else if (strcmp(funname, "viewMaxs") == 0)
		fun = &get_max_from_Doubles_holder;
	else if (strcmp(funname, "viewSums") == 0)
		fun = &get_sum_from_Doubles_holder;
	else
		error("IRanges internal error in XDoubleViews_summary1(): "
		      "invalid method \"%s\"", funname);
	ans_len = get_length_from_IRanges_holder(&ranges_holder);
	PROTECT(ans = NEW_NUMERIC(ans_len));
	for (v = 0, ans_elt = REAL(ans); v < ans_len; v++, ans_elt++) {
		view_start = get_start_elt_from_IRanges_holder(&ranges_holder, v);
		view_width = get_width_elt_from_IRanges_holder(&ranges_holder, v);
		S_view = get_view_from_Doubles_holder(&S, view_start, view_width);
		*ans_elt = fun(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}

/****************************************************************************
 * XIntegerViews_summary2() and XDoubleViews_summary2() .Call entry points
 * for fast view summary methods: viewWhichMins, viewWhichMaxs.
 */

SEXP XIntegerViews_summary2(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	Ints_holder S, S_view;
	IRanges_holder ranges_holder;
	const char *funname;
	int (*fun)(const Ints_holder *, int);
	int ans_len, v, view_start, view_width, *ans_elt, which_min;

	subject = GET_SLOT(x, install("subject"));
	S = _hold_XInteger(subject);
	ranges_holder = hold_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewWhichMins") == 0)
		fun = &get_which_min_from_Ints_holder;
	else if (strcmp(funname, "viewWhichMaxs") == 0)
		fun = &get_which_max_from_Ints_holder;
	else
		error("XVector internal error in XIntegerViews_summary2(): "
		      "invalid method \"%s\"", funname);
	ans_len = get_length_from_IRanges_holder(&ranges_holder);
	PROTECT(ans = NEW_INTEGER(ans_len));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_len; v++, ans_elt++) {
		view_start = get_start_elt_from_IRanges_holder(&ranges_holder, v);
		view_width = get_width_elt_from_IRanges_holder(&ranges_holder, v);
		S_view = get_view_from_Ints_holder(&S, view_start, view_width);
		which_min = fun(&S_view, LOGICAL(na_rm)[0]);
		if (which_min == NA_INTEGER)
			*ans_elt = which_min;
		else
			*ans_elt = S_view.ptr - S.ptr + which_min;
	}
	UNPROTECT(1);
	return ans;
}

SEXP XDoubleViews_summary2(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	Doubles_holder S, S_view;
	IRanges_holder ranges_holder;
	const char *funname;
	int (*fun)(const Doubles_holder *, int);
	int ans_len, v, view_start, view_width, *ans_elt, which_min;

	subject = GET_SLOT(x, install("subject"));
	S = _hold_XDouble(subject);
	ranges_holder = hold_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewWhichMins") == 0)
		fun = &get_which_min_from_Doubles_holder;
	else if (strcmp(funname, "viewWhichMaxs") == 0)
		fun = &get_which_max_from_Doubles_holder;
	else
		error("IRanges internal error in XDoubleViews_summary2(): "
		      "invalid method \"%s\"", funname);
	ans_len = get_length_from_IRanges_holder(&ranges_holder);
	PROTECT(ans = NEW_INTEGER(ans_len));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_len; v++, ans_elt++) {
		view_start = get_start_elt_from_IRanges_holder(&ranges_holder, v);
		view_width = get_width_elt_from_IRanges_holder(&ranges_holder, v);
		S_view = get_view_from_Doubles_holder(&S, view_start, view_width);
		which_min = fun(&S_view, LOGICAL(na_rm)[0]);
		if (which_min == NA_INTEGER)
			*ans_elt = which_min;
		else
			*ans_elt = S_view.ptr - S.ptr + which_min;
	}
	UNPROTECT(1);
	return ans;
}

