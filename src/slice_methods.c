#include "XVector.h"
#include "IRanges_interface.h"

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XInteger_slice(SEXP x, SEXP lower, SEXP upper)
{
	Ints_holder X;
	SEXP ans, start, width;
	int i, ans_length;
	const int *X_elt;
	int *start_elt, *width_elt, lower_elt, upper_elt, curr_elt, prev_elt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	X = _hold_XInteger(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, X_elt = X.ptr; i <= X.length; i++, X_elt++) {
		curr_elt = (*X_elt >= lower_elt) && (*X_elt <= upper_elt);
		if (curr_elt && !prev_elt)
			ans_length++;
		prev_elt = curr_elt;
	}

	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(width = NEW_INTEGER(ans_length));
	if (ans_length > 0) {
		start_elt = INTEGER(start) - 1;
		width_elt = INTEGER(width) - 1;
		prev_elt = 0;
		for (i = 1, X_elt = X.ptr; i <= X.length; i++, X_elt++) {
			curr_elt = (*X_elt >= lower_elt) && (*X_elt <= upper_elt);
			if (curr_elt) {
				if (prev_elt)
					*width_elt += 1;
				else {
					start_elt++;
					width_elt++;
					*start_elt = i;
					*width_elt = 1;
				}
			}
			prev_elt = curr_elt;
		}
	}
	PROTECT(ans = new_IRanges("IRanges", start, width, R_NilValue));
	UNPROTECT(3);
	return ans;
}

static int gt(double x, double y) {
	return x > y;
}

static int lt(double x, double y) {
	return x < y;
}

static int ge(double x, double y) {
	return x >= y;
}

static int le(double x, double y) {
	return x <= y;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XDouble_slice(SEXP x, SEXP lower, SEXP upper,
		SEXP include_lower, SEXP include_upper)
{
	Doubles_holder X;
	SEXP ans, start, width;
	int i, ans_length;
	const double *X_elt;
	int *start_elt, *width_elt, curr_elt, prev_elt;
	double lower_elt, upper_elt;
	int (*lower_fun)(double, double);
	int (*upper_fun)(double, double);

	lower_fun = LOGICAL(include_lower)[0] ? &ge : &gt;
	upper_fun = LOGICAL(include_upper)[0] ? &le : &lt;

	lower_elt = REAL(lower)[0];
	upper_elt = REAL(upper)[0];

	X = _hold_XDouble(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, X_elt = X.ptr; i <= X.length; i++, X_elt++) {
		curr_elt = lower_fun(*X_elt, lower_elt) && upper_fun(*X_elt, upper_elt);
		if (curr_elt && !prev_elt)
			ans_length++;
		prev_elt = curr_elt;
	}

	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(width = NEW_INTEGER(ans_length));
	if (ans_length > 0) {
		start_elt = INTEGER(start) - 1;
		width_elt = INTEGER(width) - 1;
		prev_elt = 0;
		for (i = 1, X_elt = X.ptr; i <= X.length; i++, X_elt++) {
			curr_elt = lower_fun(*X_elt, lower_elt) && upper_fun(*X_elt, upper_elt);
			if (curr_elt) {
				if (prev_elt)
					*width_elt += 1;
				else {
					start_elt++;
					width_elt++;
					*start_elt = i;
					*width_elt = 1;
				}
			}
			prev_elt = curr_elt;
		}
	}
	PROTECT(ans = new_IRanges("IRanges", start, width, R_NilValue));
	UNPROTECT(3);
	return ans;
}

