/****************************************************************************
 *                Low-level manipulation of XDouble objects                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "XVector.h"


Doubles_holder _hold_XDouble(SEXP x)
{
	Doubles_holder x_holder;
	SEXP tag;
	int offset;

	tag = _get_XVector_tag(x);
	offset = _get_XVector_offset(x);
	x_holder.ptr = (const double *) (REAL(tag) + offset);
	x_holder.length = _get_XVector_length(x);
	return x_holder;
}

SEXP _new_XDouble_from_tag(const char *classname, SEXP tag)
{
	SEXP shared, ans;

	PROTECT(shared = _new_SharedVector("SharedDouble", tag));
	PROTECT(ans = _new_XVector(classname, shared, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

/* Allocation WITHOUT initialization. */
SEXP _alloc_XDouble(const char *classname, int length)
{
	SEXP tag, ans;

	PROTECT(tag = NEW_NUMERIC(length));
	PROTECT(ans = _new_XDouble_from_tag(classname, tag));
	UNPROTECT(2);
	return ans;
}

