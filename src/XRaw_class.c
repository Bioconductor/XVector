/****************************************************************************
 *                  Low-level manipulation of XRaw objects                  *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "XVector.h"
#include "S4Vectors_interface.h"


Chars_holder _hold_XRaw(SEXP x)
{
	Chars_holder x_holder;
	SEXP tag;
	int offset;

	tag = _get_XVector_tag(x);
	offset = _get_XVector_offset(x);
	x_holder.ptr = (const char *) (RAW(tag) + offset);
	x_holder.length = _get_XVector_length(x);
	return x_holder;
}

SEXP _new_XRaw_from_tag(const char *classname, SEXP tag)
{
	SEXP shared, ans;

	PROTECT(shared = _new_SharedVector("SharedRaw", tag));
	PROTECT(ans = _new_XVector(classname, shared, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

/* Allocation WITHOUT initialization. */
SEXP _alloc_XRaw(const char *classname, int length)
{
	SEXP tag, ans;

	PROTECT(tag = NEW_RAW(length));
	PROTECT(ans = _new_XRaw_from_tag(classname, tag));
	UNPROTECT(2);
	return ans;
}

SEXP C_extract_character_from_XRaw_by_positions(SEXP x, SEXP pos,
						SEXP collapse, SEXP lkup)
{
	SEXP x_tag;
	int x_off, x_len;

	x_tag = _get_XVector_tag(x);
	if (!IS_RAW(x_tag))
		error("'x' must be an XRaw object");
	x_off = _get_XVector_offset(x);
	x_len = _get_XVector_length(x);

	if (!IS_INTEGER(pos))
		error("'pos' must be an integer vector");

	if (!(IS_LOGICAL(collapse) && LENGTH(collapse) == 1))
		error("'collapse' must be TRUE or FALSE");

	return extract_bytes_by_positions(
				(const char *) (RAW(x_tag) + x_off), x_len,
				INTEGER(pos), LENGTH(pos),
				LOGICAL(collapse)[0], lkup);
}

SEXP C_extract_character_from_XRaw_by_ranges(SEXP x, SEXP start, SEXP width,
					     SEXP collapse, SEXP lkup)
{
	SEXP x_tag;
	int x_off, x_len;
	int nranges;
	const int *start_p, *width_p;

	x_tag = _get_XVector_tag(x);
	if (!IS_RAW(x_tag))
		error("'x' must be an XRaw object");
	x_off = _get_XVector_offset(x);
	x_len = _get_XVector_length(x);

	nranges = check_integer_pairs(start, width,
				      &start_p, &width_p,
				      "start", "width");

	if (!(IS_LOGICAL(collapse) && LENGTH(collapse) == 1))
		error("'collapse' must be TRUE or FALSE");

	return extract_bytes_by_ranges(
				(const char *) (RAW(x_tag) + x_off), x_len,
				start_p, width_p, nranges,
				LOGICAL(collapse)[0], lkup);
}

