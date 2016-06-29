/****************************************************************************
 *              Low-level manipulation of XVectorList objects               *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "XVector.h"
#include "IRanges_interface.h"
#include "S4Vectors_interface.h"


/****************************************************************************
 * C-level slot getters for XVectorList objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	pool_symbol = NULL,
	ranges_symbol = NULL;

SEXP _get_XVectorList_pool(SEXP x)
{
	INIT_STATIC_SYMBOL(pool)
	return GET_SLOT(x, pool_symbol);
}

SEXP _get_XVectorList_ranges(SEXP x)
{
	INIT_STATIC_SYMBOL(ranges)
	return GET_SLOT(x, ranges_symbol);
}

/* Not strict "slot getters" but very much like. */

int _get_XVectorList_length(SEXP x)
{
	return get_IRanges_length(_get_XVectorList_ranges(x));
}

SEXP _get_XVectorList_width(SEXP x)
{
	return get_IRanges_width(_get_XVectorList_ranges(x));
}

SEXP _get_XVectorList_names(SEXP x)
{
	return get_IRanges_names(_get_XVectorList_ranges(x));
}


/****************************************************************************
 * C-level slot getter, slot setter, and constructor for GroupedIRanges
 * objects.
 */

static SEXP group_symbol = NULL;

static SEXP get_GroupedIRanges_group(SEXP x)
{
	INIT_STATIC_SYMBOL(group)
	return GET_SLOT(x, group_symbol);
}

static void set_GroupedIRanges_group(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(group)
	SET_SLOT(x, group_symbol, value);
	return;
}

static SEXP new_GroupedIRanges(SEXP ranges, SEXP group)
{
	SEXP ans;

	PROTECT(ans = new_IRanges("GroupedIRanges",
				get_IRanges_start(ranges),
				get_IRanges_width(ranges),
				get_IRanges_names(ranges)));
	set_GroupedIRanges_group(ans, group);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * C-level abstract getters.
 */

XVectorList_holder _hold_XVectorList(SEXP x)
{
	XVectorList_holder x_holder;
	SEXP ranges;

	x_holder.classname = get_classname(x);
	x_holder.element_type = get_List_elementType(x);
	x_holder.xp_list = _get_SharedVector_Pool_xp_list(
				_get_XVectorList_pool(x));
	ranges = _get_XVectorList_ranges(x);
	x_holder.length = get_IRanges_length(ranges);
	x_holder.start = INTEGER(get_IRanges_start(ranges));
	x_holder.width = INTEGER(get_IRanges_width(ranges));
	x_holder.group = INTEGER(get_GroupedIRanges_group(ranges));
	return x_holder;
}

int _get_length_from_XVectorList_holder(const XVectorList_holder *x_holder)
{
	return x_holder->length;
}

Chars_holder _get_elt_from_XRawList_holder(const XVectorList_holder *x_holder,
		int i)
{
	SEXP tag;
	Chars_holder x_elt_holder;

	tag = R_ExternalPtrTag(VECTOR_ELT(x_holder->xp_list,
					  x_holder->group[i] - 1));
	x_elt_holder.ptr = (const char *) RAW(tag) + x_holder->start[i] - 1;
	x_elt_holder.length = x_holder->width[i];
	return x_elt_holder;
}

Ints_holder _get_elt_from_XIntegerList_holder(const XVectorList_holder *x_holder,
		int i)
{
	SEXP tag;
	Ints_holder x_elt_holder;

	tag = R_ExternalPtrTag(VECTOR_ELT(x_holder->xp_list,
					  x_holder->group[i] - 1));
	x_elt_holder.ptr = INTEGER(tag) + x_holder->start[i] - 1;
	x_elt_holder.length = x_holder->width[i];
	return x_elt_holder;
}

Doubles_holder _get_elt_from_XDoubleList_holder(const XVectorList_holder *x_holder,
		int i)
{
	SEXP tag;
	Doubles_holder x_elt_holder;

	tag = R_ExternalPtrTag(VECTOR_ELT(x_holder->xp_list,
					  x_holder->group[i] - 1));
	x_elt_holder.ptr = REAL(tag) + x_holder->start[i] - 1;
	x_elt_holder.length = x_holder->width[i];
	return x_elt_holder;
}

XVectorList_holder _get_linear_subset_from_XVectorList_holder(
		const XVectorList_holder *x_holder, int offset, int length)
{
	XVectorList_holder y_holder;

	y_holder = *x_holder;
	y_holder.length = length;
	y_holder.start += offset;
	y_holder.width += offset;
	y_holder.group += offset;
	return y_holder;
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_XVectorList_pool(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(pool)
	SET_SLOT(x, pool_symbol, value);
	return;
}

static void set_XVectorList_ranges(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(ranges)
	SET_SLOT(x, ranges_symbol, value);
	return;
}

/*
 * Not strict a "slot getter" but very much like.
 * WARNING: x@ranges@NAMES is modified in-place!
 */
void _set_XVectorList_names(SEXP x, SEXP names)
{
	set_IRanges_names(_get_XVectorList_ranges(x), names);
	return;
}


/****************************************************************************
 * C-level constructors.
 *
 * Please be aware that these functions do NOT duplicate their arguments
 * before putting them in the slots of the returned object.
 * Thus they cannot be made .Call entry points!
 */

/* Constructing an XVectorList object from a list of tags.  */

static SEXP new_XVectorList_from_tags(const char *classname,
		const char *element_type,
		SEXP (*new_SharedVector_Pool)(SEXP),
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	SEXP classdef, ans, ans_pool, ans_ranges;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));

	/* set "elementType" slot */
	set_List_elementType(ans, element_type);

	/* set "pool" slot */
	PROTECT(ans_pool = new_SharedVector_Pool(tags));
	set_XVectorList_pool(ans, ans_pool);
	UNPROTECT(1);

	/* set "ranges" slot */
	PROTECT(ans_ranges = new_GroupedIRanges(ranges, ranges_group));
	set_XVectorList_ranges(ans, ans_ranges);
	UNPROTECT(1);

	UNPROTECT(2);
	return ans;
}

SEXP _new_XRawList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedRaw_Pool,
					 tags, ranges, ranges_group);
}

SEXP _new_XIntegerList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedInteger_Pool,
					 tags, ranges, ranges_group);
}

SEXP _new_XDoubleList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedDouble_Pool,
					 tags, ranges, ranges_group);
}

/*
 * Constructing an XVectorList object from a single tag.
 * For convenience, 'ranges' can be NULL as a way to indicate that the
 * returned XVectorList object has only 1 element that spans the entire tag.
 */

static SEXP new_XVectorList_from_tag(const char *classname,
		const char *element_type,
		SEXP (*new_SharedVector_Pool)(SEXP),
		SEXP tag, SEXP ranges)
{
	SEXP tags, ans_start, ans_width, ranges_group, ans;
	int nprotect = 0, ans_length, i;

	/* prepare 'tags' */
	PROTECT(tags = NEW_LIST(1));
	nprotect++;
	SET_VECTOR_ELT(tags, 0, tag);

	/* prepare 'ranges' */
	if (ranges == NULL) {
		PROTECT(ans_start = ScalarInteger(1));
		PROTECT(ans_width = ScalarInteger(LENGTH(tag)));
		PROTECT(ranges = new_IRanges("IRanges",
					ans_start, ans_width, R_NilValue));
		nprotect += 3;
	}

	/* prepare 'ranges_group' */
	ans_length = get_IRanges_length(ranges);
	PROTECT(ranges_group = NEW_INTEGER(ans_length));
	nprotect++;
	for (i = 0; i < ans_length; i++)
		INTEGER(ranges_group)[i] = 1;

	/* make the XVectorList object */
	PROTECT(ans = new_XVectorList_from_tags(classname, element_type,
				new_SharedVector_Pool,
				tags, ranges, ranges_group));
	nprotect++;
	UNPROTECT(nprotect);
	return ans;
}

SEXP _new_XRawList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedRaw_Pool,
					tag, ranges);
}

SEXP _new_XIntegerList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedInteger_Pool,
					tag, ranges);
}

SEXP _new_XDoubleList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedDouble_Pool,
					tag, ranges);
}

/* Allocation WITHOUT initialization.
 * This is a soft limit. Some tags could be longer than this limit if the
 * XVectorList object to allocate contains elements that are also longer
 * than this limit. */
#define	MAX_TAG_LENGTH 268435456  /* = 256 Mb if tag is a raw vector */

static SEXP alloc_XVectorList(const char *classname,
		const char *element_type, const char *tag_type,
		SEXP width)
{
	int ans_length, tag_length, new_tag_length, i, nelt;
	SEXP start, group, ranges, tags, tag, ans;
	IntAE *tag_lengths;

	ans_length = LENGTH(width);
	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(group = NEW_INTEGER(ans_length));
	tag_lengths = new_IntAE(0, 0, 0);
	if (ans_length != 0) {
		tag_length = 0;
		for (i = 0; i < ans_length; i++) {
			new_tag_length = tag_length + INTEGER(width)[i];
			if (new_tag_length > MAX_TAG_LENGTH
			 || new_tag_length < tag_length) {
				IntAE_insert_at(tag_lengths,
					IntAE_get_nelt(tag_lengths),
					tag_length);
				tag_length = 0;
			}
			INTEGER(start)[i] = tag_length + 1;
			INTEGER(group)[i] = IntAE_get_nelt(tag_lengths) + 1;
			tag_length += INTEGER(width)[i];
		}
		IntAE_insert_at(tag_lengths,
			IntAE_get_nelt(tag_lengths), tag_length);
	}
	PROTECT(ranges = new_IRanges("IRanges", start, width, NULL));
	nelt = IntAE_get_nelt(tag_lengths);
	PROTECT(tags = NEW_LIST(nelt));
	if (strcmp(tag_type, "raw") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_RAW(tag_lengths->elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XRawList_from_tags(classname,
					element_type, tags, ranges, group));
	} else if (strcmp(tag_type, "integer") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_INTEGER(tag_lengths->elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XIntegerList_from_tags(classname,
					element_type, tags, ranges, group));
	} else if (strcmp(tag_type, "double") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_NUMERIC(tag_lengths->elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XDoubleList_from_tags(classname,
					element_type, tags, ranges, group));
	} else {
		UNPROTECT(4);
		error("IRanges internal error in alloc_XVectorList(): "
		      "%s: invalid 'tag_type'", tag_type);
	}
	UNPROTECT(5);
	return ans;
}

SEXP _alloc_XRawList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "raw", width);
}

SEXP _alloc_XIntegerList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "integer", width);
}

SEXP _alloc_XDoubleList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "double", width);
}

/* More constructors. */

SEXP _new_XRawList_from_CharAEAE(const char *classname,
		const char *element_type,
		const CharAEAE *char_aeae, SEXP lkup)
{
	const int *lkup0;
	int lkup_length, nelt, i;
	SEXP ans_width, ans;
	const CharAE *src;
	XVectorList_holder ans_holder;
	Chars_holder dest;

	if (lkup == R_NilValue) {
		lkup0 = NULL;
	} else {
		lkup0 = INTEGER(lkup);
		lkup_length = LENGTH(lkup);
	}
	nelt = CharAEAE_get_nelt(char_aeae);
	PROTECT(ans_width = NEW_INTEGER(nelt));
	for (i = 0; i < nelt; i++) {
		src = char_aeae->elts[i];
		INTEGER(ans_width)[i] = CharAE_get_nelt(src);
	}
	PROTECT(ans = _alloc_XRawList(classname, element_type, ans_width));
	ans_holder = _hold_XVectorList(ans);
	for (i = 0; i < nelt; i++) {
		src = char_aeae->elts[i];
		dest = _get_elt_from_XRawList_holder(&ans_holder, i);
		/* dest.ptr is a const char * so we need to cast it to
		   char * before we can write to it */
		_Ocopy_bytes_to_i1i2_with_lkup(0, dest.length - 1,
			(char *) dest.ptr, dest.length,
			src->elts, CharAE_get_nelt(src),
			lkup0, lkup_length);
	}
	UNPROTECT(2);
	return ans;
}

SEXP _new_XIntegerList_from_IntAEAE(const char *classname,
		const char *element_type,
		const IntAEAE *int_aeae)
{
	int nelt, i;
	SEXP ans_width, ans;
	const IntAE *src;
	XVectorList_holder ans_holder;
	Ints_holder dest;

	nelt = IntAEAE_get_nelt(int_aeae);
	PROTECT(ans_width = NEW_INTEGER(nelt));
	for (i = 0; i < nelt; i++) {
		src = int_aeae->elts[i];
		INTEGER(ans_width)[i] = IntAE_get_nelt(src);
	}
	PROTECT(ans = _alloc_XIntegerList(classname, element_type, ans_width));
	ans_holder = _hold_XVectorList(ans);
	for (i = 0; i < nelt; i++) {
		src = int_aeae->elts[i];
		dest = _get_elt_from_XIntegerList_holder(&ans_holder, i);
		/* dest.ptr is a const int * so we need to cast it to
		   char * before we can write to it */
		_Ocopy_byteblocks_to_i1i2(0, dest.length - 1,
			(char *) dest.ptr, dest.length,
			(const char *) src->elts, IntAE_get_nelt(src),
			sizeof(int));
	}
	UNPROTECT(2);
	return ans;
}

