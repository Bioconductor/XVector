#include "XVector.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("XVector", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* io_utils.c */
	CALLMETHOD_DEF(new_input_filexp, 1),
	CALLMETHOD_DEF(rewind_filexp, 1),
	CALLMETHOD_DEF(new_output_filexp, 4),
	CALLMETHOD_DEF(finalize_filexp, 1),

/* RDS_random_access.c */
	CALLMETHOD_DEF(RDS_read_file, 3),
	CALLMETHOD_DEF(RDS_extract_subvector, 2),
	CALLMETHOD_DEF(RDS_extract_subarray, 3),

/* SharedVector_class.c */
	CALLMETHOD_DEF(get_object_address, 1),
	CALLMETHOD_DEF(get_list_addresses, 1),
	CALLMETHOD_DEF(externalptr_new, 0),
	CALLMETHOD_DEF(externalptr_get_tag, 1),
	CALLMETHOD_DEF(externalptr_set_tag, 2),
	CALLMETHOD_DEF(externalptr_tagtype, 1),
	CALLMETHOD_DEF(externalptr_taglength, 1),
	CALLMETHOD_DEF(externalptr_show, 1),
	CALLMETHOD_DEF(SharedVector_address0, 1),
	CALLMETHOD_DEF(SharedVector_memcmp, 5),
	CALLMETHOD_DEF(SharedVector_Ocopy_from_start, 6),
	CALLMETHOD_DEF(SharedVector_Ocopy_from_subscript, 4),
	CALLMETHOD_DEF(SharedVector_mcopy, 7),

/* SharedRaw_class.c */
	CALLMETHOD_DEF(SharedRaw_new, 2),

	CALLMETHOD_DEF(SharedRaw_read_chars_from_i1i2, 3),
	CALLMETHOD_DEF(SharedRaw_read_chars_from_subscript, 2),
	CALLMETHOD_DEF(SharedRaw_write_chars_to_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_write_chars_to_subscript, 3),

	CALLMETHOD_DEF(SharedRaw_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(SharedRaw_read_ints_from_subscript, 2),
	CALLMETHOD_DEF(SharedRaw_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_write_ints_to_subscript, 3),

	CALLMETHOD_DEF(SharedRaw_read_enc_chars_from_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_read_enc_chars_from_subscript, 3),
	CALLMETHOD_DEF(SharedRaw_write_enc_chars_to_i1i2, 5),
	CALLMETHOD_DEF(SharedRaw_write_enc_chars_to_subscript, 4),

	CALLMETHOD_DEF(SharedRaw_read_complexes_from_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_read_complexes_from_subscript, 3),

/* SharedInteger_class.c */
	CALLMETHOD_DEF(SharedInteger_new, 2),
	CALLMETHOD_DEF(SharedInteger_get_show_string, 1),

	CALLMETHOD_DEF(SharedInteger_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(SharedInteger_read_ints_from_subscript, 2),
	CALLMETHOD_DEF(SharedInteger_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(SharedInteger_write_ints_to_subscript, 3),

/* SharedDouble_class.c */
	CALLMETHOD_DEF(SharedDouble_new, 2),
	CALLMETHOD_DEF(SharedDouble_get_show_string, 1),

	CALLMETHOD_DEF(SharedDouble_read_nums_from_i1i2, 3),
	CALLMETHOD_DEF(SharedDouble_read_nums_from_subscript, 2),
	CALLMETHOD_DEF(SharedDouble_write_nums_to_i1i2, 4),
	CALLMETHOD_DEF(SharedDouble_write_nums_to_subscript, 3),

/* XRawList_comparison.c */
	CALLMETHOD_DEF(XRawList_pcompare, 2),
	CALLMETHOD_DEF(XRawList_is_unsorted, 2),
	CALLMETHOD_DEF(XRawList_order, 2),
	CALLMETHOD_DEF(XRawList_rank, 2),
	CALLMETHOD_DEF(XRawList_match_hash, 3),
	CALLMETHOD_DEF(XRawList_selfmatch_hash, 1),

/* slice_methods.c */
	CALLMETHOD_DEF(XInteger_slice, 3),
	CALLMETHOD_DEF(XDouble_slice, 5),

/* view_summarization_methods.c */
	CALLMETHOD_DEF(XIntegerViews_summary1, 3),
	CALLMETHOD_DEF(XDoubleViews_summary1, 3),
	CALLMETHOD_DEF(XIntegerViews_summary2, 3),
	CALLMETHOD_DEF(XDoubleViews_summary2, 3),

	{NULL, NULL, 0}
};


void R_init_XVector(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* io_utils.c */
	REGISTER_CCALLABLE(_filexp_read);
	REGISTER_CCALLABLE(_filexp_gets);
	REGISTER_CCALLABLE(_filexp_seek);
	REGISTER_CCALLABLE(_filexp_rewind);
	REGISTER_CCALLABLE(_filexp_puts);
	REGISTER_CCALLABLE(_filexp_putc);
	REGISTER_CCALLABLE(_delete_trailing_LF_or_CRLF);

/* Ocopy_byteblocks.c */
	REGISTER_CCALLABLE(_Ocopy_byteblocks_from_i1i2);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_from_subscript);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_to_i1i2);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_to_subscript);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_subscript_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_to_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_to_subscript_with_lkup);
	REGISTER_CCALLABLE(_Orevcopy_byteblocks_from_i1i2);
	REGISTER_CCALLABLE(_Orevcopy_bytes_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_i1i2_to_complex);

/* SharedVector_class.c */
	REGISTER_CCALLABLE(_new_SharedVector);
	REGISTER_CCALLABLE(_get_SharedVector_tag);
	REGISTER_CCALLABLE(_get_SharedVector_length);

/* XVector_class.c */
	REGISTER_CCALLABLE(_get_XVector_shared);
	REGISTER_CCALLABLE(_get_XVector_offset);
	REGISTER_CCALLABLE(_get_XVector_length);
	REGISTER_CCALLABLE(_get_XVector_tag);
	REGISTER_CCALLABLE(_hold_XRaw);
	REGISTER_CCALLABLE(_hold_XInteger);
	REGISTER_CCALLABLE(_hold_XDouble);
	REGISTER_CCALLABLE(_new_XVector);
	REGISTER_CCALLABLE(_new_XRaw_from_tag);
	REGISTER_CCALLABLE(_new_XInteger_from_tag);
	REGISTER_CCALLABLE(_new_XDouble_from_tag);
	REGISTER_CCALLABLE(_alloc_XRaw);
	REGISTER_CCALLABLE(_alloc_XInteger);
	REGISTER_CCALLABLE(_alloc_XDouble);

/* XVectorList_class.c */
	REGISTER_CCALLABLE(_get_XVectorList_length);
	REGISTER_CCALLABLE(_get_XVectorList_width);
	REGISTER_CCALLABLE(_get_XVectorList_names);
	REGISTER_CCALLABLE(_hold_XVectorList);
	REGISTER_CCALLABLE(_get_length_from_XVectorList_holder);
	REGISTER_CCALLABLE(_get_elt_from_XRawList_holder);
	REGISTER_CCALLABLE(_get_elt_from_XIntegerList_holder);
	REGISTER_CCALLABLE(_get_elt_from_XDoubleList_holder);
	REGISTER_CCALLABLE(_get_linear_subset_from_XVectorList_holder);
	REGISTER_CCALLABLE(_set_XVectorList_names);
	REGISTER_CCALLABLE(_new_XRawList_from_tags);
	REGISTER_CCALLABLE(_new_XIntegerList_from_tags);
	REGISTER_CCALLABLE(_new_XDoubleList_from_tags);
	REGISTER_CCALLABLE(_new_XRawList_from_tag);
	REGISTER_CCALLABLE(_new_XIntegerList_from_tag);
	REGISTER_CCALLABLE(_new_XDoubleList_from_tag);
	REGISTER_CCALLABLE(_alloc_XRawList);
	REGISTER_CCALLABLE(_alloc_XIntegerList);
	REGISTER_CCALLABLE(_alloc_XDoubleList);
	REGISTER_CCALLABLE(_new_XRawList_from_CharAEAE);
	REGISTER_CCALLABLE(_new_XIntegerList_from_IntAEAE);
	return;
}

