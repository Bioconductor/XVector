#include "../inst/include/XVector_defines.h"
#include <string.h>

#define INTERNAL_ERR_IN "XVector internal error in "

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* io_utils.c */

int _filexp_read(
	SEXP filexp,
	char *buf,
	int buf_size
);

int _filexp_gets(
	SEXP filexp,
	char *buf,
	int buf_size,
	int *EOL_in_buf
);

void _filexp_seek(
	SEXP filexp,
	long long int offset,
	int whence
);

void _filexp_rewind(SEXP filexp);

int _filexp_puts(
	SEXP filexp,
	const char *s
);

void _filexp_putc(
	SEXP filexp,
	int c
);

SEXP new_input_filexp(SEXP filepath);

SEXP rewind_filexp(SEXP filexp);

SEXP new_output_filexp(
	SEXP filepath,
	SEXP append,
	SEXP compress,
	SEXP compression_level
);

SEXP finalize_filexp(SEXP filexp);

int _delete_trailing_LF_or_CRLF(
	const char *buf,
	int buf_len
);


/* RDS_random_access.c */

SEXP RDS_read_file(
	SEXP filexp,
	SEXP mode,
	SEXP attribs_dump
);

SEXP RDS_extract_subvector(
	SEXP filexp,
	SEXP pos
);

SEXP RDS_extract_subarray(
	SEXP filexp,
	SEXP dim,
	SEXP index
);


/* Ocopy_byteblocks.c */

void _Ocopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_from_subscript(
	const int *subscript,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_to_subscript(
	const int *subscript,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_from_subscript_with_lkup(
	const int *subscript,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_to_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_to_subscript_with_lkup(
	const int *subscript,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Orevcopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Orevcopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_from_i1i2_to_complex(
	int i1,
	int i2,
	Rcomplex *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const Rcomplex *lkup,
	int lkup_length
);


/* vector_copy.c */

int _vector_memcmp(
	SEXP x1,
	int x1_offset,
	SEXP x2,
	int x2_offset,
	int nelt
);

void _vector_copy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt
);

void _vector_Ocopy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt,
	SEXP lkup,
	int reverse,
	int Omode
);

void _vector_Ocopy_from_offset(
	SEXP out,
	SEXP in,
	int in_offset,
	int nelt,
	SEXP lkup,
	int reverse
);

void _vector_Ocopy_to_offset(
	SEXP out,
	SEXP in,
	int out_offset,
	int nelt,
	SEXP lkup
);

void _vector_Ocopy_from_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

void _vector_Ocopy_to_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

void _vector_mcopy(
	SEXP out,
	int out_offset,
	SEXP in,
	SEXP in_start,
	SEXP in_width,
	SEXP lkup,
	int reverse
);


/* SharedVector_class.c */

SEXP get_object_address(SEXP x);

SEXP get_list_addresses(SEXP x);

SEXP externalptr_new();

SEXP externalptr_get_tag(SEXP x);

SEXP externalptr_set_tag(
	SEXP x,
	SEXP tag
);

SEXP externalptr_tagtype(SEXP x);

SEXP externalptr_taglength(SEXP x);

SEXP externalptr_show(SEXP x);

SEXP _get_SharedVector_tag(SEXP x);

int _get_SharedVector_length(SEXP x);

SEXP _new_SharedVector(const char *classname, SEXP tag);

SEXP SharedVector_address0(SEXP x);

SEXP SharedVector_memcmp(
	SEXP x1,
	SEXP start1,
	SEXP x2,
	SEXP start2,
	SEXP width
);

SEXP SharedVector_Ocopy_from_start(
	SEXP out,
	SEXP in,
	SEXP in_start,
	SEXP width,
	SEXP lkup,
	SEXP reverse
);

SEXP SharedVector_Ocopy_from_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

SEXP SharedVector_mcopy(
	SEXP out,
	SEXP out_offset,
	SEXP in,
	SEXP in_start,
	SEXP in_width,
	SEXP lkup,
	SEXP reverse
);

SEXP _get_SharedVector_Pool_xp_list(SEXP x);

SEXP _new_SharedRaw_Pool(SEXP tags);

SEXP _new_SharedInteger_Pool(SEXP tags);

SEXP _new_SharedDouble_Pool(SEXP tags);

SEXP _new_SharedVector_Pool1(SEXP shared);


/* SharedRaw_class.c */

SEXP SharedRaw_new(
	SEXP length,
	SEXP val
);

SEXP SharedRaw_read_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedRaw_read_chars_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedRaw_write_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string
);

SEXP SharedRaw_write_chars_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP string
);

SEXP SharedRaw_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedRaw_read_ints_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedRaw_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedRaw_write_ints_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);

SEXP SharedRaw_read_enc_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP SharedRaw_read_enc_chars_from_subscript(
	SEXP src,
	SEXP subscript,
	SEXP lkup
);

SEXP SharedRaw_write_enc_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string,
	SEXP lkup
);

SEXP SharedRaw_write_enc_chars_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP string,
	SEXP lkup
);

SEXP SharedRaw_read_complexes_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP SharedRaw_read_complexes_from_subscript(
	SEXP src,
	SEXP subscript,
	SEXP lkup
);


/* SharedInteger_class.c */

SEXP SharedInteger_new(
	SEXP length,
	SEXP val
);

SEXP SharedInteger_get_show_string(SEXP x);

SEXP SharedInteger_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedInteger_read_ints_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedInteger_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedInteger_write_ints_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);


/* SharedDouble_class.c */

SEXP SharedDouble_new(
	SEXP length,
	SEXP val
);

SEXP SharedDouble_get_show_string(SEXP x);

SEXP SharedDouble_read_nums_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedDouble_read_nums_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedDouble_write_nums_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedDouble_write_nums_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);


/* XVector_class.c */

SEXP _get_XVector_shared(SEXP x);

int _get_XVector_offset(SEXP x);

int _get_XVector_length(SEXP x);

SEXP _get_XVector_tag(SEXP x);

Chars_holder _hold_XRaw(SEXP x);

Ints_holder _hold_XInteger(SEXP x);

Doubles_holder _hold_XDouble(SEXP x);

SEXP _new_XVector(
	const char *classname,
	SEXP shared,
	int offset,
	int length
);

SEXP _new_XRaw_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _new_XInteger_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _new_XDouble_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _alloc_XRaw(
	const char *classname,
	int length
);

SEXP _alloc_XInteger(
	const char *classname,
	int length
);

SEXP _alloc_XDouble(
	const char *classname,
	int length
);


/* XVectorList_class.c */

SEXP _get_XVectorList_pool(SEXP x);

SEXP _get_XVectorList_ranges(SEXP x);

int _get_XVectorList_length(SEXP x);

SEXP _get_XVectorList_width(SEXP x);

SEXP _get_XVectorList_names(SEXP x);

XVectorList_holder _hold_XVectorList(SEXP x);

int _get_length_from_XVectorList_holder(const XVectorList_holder *x_holder);

Chars_holder _get_elt_from_XRawList_holder(
	const XVectorList_holder *x_holder,
	int i
);

Ints_holder _get_elt_from_XIntegerList_holder(
	const XVectorList_holder *x_holder,
	int i
);

Doubles_holder _get_elt_from_XDoubleList_holder(
	const XVectorList_holder *x_holder,
	int i
);

XVectorList_holder _get_linear_subset_from_XVectorList_holder(
	const XVectorList_holder *x_holder,
	int offset,
	int length
);

void _set_XVectorList_names(SEXP x, SEXP names);

SEXP _new_XRawList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP _new_XIntegerList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP _new_XDoubleList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP _new_XRawList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP _new_XIntegerList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP _new_XDoubleList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP _alloc_XRawList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP _alloc_XIntegerList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP _alloc_XDoubleList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP _new_XRawList_from_CharAEAE(
	const char *classname,
	const char *element_type,
	const CharAEAE *char_aeae,
	SEXP lkup
);

SEXP _new_XIntegerList_from_IntAEAE(
	const char *classname,
	const char *element_type,
	const IntAEAE *int_aeae
);


/* XRawList_comparison.c */

SEXP XRawList_pcompare(
	SEXP x,
	SEXP y
);

SEXP XRawList_is_unsorted(
	SEXP x,
	SEXP strictly
);

SEXP XRawList_order(
	SEXP x,
	SEXP decreasing
);

SEXP XRawList_rank(
	SEXP x,
	SEXP ties_method
);

SEXP XRawList_match_hash(
	SEXP x1,
	SEXP x2,
	SEXP nomatch
);

SEXP XRawList_selfmatch_hash(
	SEXP x
);


/* slice_methods.c */

SEXP XInteger_slice(
	SEXP x,
	SEXP lower,
	SEXP upper
);

SEXP XDouble_slice(
	SEXP x,
	SEXP lower,
	SEXP upper,
	SEXP include_lower,
	SEXP include_upper
);


/* view_summarization_methods.c */

SEXP XIntegerViews_summary1(
	SEXP x,
	SEXP na_rm,
	SEXP method
);

SEXP XDoubleViews_summary1(
	SEXP x,
	SEXP na_rm,
	SEXP method
);

SEXP XIntegerViews_summary2(
	SEXP x,
	SEXP na_rm,
	SEXP method
);

SEXP XDoubleViews_summary2(
	SEXP x,
	SEXP na_rm,
	SEXP method
);

