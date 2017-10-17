### =========================================================================
### Random access to the elements of a serialized atomic vector or array
### -------------------------------------------------------------------------


### Should probably move this to R/io-utils.R
.open_input_file <- function(file)
{
    filexp_list <- open_input_files(file)
    stopifnot(length(filexp_list) == 1L)
    filexp_list[[1L]]
}

.read_RDS_file <- function(file, mode, attribs_dump=NULL)
{
    filexp <- .open_input_file(file)
    .Call("RDS_read_file", filexp, mode, attribs_dump, PACKAGE="XVector")
}

read_RDS <- function(file, attribs.only=FALSE)
{
    mode <- if (attribs.only) 3L else 0L
    attribs_dump <- new.env(parent=emptyenv())
    ans <- .read_RDS_file(file, mode, attribs_dump=attribs_dump)
    if (attribs.only)
        ans <- attribs_dump
    ans
}

read_RDS_typeof_and_length <- function(file) .read_RDS_file(file, 4L)

extract_subvector_from_RDS_vector <- function(file, pos)
{
    filexp <- .open_input_file(file)
    .Call("RDS_extract_subvector", filexp, pos, PACKAGE="XVector")
}

extract_subarray_from_RDS_array <- function(file, index)
{
    attribs_dump <- read_RDS(file, attribs.only=TRUE)
    x_dim <- try(get("dim", envir=attribs_dump, inherits=FALSE), silent=TRUE)
    if (inherits(x_dim , "try-error"))
        stop("serialized object is not an array")
    filexp <- .open_input_file(file)
    .Call("RDS_extract_subarray", filexp, x_dim, index, PACKAGE="XVector")
}

