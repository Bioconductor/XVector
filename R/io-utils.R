### =========================================================================
### Some low-level (not exported) I/O utility functions
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
###


new_input_filexp <- function(filepath)
{
    .Call2("new_input_filexp", filepath, PACKAGE="XVector")
}

rewind_filexp <- function(filexp)
{
    .Call2("rewind_filexp", filexp, PACKAGE="XVector")
}

new_output_filexp <- function(filepath, append, compress, compression_level)
{
    .Call2("new_output_filexp",
           filepath, append,
           compress, compression_level,
           PACKAGE="XVector")
}

finalize_filexp <- function(filexp)
{
    .Call2("finalize_filexp", filexp, PACKAGE="XVector")
}

.normarg_input_filepath <- function(filepath)
{
    if (!is.character(filepath) || any(is.na(filepath)))
        stop(wmsg("'filepath' must be a character vector with no NAs"))
    ## First pass: expand local paths and download any remote file.
    filepath2 <- character(length(filepath))
    for (i in seq_len(length(filepath))) {
        fp <- filepath[i]
        con <- file(fp)
        con_class <- class(con)[1L]
        close(con)
        if (con_class == "url") {
            filepath2[i] <- tempfile()
            download.file(fp, filepath2[i])
        } else {
            filepath2[i] <- path.expand(fp)
        }
    }
    ## Second pass: check the type of the local files (all files are
    ## now local).
    filetype <- character(length(filepath2))
    for (i in seq_len(length(filepath2))) {
        fp <- filepath2[i]
        con <- file(fp)
        filetype[i] <- summary(con)$class
        close(con)
        if (!(filetype[i] %in% c("file", "gzfile")))
            stop(wmsg("file \"", filepath[i], "\" ",
                      "has unsupported type: ", filetype[i]))
    }
    names(filepath2) <- filetype
    filepath2
}

### Return a named list of "file external pointers".
open_input_files <- function(filepath)
{
    filepath2 <- .normarg_input_filepath(filepath)
    ans <- lapply(filepath2,
           function(fp)
           {
               filexp <- new_input_filexp(fp)
               reg.finalizer(filexp, finalize_filexp, onexit=TRUE)
               filexp
           })
    names(ans) <- filepath
    ans
}

.normarg_compress <- function(compress)
{
    if (isTRUEorFALSE(compress)) {
        if (compress)
            return("gzip")
        return("no")
    }
    if (isSingleString(compress)) {
        # Types of compression supported by save():
        #VALID_COMPRESS <- c("no", "gzip", "bzip2", "xz")
        VALID_COMPRESS <- c("no", "gzip")
        if (!(compress %in% VALID_COMPRESS))
            stop(wmsg("when 'compress' is a single string, it must be one of ",
                      paste(paste0("\"", VALID_COMPRESS, "\""),
                            collapse=", ")))
        return(compress)
    }
    stop(wmsg("'compress' must be TRUE or FALSE or a single string"))
}

.normarg_compression_level <- function(compression_level, compress)
{
    if (!isSingleNumberOrNA(compression_level))
        stop(wmsg("'compression_level' must be a single number or NA"))
    if (is.na(compression_level))
        return(switch(compress, no=0L, gzip=6L, bzip2=9L, xz=9L))
    if (!is.integer(compression_level))
        compression_level <- as.integer(compression_level)
    if (compression_level < 0L)
        stop(wmsg("'compression_level' cannot be negative"))
    compression_level
}

### Return a named list of one "file external pointer".
open_output_file <- function(filepath, append=FALSE,
                             compress=FALSE, compression_level=NA)
{
    if (!isSingleString(filepath))
        stop(wmsg("'filepath' must be a single string"))
    if (!isTRUEorFALSE(append))
        stop(wmsg("'append' must be TRUE or FALSE"))
    compress <- .normarg_compress(compress)
    compression_level <- .normarg_compression_level(compression_level, compress)
    filepath2 <- path.expand(filepath)
    filexp <- new_output_filexp(filepath2, append,
                                compress, compression_level)
    reg.finalizer(filexp, finalize_filexp, onexit=TRUE)
    ans <- list(filexp)
    names(ans) <- filepath
    ans
}

