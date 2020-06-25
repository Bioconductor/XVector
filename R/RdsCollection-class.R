### =========================================================================
### RdsCollection objects
### -------------------------------------------------------------------------
###
### An RdsCollection object points to a collection of serialized R objects
### stored as 1 object per .rds file, all located in the same directory on
### the file system.
###

setClass("RdsCollection",
    contains="List",
    representation(
        dirpath="character",   # Absolute path to an existing directory.
        filenames="character"  # Vector of file names. The vector itself
                               # must have names on it.
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallel_slot_names()
###

### Combine the new "parallel slots" with those of the parent class. Make
### sure to put the new parallel slots **first**. See R/Vector-class.R file
### in the S4Vectors package for what slots should or should not be considered
### "parallel".
setMethod("parallel_slot_names", "RdsCollection",
    function(x) c("filenames", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###
### Similarly to validity methods, helper functions used by the validity
### method must return TRUE if the component to validate is valid, or a single
### string describing why it's not. With a small difference: while validity
### methods in general are allowed to return a character vector of arbitrary
### length describing a list of problems, our helper functions are only
### allowed to return a single string (or a TRUE). The returned string must
### describe the first encountered problem.
###

.validate_dirpath <- function(dirpath, what="the 'dirpath' slot")
{
    if (!isSingleString(dirpath))
        return(paste0(what, " must be a single string specifying the path ",
                      "to the directory where the .rds files are located"))
    if (!dir.exists(dirpath))
        return(paste0(what, " must contain the path to an existing directory"))
    TRUE
}

.RDS_FILEEXT <- ".rds"

.bad_files_msg <- function(filenames, in_what, be_what)
{
    filenames_in1string <- paste0(paste0("\"", filenames, "\""), collapse=", ")
    paste0("All the files specified in ", in_what, " must be ", be_what, ". ",
           "The following are not: ", filenames_in1string, ".")
}

.validate_filenames <- function(filenames, dirpath, what="the 'filenames' slot")
{
    if (!is.character(filenames))
        return(paste0(what, " must be a character vector"))

    if (anyNA(filenames))
        return(paste0(what, " cannot contain NAs"))

    ## Check extension.
    suffix_ends <- nchar(filenames)
    suffix_starts <- suffix_ends - nchar(.RDS_FILEEXT) + 1L
    suffixes <- substr(filenames, suffix_starts, suffix_ends)
    if (!all(suffixes == .RDS_FILEEXT))
        return(paste0("all filenames in ", what, " must have ",
                      "file extension \"", .RDS_FILEEXT, "\""))

    ## Check that the .rds files exist.
    filepaths <- file.path(dirpath, filenames)
    missing_idx <- which(!file.exists(filepaths))
    if (length(missing_idx) != 0L) {
        be_what <- paste0("present in directory \"", dirpath, "\"")
        return(.bad_files_msg(filenames[missing_idx], what, be_what))
    }

    ## Check that the .rds files are valid RDS files.
    ## Do NOT try to check their content, that would be too expensive!
    not_ok <- vapply(filepaths,
        function(filepath) {
            inherits(try(infoRDS(filepath), silent=TRUE), "try-error")
        },
        logical(1)
    )
    not_ok_idx <- which(not_ok)
    if (length(not_ok_idx) != 0L)
        return(.bad_files_msg(filenames[not_ok_idx], what, "valid RDS files"))

    TRUE
}

.validate_filenames_names <- function(names, what="the 'filenames' slot")
{
    if (is.null(names))
        return(paste0(what, " must have names on it"))
    if (any(names %in% c(NA_character_, "")))
        return(paste0("the names on ", what, " cannot contain NAs ",
                      "or empty strings"))
    TRUE
}

.validate_RdsCollection <- function(x)
{
    ## Validate the 'dirpath' slot.
    msg <- .validate_dirpath(x@dirpath)
    if (!isTRUE(msg))
        return(msg)
    abspath <- file_path_as_absolute(x@dirpath)
    if (abspath != x@dirpath)
        return(paste0("the 'dirpath' slot must contain the absolute ",
                      "path (", abspath, ") to directory ", x@dirpath))

    ## Validate the 'filenames' slot.
    msg <- .validate_filenames(x@filenames, x@dirpath)
    if (!isTRUE(msg))
        return(msg)

    ## Validate the names on the 'filenames' slot.
    msg <- .validate_filenames_names(names(x@filenames))
    if (!isTRUE(msg))
        return(msg)

    TRUE
}

setValidity2("RdsCollection", .validate_RdsCollection)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RdsCollection <- function(path=".", filenames=NULL)
{

    msg <- .validate_dirpath(path, what="'path'")
    if (!isTRUE(msg))
        stop(wmsg(msg))

    check <- FALSE
    if (is.null(filenames)) {
        ## Autogenerate the filenames.
        pattern <- paste0("\\", .RDS_FILEEXT, "$")
        filenames <- dir(path, pattern=pattern, all.files=TRUE)
        check <- TRUE
    } else {
        msg <- .validate_filenames(filenames, path, what="'filenames'")
        if (!isTRUE(msg))
            stop(wmsg(msg))
    }

    names <- names(filenames)
    if (is.null(names)) {
        ## Infer names from the filenames.
        noext_ends <- nchar(filenames) - nchar(.RDS_FILEEXT)
        noext_filenames <- substr(filenames, 1L, noext_ends)
        names(filenames) <- noext_filenames
        check <- TRUE
    } else {
        msg <- .validate_filenames_names(names, what="'filenames'")
        if (!isTRUE(msg))
            stop(wmsg(msg))
    }

    new2("RdsCollection", dirpath=file_path_as_absolute(path),
                          filenames=filenames,
                          check=check)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("path", "RdsCollection", function(object) object@dirpath)

setMethod("names", "RdsCollection", function(x) names(x@filenames))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("extractROWS", "RdsCollection",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x)
        x@filenames <- x@filenames[i]
        x
    }
)

setMethod("getListElement", "RdsCollection",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact)
        readRDS(file.path(path(x), x@filenames[[i]]))
    }
)

