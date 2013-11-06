### =========================================================================
### RdaCollection objects
### -------------------------------------------------------------------------
###
### An RdaCollection object points to a collection of serialized R objects
### stored as 1 object per rda file, all located in the same folder on the
### file system. In addition the name of each rda file must be <objname>.rda,
### where <objname> is the name of the serialized object. Each serialized
### object must have a name that is unique within the RdaCollection object.
###

setClass("RdaCollection",
    representation(
        dirpath="character",   # a single string
        objnames="character"   # a vector of unique object names (no NAs, no
                               # empty strings)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 low-level helper functions.
###

### Recycles shortest arg along longest.
.get_rda_filepath <- function(dirpath, objname)
{
    if (length(objname) == 0L)
        return(character(0))
    filename <- paste0(objname, ".rda")
    file.path(dirpath, filename)
}

.load_serialized_object <- function(dirpath, objname)
{
    filepath <- .get_rda_filepath(dirpath, objname)
    tempenv <- new.env(parent=emptyenv())
    loaded_names <- load(filepath, envir=tempenv)
    if (length(loaded_names) != 1L)
        stop("file '", filepath, "' contains 0 or more ",
             "than 1 serialized object")
    if (loaded_names != objname)
        stop("serialized object in file '", filepath, "' ",
             "doesn't have the expected name (expected: ", objname,
             " -- current: ", loaded_names, ")")
    get(objname, envir=tempenv)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters.
###

setMethod("length", "RdaCollection", function(x) length(x@objnames))

setMethod("names", "RdaCollection", function(x) x@objnames)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RdaCollection <- function(x)
{
    x_dirpath <- x@dirpath
    x_objnames <- x@objnames
    if (!is.character(x_objnames) || !is.null(names(x_objnames)))
        return("\"objnames\" slot must be an unnamed character vector")
    if (anyDuplicated(x_objnames))
        return("\"objnames\" slot contains duplicates")
    if (any(x_objnames %in% c(NA_character_, "")))
        return("\"objnames\" slot contains NAs or empty strings")

    ## Only checks that all the rda files exist. Does NOT try to check
    ## their content (that would be too expensive).
    filepaths <- .get_rda_filepath(x_dirpath, x_objnames)
    missing_idx <- which(!file.exists(filepaths))
    nmissing <- length(missing_idx)
    if (nmissing != 0L) {
        if (nmissing == 1L) {
            what <- "file"
            is_or_are <- "is"
        } else {
            what <- "files"
            is_or_are <- "are"
        }
        filepaths_in_1string <-
            paste(paste0("'", filepaths[missing_idx], "'"), collapse=", ")
        is_or_are <- ifelse(length(missing_idx) == 1L, "is", "are")
        msg <- c(what, filepaths_in_1string, is_or_are, "missing")
        return(paste(msg, collapse=" "))
    }
    NULL
}

setValidity2("RdaCollection", .valid.RdaCollection)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RdaCollection <- function(dirpath, objnames)
{
    new("RdaCollection", dirpath=dirpath, objnames=objnames)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "[[" method (list-element extraction).
###
### We only support subetting by name.
###

setMethod("[[", "RdaCollection",
    function(x, i, j, ...)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (!is.character(i))
            stop("an RdaCollection object can only be subsetted by name")
        if (length(i) < 1L)
            stop("attempt to select less than one element")
        if (length(i) > 1L)
            stop("attempt to select more than one element")
        idx <- match(i, names(x))
        if (is.na(idx))
            stop("invalid object name: ", i)
        .load_serialized_object(x@dirpath, i)
    }
)

