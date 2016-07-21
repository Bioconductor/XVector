### =========================================================================
### XVectorList objects
### -------------------------------------------------------------------------
###
### An XVectorList object is *conceptually* a list of XVector objects
### but is actually not *implemented* as a list of such objects.
### This is to avoid having to generate long lists of S4 objects which the
### current S4 implementation is *very* inefficient at.
###

setClass("GroupedIRanges",
    contains="IRanges",
    representation(
        group="integer"
    )
)

setClass("XVectorList",
    contains="List",
    representation(
        "VIRTUAL",
        pool="SharedVector_Pool",
        ranges="GroupedIRanges"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GroupedIRanges methods.
###

### Ugly workaround a serious callNextMethod inefficiency reported here:
###   https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16974
.GroupedIRanges_parallelSlotNames <-
    c("group", parallelSlotNames(new("IRanges")))

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "GroupedIRanges",
    #function(x) c("group", callNextMethod())
    function(x) .GroupedIRanges_parallelSlotNames
)

.valid.GroupedIRanges <- function(x)
{
    if (length(x@group) != length(x))
        return("slot \"group\" slot must have same length as object")
    NULL
}

setValidity2("GroupedIRanges", .valid.GroupedIRanges)

setMethod("as.data.frame", "GroupedIRanges",
    function(x, row.names=NULL, optional=FALSE, ...)
        cbind(group=x@group, callNextMethod(), stringsAsFactors=FALSE)
)

setMethod("show", "GroupedIRanges",
    function(object) show(as.data.frame(object))
)

setMethod("c", "GroupedIRanges",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for GroupedIRanges objects ",
                 "does not support the 'recursive' argument")
        old_val <- S4Vectors:::disableValidity()
        on.exit(S4Vectors:::disableValidity(old_val))
        S4Vectors:::disableValidity(TRUE)
        ans <- callNextMethod(x, ..., recursive=FALSE)
        ans@group <-
            do.call(c, lapply(unname(list(x, ...)), function(arg) arg@group))
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList accessor-like methods.
###

setMethod("length", "XVectorList", function(x) length(x@ranges))

setMethod("width", "XVectorList", function(x) width(x@ranges))

setMethod("elementNROWS", "XVectorList", function(x) width(x))

setMethod("names", "XVectorList", function(x) names(x@ranges))

setReplaceMethod("names", "XVectorList",
    function(x, value)
    {
        names(x@ranges) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 internal bookkeeping functions to keep the XVectorList "pool" slot
### clean and tidy.
###

### Used in "extractROWS" method for XVectorList objects.
.dropUnusedPoolElts <- function(x)
{
    pool_len <- length(x@pool)
    if (pool_len == 0L)
        return(x)
    keep_it <- logical(pool_len)
    keep_it[x@ranges@group] <- TRUE
    keep_idx <- which(keep_it)
    remap <- integer(pool_len)
    remap[keep_idx] <- seq_len(length(keep_idx))
    x@pool <- x@pool[keep_idx]
    x@ranges@group <- remap[x@ranges@group]
    x
}

### Used in "c" method for XVectorList objects and in
### new_XVectorList_from_list_of_XVector() constructor.
.dropDuplicatedPoolElts <- function(x)
{
    pool_len <- length(x@pool)
    if (pool_len == 0L)
        return(x)
    addr <- addresses(x@pool@xp_list)
    keep_idx <- which(!duplicated(addr))
    remap <- match(addr, addr[keep_idx])
    x@pool <- x@pool[keep_idx]
    x@ranges@group <- remap[x@ranges@group]
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList constructors.
###

new_XVectorList_from_list_of_XVector <- function(classname, x)
{
    if (!is.list(x))
        stop("'x' must be a list")
    x_names <- names(x)
    if (!is.null(x_names))
        names(x) <- NULL
    ans_elementType <- elementType(new(classname))
    x_len <- length(x)
    if (x_len != 0L) {
        ok <- lapply(x, function(x_elt) is(x_elt, ans_elementType))
        if (!all(unlist(ok)))
            stop("all elements in 'x' must be ", ans_elementType,
                 " objects")
    }
    elt0 <- new(ans_elementType)
    ans_pool_class <- class(elt0@shared)
    shared_list <- lapply(x, function(x_elt) x_elt@shared)
    ans_pool <- new_SharedVector_Pool_from_list_of_SharedVector(ans_pool_class,
                                                                shared_list)
    if (x_len == 0L) {
        ans_ranges <- new2("GroupedIRanges", check=FALSE)
    } else {
        ans_ranges_start <- unlist(lapply(x, function(x_elt) x_elt@offset)) +
                            1L
        ans_ranges_width <- unlist(lapply(x, function(x_elt) x_elt@length))
        ans_ranges_group <- seq_len(x_len)
        ans_ranges <- new2("GroupedIRanges", start=ans_ranges_start,
                                             width=ans_ranges_width,
                                             group=ans_ranges_group,
                                             check=FALSE)
    }
    ans <- new2(classname, pool=ans_pool, ranges=ans_ranges, check=FALSE)
    ans <- .dropDuplicatedPoolElts(ans)
    if (!is.null(x_names))
        names(ans) <- x_names
    ans
}

### Produces an XVectorList object of the given length with empty elements.
XVectorList <- function(classname, length=0L)
{
    elt0 <- new(elementType(new(classname)))
    ans1_pool <- as(elt0@shared, "SharedVector_Pool")
    ans1_ranges <- new("GroupedIRanges", IRanges(start=1L, width=0L), group=1L)
    ans1 <- new2(classname, pool=ans1_pool, ranges=ans1_ranges, check=FALSE)
    rep.int(ans1, length)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from XVector to XVectorList with extractList() and family.
###

setMethod("relistToClass", "XVector",
    function(x) paste0(class(x), "List")
)

### Takes one XVector object ('x') and a Ranges object ('i') defining
### 1-based ranges on 'x' (conceptually equivalent to defining views on
### subject 'x').
.unsafe.extractList <- function(x, i)
{
    ans_class <- relistToClass(x)
    ans_pool <- as(x@shared, "SharedVector_Pool")
    if (!is(i, "IRanges"))
        i <- as(i, "IRanges")
    ranges_group <- rep.int(1L, length(i))
    ans_ranges <- new2("GroupedIRanges",
                       shift(i, x@offset),
                       group=ranges_group, check=FALSE)
    new2(ans_class, pool=ans_pool, ranges=ans_ranges, check=FALSE)
}

### Does not copy the sequence data!
setMethod("relist", c("XVector", "PartitioningByEnd"),
    function(flesh, skeleton)
    {
        skeleton_len <- length(skeleton)
        if (skeleton_len == 0L) {
            flesh_len2 <- 0L
        } else {
            flesh_len2 <- end(skeleton)[skeleton_len]
        }
        if (length(flesh) != flesh_len2)
            stop("shape of 'skeleton' is not compatible with 'length(flesh)'")
        .unsafe.extractList(flesh, skeleton)
    }
)

### Does not copy the sequence data!
setMethod("extractList", c("XVector", "Ranges"),
    function(x, i)
    {
        if (length(i) != 0L
         && (min(start(i)) < 1L || max(end(i)) > length(x)))
            stop("some ranges are out of bounds")
        .unsafe.extractList(x, i)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList subsetting.
###

### TODO: Make this a "getListElement" method for XVectorList objects.
XVectorList.getElement <- function(x, i)
{
    ans_class <- elementType(x)
    ans_shared <- x@pool[[x@ranges@group[i]]]
    ans_offset <- x@ranges@start[i] - 1L
    ans_length <- x@ranges@width[i]
    ans <- new2(ans_class,
                shared=ans_shared,
                offset=ans_offset,
                length=ans_length,
                check=FALSE)
    return(ans)
}

setMethod("[[", "XVectorList",
    function(x, i, j, ..., exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x)
        XVectorList.getElement(x, i)
    }
)

setMethod("extractROWS", "XVectorList",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ans_ranges <- extractROWS(x@ranges, i)
        ans_mcols <- extractROWS(x@elementMetadata, i)
        ans <- BiocGenerics:::replaceSlots(x, ranges=ans_ranges,
                                              mcols=ans_mcols,
                                              check=FALSE)
        ## Drop unused pool elements.
        .dropUnusedPoolElts(ans)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subseq()
###

setMethod("subseq", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
        narrow(x, start=start, end=end, width=width)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###
### The "c" method for XVectorList objects is implemented to behave like an
### endomorphism i.e. to return an object of the same class as 'x'. In
### particular 'c(x)' returns 'x' and not 'as(x, "XVectorList")'.
### It's easy to implement specific "c" methods for XVectorList subclasses.
### Typically they just need to do something like:
###
###     old_val <- S4Vectors:::disableValidity()
###     on.exit(S4Vectors:::disableValidity(old_val))
###     S4Vectors:::disableValidity(TRUE)
###     ans <- callNextMethod(x, ..., recursive=FALSE)
###     ...
###
### and to take care of the additional slots (aka the subclass-specific
### slots). If there aren't any additional slots (e.g. XRawList), or if the
### additional slots don't need to be modified, then no need to implement a
### specific method at all.
###

### 'Class' must be the name of a concrete subclass that extends XVectorList.
### Returns an instance of class 'Class'.
unlist_list_of_XVectorList <- function(Class, x,
                                       use.names=TRUE, ignore.mcols=FALSE)
{
    if (!isSingleString(Class))
        stop("'Class' must be a single character string")
    if (!extends(Class, "XVectorList"))
        stop("'Class' must be the name of a class that extends XVectorList")
    if (!is.list(x))
        stop("'x' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ### TODO: Support 'use.names=TRUE'.
    if (use.names)
        stop("'use.names=TRUE' is not supported yet")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")

    ## TODO: Implement (in C) fast elementIsNull(x), that does
    ## 'sapply(x, is.null, USE.NAMES=FALSE)' on list 'x', and use it here.
    null_idx <- which(sapply(x, is.null, USE.NAMES=FALSE))
    if (length(null_idx) != 0L)
        x <- x[-null_idx]
    if (length(x) == 0L)
        return(new(Class))
    ## TODO: Implement (in C) fast elementIs(x, class), that does
    ## 'sapply(x, is, class, USE.NAMES=FALSE)' on list 'x', and use it here.
    ## 'elementIs(x, "NULL")' should work and be equivalent to
    ## 'elementIsNull(x)'.
    if (!all(sapply(x, is, Class, USE.NAMES=FALSE)))
        stop("all elements in 'x' must be ", Class, " objects (or NULLs)")
    x_names <- names(x)
    names(x) <- NULL  # so lapply(x, ...) below returns an unnamed list

    ## Combine "pool" slots.
    pool_slots <- lapply(x, function(xi) xi@pool)
    ## TODO: Implement unlist_list_of_SharedRaw_Pool() and use it here.
    ans_pool <- do.call(c, pool_slots)

    ## Combine "ranges" slots.
    ranges_slots <- lapply(x, function(xi) xi@ranges)
    ## TODO: Implement unlist_list_of_GroupedRanges() (that takes an 'offsets'
    ## arg) and use it here.
    ans_ranges <- do.call(c, ranges_slots)

    breakpoints <- cumsum(elementNROWS(pool_slots))
    offsets <- c(0L, breakpoints[-length(breakpoints)])
    offsets <- rep.int(offsets, elementNROWS(ranges_slots))
    ans_ranges@group <- ans_ranges@group + offsets

    ## Combine "mcols" slots.
    ans_mcols <- do.call(S4Vectors:::rbind_mcols, x)

    ## Make 'ans' and return it.
    ans <- new(Class, pool=ans_pool,
                      ranges=ans_ranges,
                      elementMetadata=ans_mcols)
    .dropDuplicatedPoolElts(ans)
}

setMethod("c", "XVectorList",
    function(x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for XVectorList objects ",
                 "does not support the 'recursive' argument")
        if (missing(x)) {
            args <- list(...)
            x <- args[[1L]]
        } else {
            args <- list(x, ...)
        }
        unlist_list_of_XVectorList(class(x), args,
                                   use.names=FALSE, ignore.mcols=ignore.mcols)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Replacement methods.
###

setReplaceMethod("[[", "XVectorList",
    function(x, i, j, ..., value)
    {
        i <- normalizeDoubleBracketSubscript(i, x)
        if (!is(value, elementType(x)))
            stop("supplied replacement value must be a ",
                 elementType(x), " object")
        x[i] <- as(value, class(x))
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

setMethod("endoapply", "XVectorList",
    function(X, FUN, ...)
    {
        Xconstructor <- get(class(X))
        ## If there is no constructor for 'class(X)' or if the constructor
        ## doesn't work on a list (here we try on an empty list), then we
        ## call the default method i.e. the method for List objects. This will
        ## be much slower but still better than failing.
        if (!is.function(Xconstructor) ||
            inherits(try(Xconstructor(list()), silent=TRUE), "try-error"))
            return(callNextMethod())
        Xconstructor(lapply(X, FUN, ...))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show method for data column.
###

setMethod("showAsCell", "XVectorList", function(object) as.character(object))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unsplit_list_of_XVectorList()
###
### Not intended for the end user.
###
### 'f' must be a factor with number of levels equal to 'length(x)' and
### length equal to 'sum(elementNROWS(x))'. 
unsplit_list_of_XVectorList <- function(classname, x, f)
{
    ans <- XVectorList(classname, length(f))
    unlisted_x <- do.call(c, unname(x))
    idx <- unname(split(seq_len(length(f)), f))
    ans[unlist(idx)] <- unlisted_x
    ans
}

