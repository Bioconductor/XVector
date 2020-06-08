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
### parallel_slot_names()
###

### Combine the new "parallel slots" with those of the parent class. Make
### sure to put the new parallel slots **first**. See R/Vector-class.R file
### in the S4Vectors package for what slots should or should not be considered
### "parallel".

### Ugly workaround a serious callNextMethod inefficiency reported here:
###   https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16974
.GroupedIRanges_parallel_slot_names <-
    c("group", parallel_slot_names(new("IRanges")))

setMethod("parallel_slot_names", "GroupedIRanges",
    #function(x) c("group", callNextMethod())
    function(x) .GroupedIRanges_parallel_slot_names
)

setMethod("parallel_slot_names", "XVectorList",
    function(x) c("ranges", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GroupedIRanges methods
###

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList accessors
###

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
### clean and tidy
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
### XVectorList constructors
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
### Going from XVector to XVectorList with extractList() and family
###

setMethod("relistToClass", "XVector",
    function(x) paste0(class(x), "List")
)

### Takes one XVector object ('x') and a IntegerRanges object ('i') defining
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
setMethod("extractList", c("XVector", "IntegerRanges"),
    function(x, i)
    {
        if (length(i) != 0L
         && (min(start(i)) < 1L || max(end(i)) > length(x)))
            stop("some ranges are out of bounds")
        .unsafe.extractList(x, i)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### XVectorList subsetting
###

.getListElement_XVectorList <- function(x, i, exact=TRUE)
{
    i2 <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                          allow.NA=TRUE,
                                          allow.nomatch=TRUE)
    if (is.na(i2))
        return(NULL)
    ans_class <- elementType(x)
    ans_shared <- x@pool[[x@ranges@group[i2]]]
    ans_offset <- x@ranges@start[i2] - 1L
    ans_length <- x@ranges@width[i2]
    new2(ans_class, shared=ans_shared,
                    offset=ans_offset,
                    length=ans_length,
                    check=FALSE)
}

setMethod("getListElement", "XVectorList", .getListElement_XVectorList)

### Drop unused pool elements.
setMethod("extractROWS", "XVectorList",
    function(x, i) .dropUnusedPoolElts(callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subseq()
###

setMethod("subseq", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
        narrow(x, start=start, end=end, width=width)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.concatenate_XVectorList_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- S4Vectors:::prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    ## 1. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel slots
    ## (i.e. "ranges" and "elementMetadata" in the case of XVectorList) and
    ## stick them into 'ans'. Note that the resulting 'ans' can be an invalid
    ## object so we use 'check=FALSE' to skip validation.
    ans <- callNextMethod(x, objects, use.names=use.names,
                                      ignore.mcols=ignore.mcols,
                                      check=FALSE)

    ## 2. Take care of the non-parallel slots

    ## Concatenate the "pool" slots.
    pool_list <- lapply(all_objects, slot, "pool")
    ans_pool <- do.call(c, pool_list)

    ## 3. Fix parallel slot "ranges"

    ans_ranges <- ans@ranges
    breakpoints <- cumsum(lengths(pool_list))
    offsets <- c(0L, breakpoints[-length(breakpoints)])
    offsets <- rep.int(offsets, lengths(all_objects))
    ans_ranges@group <- ans_ranges@group + offsets
    if (!(use.names || is.null(names(ans_ranges))))
        names(ans_ranges) <- NULL

    ans <- BiocGenerics:::replaceSlots(ans, pool=ans_pool,
                                            ranges=ans_ranges,
                                            check=check)
    .dropDuplicatedPoolElts(ans)
}

setMethod("bindROWS", "XVectorList", .concatenate_XVectorList_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show method for data column
###

setMethod("showAsCell", "XVectorList", function(object) as.character(object))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unsplit_list_of_XVectorList()
###
### Not intended for the end user.
###
### 'f' must be a factor with number of levels equal to 'length(x)' and
### length equal to 'sum(lengths(x))'.
unsplit_list_of_XVectorList <- function(classname, x, f)
{
    ans <- XVectorList(classname, length(f))
    unlisted_x <- do.call(c, unname(x))
    idx <- unname(split(seq_len(length(f)), f))
    ans[unlist(idx)] <- unlisted_x
    ans
}

