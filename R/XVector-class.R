### =========================================================================
### XVector objects
### -------------------------------------------------------------------------
###
### The XVector virtual class is a general container for storing
### an "external vector" i.e. a *single* view on a SharedVector object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of "external
### vector" differ significantly from those found in the externalVector
### package!
###

setClass("XVector",
    contains="Vector",
    representation(
        "VIRTUAL",
        shared="SharedVector",
        offset="integer",  # a single integer
        length="integer"   # a single integer
    ),
    prototype(
        offset=0L,
        length=0L
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("length", "XVector", function(x) x@length)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.concatenate_XVector_objects <-
    function(.Object, objects, use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")

    objects <- unname(S4Vectors:::delete_NULLs(objects))
    S4Vectors:::check_class_of_objects_to_concatenate(.Object, objects)

    if (length(objects) == 0L) {
        if (length(.Object) != 0L)
            .Object <- .Object[integer(0)]
        return(.Object)
    }

    ans_len <- suppressWarnings(sum(lengths(objects)))
    if (is.na(ans_len))
        stop("too many vector elements to concatenate")

    ans_shared <- SharedVector(class(.Object@shared), length=ans_len)
    dest_offset <- 0L
    for (object in objects) {
        width <- length(object)
        if (width == 0L)  # would be TRUE on NULLs too...
            next
        ## From here 'object' is guaranteed to be an XVector object.
        src <- object@shared
        src_start <- object@offset + 1L
        SharedVector.mcopy(ans_shared, dest_offset, src, src_start, width)
        dest_offset <- dest_offset + width
    }

    .Object <- new2(class(.Object), shared=ans_shared,
                                    length=ans_len,
                                    check=FALSE)

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "elementMetadata" in the case of XVector) and stick
    ## them into '.Object'.
    callNextMethod()
}

setMethod("concatenateObjects", "XVector", .concatenate_XVector_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("extractROWS", "XVector",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x)
        new_shared <- SharedVector(class(x@shared), length=length(i))
        SharedVector.copy(new_shared, x@offset + i, src=x@shared)
        x@shared <- new_shared
        x@offset <- 0L
        x@length <- length(new_shared)
        x@elementMetadata <- extractROWS(x@elementMetadata, i)
        x
    }
)

### Extracts a linear subsequence without copying the sequence data!
setGeneric("subseq", signature="x",
    function(x, start=NA, end=NA, width=NA) standardGeneric("subseq")
)

### Replace a linear subsequence.
setGeneric("subseq<-", signature="x",
    function(x, start=NA, end=NA, width=NA, value) standardGeneric("subseq<-")
)

setMethod("subseq", "XVector",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- IRanges:::solveUserSEWForSingleSeq(length(x),
                                                         start, end, width)
        x@offset <- x@offset + start(solved_SEW) - 1L
        x@length <- width(solved_SEW)
        mcols(x) <- extractROWS(mcols(x), solved_SEW)
        x
    }
)

setReplaceMethod("subseq", "XVector",
    function(x, start=NA, end=NA, width=NA, value)
    {
        solved_SEW <- IRanges:::solveUserSEWForSingleSeq(length(x),
                                                         start, end, width)
        if (!is.null(value)) {
            if (!is(value, class(x)))
                stop("'value' must be a ", class(x), " object or NULL")
        }
        c(subseq(x, end=start(solved_SEW)-1L),
          value,
          subseq(x, start=end(solved_SEW)+1L))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "XVector",
    function(x, ...) as.numeric(as.integer(x))
)

setAs("XVector", "Rle", function(from) {
  Rle(as.vector(from))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method
###

setMethod("show", "XVector",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo != 0L)
            cat(" [1] ", S4Vectors:::toNumSnippet(object, getOption("width")-5), "\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Equality
###

.XVector.equal <- function(x, y)
{
    if (class(x) != class(y) || x@length != y@length)
        return(FALSE)
    ans <- !SharedVector.compare(x@shared, x@offset + 1L,
                                 y@shared, y@offset + 1L,
                                 x@length)
    as.logical(ans)
}

setMethod("==", signature(e1="XVector", e2="XVector"),
    function(e1, e2) .XVector.equal(e1, e2)
)

