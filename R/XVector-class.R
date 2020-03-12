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
        mcols(x) <- extractROWS(mcols(x, use.names=FALSE), solved_SEW)
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
### Concatenation
###

.concatenate_XVector_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- S4Vectors:::prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    ans_len <- suppressWarnings(sum(lengths(all_objects)))
    if (is.na(ans_len))
        stop("too many vector elements to concatenate")

    ## 1. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "elementMetadata" in the case of XVector) and stick them
    ## into 'ans'. Note that the resulting 'ans' can be an invalid object
    ## because its "elementMetadata" slot can be longer (i.e. have more rows)
    ## than 'ans' itself so we use 'check=FALSE' to skip validation.
    ans <- callNextMethod(x, objects, use.names=use.names,
                                      ignore.mcols=ignore.mcols,
                                      check=FALSE)

    ## 2. Take care of the non-parallel slots

    ans_shared <- SharedVector(class(x@shared), length=ans_len)
    dest_offset <- 0L
    for (object in all_objects) {
        object_len <- length(object)
        if (object_len == 0L)  # would be TRUE on NULLs too...
            next
        ## From here 'object' is guaranteed to be an XVector object.
        src <- object@shared
        src_start <- object@offset + 1L
        SharedVector.mcopy(ans_shared, dest_offset, src, src_start, object_len)
        dest_offset <- dest_offset + object_len
    }

    BiocGenerics:::replaceSlots(ans, shared=ans_shared,
                                     offset=0L,
                                     length=ans_len,
                                     check=check)
}

setMethod("bindROWS", "XVector", .concatenate_XVector_objects)


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

