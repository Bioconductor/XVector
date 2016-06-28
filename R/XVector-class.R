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
### Getters.
###

setMethod("length", "XVector", function(x) x@length)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

### Should work as an endomorphism (e.g. will return a DNAString instance if
### 'x' is a DNAString instance).
setMethod("c", "XVector",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for XVector objects ",
                 "does not support the 'recursive' argument")
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        ## Remove NULL elements by setting them to NULL!
        if (any(arg_is_null))
            args[arg_is_null] <- NULL
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ",
                 class(x), " objects (or NULLs)")
        ans_length <- sum(sapply(args, length))
        ans_shared <- SharedVector(class(x@shared), length=ans_length)
        dest_offset <- 0L
        for (arg in args) {
            width <- length(arg)
            if (width == 0L)  # will be TRUE on NULLs too...
                next
            ## ... so from here 'arg' is guaranteed to be an XVector object.
            src <- arg@shared
            src_start <- arg@offset + 1L
            SharedVector.mcopy(ans_shared, dest_offset, src, src_start, width)
            dest_offset <- dest_offset + width
        }
        ans <- new2(class(x), length=ans_length, check=FALSE)
        ans@shared <- ans_shared
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
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
### Coercion.
###

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "XVector",
    function(x, ...) as.numeric(as.integer(x))
)

setAs("XVector", "Rle", function(from) {
  Rle(as.vector(from))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
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
### Equality.
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

