### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an "external raw vector"
### i.e. a *single* view on a SharedRaw object.
###
### IMPORTANT NOTE: Our concept/representation/implementation of "external
### vector" in general differ significantly from those found in the
### externalVector package!
###

setClass("XRaw",
    contains="XVector",
    representation(
        shared="SharedRaw"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

XRaw <- function(length=0L, val=NULL)
{
    if (!isSingleNumber(length) || length < 0L)
        stop("'length' must be a single non-negative integer")
    if (!is.integer(length))
        length <- as.integer(length)
    new2("XRaw", shared=SharedRaw(length=length, val=val), length=length,
         check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_character_from_XRaw_by_positions() and
### extract_character_from_XRaw_by_ranges()
###

### Typical use:
###   x <- subseq(as(charToRaw("--Hello--"), "XRaw"), 3, 7)
###   extract_character_from_XRaw_by_positions(x, 5:2)
###   extract_character_from_XRaw_by_positions(x, 5:2, collapse=TRUE)
###   lkup <- S4Vectors:::TOUPPER_LOOKUP
###   extract_character_from_XRaw_by_positions(x, 5:2, lkup=lkup)
###   extract_character_from_XRaw_by_positions(x, 5:2, collapse=TRUE, lkup=lkup)
extract_character_from_XRaw_by_positions <- function(x, pos,
                                                     collapse=FALSE,
                                                     lkup=NULL)
{
    .Call("C_extract_character_from_XRaw_by_positions",
          x, pos, collapse, lkup,
          PACKAGE="XVector")
}

### Typical use:
###   x <- subseq(as(charToRaw("--Hello--"), "XRaw"), 3, 7)
###   extract_character_from_XRaw_by_ranges(x, 3:1, c(2:1, 4L))
###   extract_character_from_XRaw_by_ranges(x, 3:1, c(2:1, 4L), collapse=TRUE)
###   lkup <- S4Vectors:::TOUPPER_LOOKUP
###   extract_character_from_XRaw_by_ranges(x, 3:1, c(2:1, 4L), lkup=lkup)
###   extract_character_from_XRaw_by_ranges(x, 3:1, c(2:1, 4L), collapse=TRUE,
###                                         lkup=lkup)
extract_character_from_XRaw_by_ranges <- function(x, start, width,
                                                  collapse=FALSE, lkup=NULL)
{
    .Call("C_extract_character_from_XRaw_by_ranges",
          x, start, width, collapse, lkup,
          PACKAGE="XVector")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From standard vectors to XRaw objects:
setAs("raw", "XRaw", function(from) XRaw(length(from), val=from))
setAs("raw", "XVector", function(from) as(from, "XRaw"))
setAs("numeric", "XRaw", function(from) XRaw(length(from), val=from))

### From XRaw objects to standard vectors:
### TODO: Modify SharedRaw.read() so it returns a raw vector instead of a
### character string, and use it here.
setMethod("as.raw", "XRaw", function(x) as.raw(as.integer(x)))
setMethod("as.integer", "XRaw",
    function(x, ...) SharedRaw.readInts(x@shared, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", "XRaw",
    function(x, mode="any")
    { 
        if (!identical(mode, "any"))
            stop("\"as.vector\" method for XRaw objects ",
                 "does not support the 'mode' argument")
        as.raw(x)
    }
)

