### =========================================================================
### Intra-range methods
### -------------------------------------------------------------------------
###


### The default "narrow" method calls windows() so we only need to implement
### a "windows" method for XVectorList objects to make narrow() work on these
### objects.
setMethod("windows", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
    {
        x@ranges <- windows(x@ranges, start=start, end=end, width=width)
        x
    }
)

setMethod("threebands", "XVectorList",
    function(x, start=NA, end=NA, width=NA)
    {
        threeranges <- threebands(x@ranges, start=start, end=end, width=width)
        left <- right <- x
        left@ranges <- threeranges$left
        x@ranges <- threeranges$middle
        right@ranges <- threeranges$right
        list(left=left, middle=x, right=right)
    }
)

