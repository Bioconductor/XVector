### =========================================================================
### Intra-range methods
### -------------------------------------------------------------------------
###


setMethod("narrow", "XVectorList",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        x@ranges <- narrow(x@ranges, start=start, end=end, width=width,
                                     use.names=use.names)
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

