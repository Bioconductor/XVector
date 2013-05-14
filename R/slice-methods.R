### =========================================================================
### Slice the bread
### -------------------------------------------------------------------------


setMethod("slice", "integer",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)
        slice(as(x, "XInteger"), lower=lower, upper=upper)
)

setMethod("slice", "XInteger",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)
    {
        if (!isSingleNumber(lower))
            stop("'lower' must be a single integer")
        if (!is.integer(lower))
            lower <- as.integer(lower)
        if (!isSingleNumber(upper))
            stop("'upper' must be a single integer")
        if (!is.integer(upper))
            upper <- as.integer(upper)
        ranges <- .Call2("XInteger_slice", x, lower, upper,
                        PACKAGE="XVector")
        Views(x, ranges)
    }
)

setMethod("slice", "numeric",
    function(x, lower=-Inf, upper=Inf,
             includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
        slice(as(x, "XDouble"), lower=lower, upper=upper,
              includeLower=includeLower, includeUpper=includeUpper,
              rangesOnly=rangesOnly)
)

setMethod("slice", "XDouble",
    function(x, lower=-.Machine$double.xmax, upper=.Machine$double.xmax,
             includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
    {
        if (!isSingleNumber(lower))
            stop("'lower' must be a single integer")
        if (!is.numeric(lower))
            lower <- as.numeric(lower)
        if (!isSingleNumber(upper))
            stop("'upper' must be a single integer")
        if (!is.numeric(upper))
            upper <- as.numeric(upper)
        if (!isTRUEorFALSE(includeLower))
            stop("'includeLower' must be TRUE or FALSE")
        if (!isTRUEorFALSE(includeUpper))
            stop("'includeUpper' must be TRUE or FALSE")
        if (!isTRUEorFALSE(rangesOnly))
            stop("'rangesOnly' must be TRUE or FALSE")

        ranges <- .Call2("XDouble_slice", x, lower, upper, includeLower,
                        includeUpper, PACKAGE="XVector")
        if (rangesOnly) {
            ranges
        } else {
            Views(x, ranges)
        }
    }
)

