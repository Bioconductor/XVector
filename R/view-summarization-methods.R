### =========================================================================
### Summarize views on an XInteger or XDouble object
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### viewMins(), viewMaxs(), viewSums(), and viewMeans() methods
###

.XIntegerViews_summary1 <- function(x, na.rm, method)
    setNames(.Call2("XIntegerViews_summary1", x, na.rm, method,
                    PACKAGE="XVector"), names(x))

.XDoubleViews_summary1 <- function(x, na.rm, method)
    setNames(.Call2("XDoubleViews_summary1", x, na.rm, method,
                    PACKAGE="XVector"), names(x))

setMethod("viewMins", "XIntegerViews",
    function(x, na.rm=FALSE) .XIntegerViews_summary1(x, na.rm, "viewMins")
)

setMethod("viewMins", "XDoubleViews",
    function(x, na.rm=FALSE) .XDoubleViews_summary1(x, na.rm, "viewMins")
)

setMethod("viewMaxs", "XIntegerViews",
    function(x, na.rm=FALSE) .XIntegerViews_summary1(x, na.rm, "viewMaxs")
)

setMethod("viewMaxs", "XDoubleViews",
    function(x, na.rm=FALSE) .XDoubleViews_summary1(x, na.rm, "viewMaxs")
)

setMethod("viewSums", "XIntegerViews",
    function(x, na.rm=FALSE) .XIntegerViews_summary1(x, na.rm, "viewSums")
)

setMethod("viewSums", "XDoubleViews",
    function(x, na.rm=FALSE) .XDoubleViews_summary1(x, na.rm, "viewSums")
)

### No native implementation for XIntegerViews yet so we rely on the
### mean() method for CompressedIntegerList for now.
setMethod("viewMeans", "XIntegerViews",
    function(x, na.rm=FALSE)
        setNames(mean(as(x, "CompressedIntegerList"), na.rm=na.rm), names(x))
)

### No native implementation for XDoubleViews yet so we rely on the
### mean() method for CompressedNumericList for now.
setMethod("viewMeans", "XDoubleViews",
    function(x, na.rm=FALSE)
        setNames(mean(as(x, "CompressedNumericList"), na.rm=na.rm), names(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### viewWhichMins() and viewWhichMaxs() methods
###

.XIntegerViews_summary2 <- function(x, na.rm, method)
    setNames(.Call2("XIntegerViews_summary2", x, na.rm, method,
                    PACKAGE="XVector"), names(x))

.XDoubleViews_summary2 <- function(x, na.rm, method)
    setNames(.Call2("XDoubleViews_summary2", x, na.rm, method,
                    PACKAGE="XVector"), names(x))

setMethod("viewWhichMins", "XIntegerViews",
    function(x, na.rm=FALSE) .XIntegerViews_summary2(x, na.rm, "viewWhichMins")
)

setMethod("viewWhichMins", "XDoubleViews",
    function(x, na.rm=FALSE) .XDoubleViews_summary2(x, na.rm, "viewWhichMins")
)

setMethod("viewWhichMaxs", "XIntegerViews",
    function(x, na.rm=FALSE) .XIntegerViews_summary2(x, na.rm, "viewWhichMaxs")
)

setMethod("viewWhichMaxs", "XDoubleViews",
    function(x, na.rm=FALSE) .XDoubleViews_summary2(x, na.rm, "viewWhichMaxs")
)

