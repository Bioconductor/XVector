### =========================================================================
### Summarize views on an XInteger or XDouble object
###


setMethod("viewMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewMins",
              PACKAGE="XVector")
)

setMethod("viewMins", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewMins",
              PACKAGE="XVector")
)

setMethod("viewMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewMaxs",
              PACKAGE="XVector")
)

setMethod("viewMaxs", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewMaxs",
              PACKAGE="XVector")
)

setMethod("viewSums", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewSums",
              PACKAGE="XVector")
)

setMethod("viewSums", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewSums",
              PACKAGE="XVector")
)

setMethod("viewMeans", "XIntegerViews",
    function(x, na.rm=FALSE) {
        if (!isTRUEorFALSE(na.rm))
            stop("'na.rm' must be TRUE or FALSE")
        if (na.rm) {
            n <-
              viewSums(Views(!is.na(Rle(as.integer(subject(x)))), as(x, "Rle")))
        } else {
            n <- width(x)
        }
        viewSums(x, na.rm = na.rm) / n
    }
)

setMethod("viewMeans", "XDoubleViews",
    function(x, na.rm=FALSE) {
        if (!isTRUEorFALSE(na.rm))
            stop("'na.rm' must be TRUE or FALSE")
        if (na.rm) {
            n <-
              viewSums(Views(!is.na(Rle(as.numeric(subject(x)))), as(x, "Rle")))
        } else {
            n <- width(x)
        }
        viewSums(x, na.rm = na.rm) / n
    }
)

setMethod("viewWhichMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary2", x, na.rm, "viewWhichMins",
              PACKAGE="XVector")
)

setMethod("viewWhichMins", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary2", x, na.rm, "viewWhichMins",
              PACKAGE="XVector")
)

setMethod("viewWhichMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary2", x, na.rm, "viewWhichMaxs",
              PACKAGE="XVector")
)

setMethod("viewWhichMaxs", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary2", x, na.rm, "viewWhichMaxs",
              PACKAGE="XVector")
)

