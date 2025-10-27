## Like base::min() except:
## - returns NA instead of Inf when 'x' is integer;
## - suppress the warning when no non-missing values.
.min2 <- function(x, na.rm=FALSE)
{
    if (!is.integer(x))
        return(suppressWarnings(min(x, na.rm=na.rm)))
    if (na.rm)
        x <- x[!is.na(x)]
    if (length(x) == 0L) NA_integer_ else min(x)
}

## Like base::max() except:
## - returns NA instead of -Inf when 'x' is integer;
## - suppress the warning when no non-missing values.
.max2 <- function(x, na.rm=FALSE)
{
    if (!is.integer(x))
        return(suppressWarnings(max(x, na.rm=na.rm)))
    if (na.rm)
        x <- x[!is.na(x)]
    if (length(x) == 0L) NA_integer_ else max(x)
}

## Like base::which.min() and base::which.max() except that they
## return NA if (1) or (2):
##   (1) 'na.rm=FALSE' and 'x' contains NAs
##   (2) 'x' has no non-missing values.
.which.min2 <- function(x, na.rm=FALSE)
{
    if (!na.rm && anyNA(x))
        return(NA_integer_)
    ans <- which.min(x)
    if (length(ans) == 0L) NA_integer_ else ans
}
.which.max2 <- function(x, na.rm=FALSE)
{
    if (!na.rm && anyNA(x))
        return(NA_integer_)
    ans <- which.max(x)
    if (length(ans) == 0L) NA_integer_ else ans
}

.naive_view_summary <- function(FUN, x, na.rm=FALSE, res.type="integer")
{
    na.rm_is_missing <- missing(na.rm)
    vapply(x,
        function(xx) {
            if (na.rm_is_missing) {
                FUN(as.vector(xx))
            } else {
                FUN(as.vector(xx), na.rm=na.rm)
            }
        },
        FUN.VALUE=vector(res.type, length=1L))
}

test_XIntegerViews_summarization <- function() {
    x <- Views(c(4L, NA, NA, 2:5),
               start=1:7, end=c(2:6, 6, 6), names=LETTERS[1:7])

    for (na.rm in c(FALSE, TRUE)) {
        target <- .naive_view_summary(.min2, x, na.rm=na.rm)
        current <- viewMins(x, na.rm=na.rm)
        checkIdentical(target, current)
        target <- .naive_view_summary(.max2, x, na.rm=na.rm)
        current <- viewMaxs(x, na.rm=na.rm)
        checkIdentical(target, current)
        target <- .naive_view_summary(sum, x, na.rm=na.rm)
        current <- viewSums(x, na.rm=na.rm)
        checkIdentical(target, current)
        target <- .naive_view_summary(mean, x, na.rm=na.rm, res.type="double")
        current <- viewMeans(x, na.rm=na.rm)
        checkIdentical(target, current)
        ## Oh my... it looks like viewWhichMins() and viewWhichMaxs()
        ## return indices with respect to the subject. What a choice!
        offsets <- start(x) - 1L
        target <- .naive_view_summary(.which.min2, x, na.rm=na.rm)
        current <- viewWhichMins(x, na.rm=na.rm) - offsets
        checkIdentical(target, current)
        target <- .naive_view_summary(.which.max2, x, na.rm=na.rm)
        current <- viewWhichMaxs(x, na.rm=na.rm) - offsets
        checkIdentical(target, current)
    }
}

test_XDoubleViews_summarization <- function() {
    x1 <- Views(c(4.2, NA, NA, 2:4, Inf, -pi, NaN),
                start=c(1:8, 8), end=c(2:8, 8, 7), names=LETTERS[1:9])
    x2 <- Views(rnorm(100), IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18)))
    for (x in list(x1, x2)) {
        for (na.rm in c(FALSE, TRUE)) {
            target <- .naive_view_summary(.min2, x, na.rm=na.rm,
                                          res.type="double")
            current <- viewMins(x, na.rm=na.rm)
            checkEqualsNumeric(target, current)
            target <- .naive_view_summary(.max2, x, na.rm=na.rm,
                                          res.type="double")
            current <- viewMaxs(x, na.rm=na.rm)
            checkEqualsNumeric(target, current)
            target <- .naive_view_summary(sum, x, na.rm=na.rm,
                                          res.type="double")
            current <- viewSums(x, na.rm=na.rm)
            checkEqualsNumeric(target, current)
            target <- .naive_view_summary(mean, x, na.rm=na.rm,
                                          res.type="double")
            current <- viewMeans(x, na.rm=na.rm)
            checkEqualsNumeric(target, current)
            ## Oh my... it looks like viewWhichMins() and viewWhichMaxs()
            ## return indices with respect to the subject. What a choice!
            offsets <- start(x) - 1L
            target <- .naive_view_summary(.which.min2, x, na.rm=na.rm)
            current <- viewWhichMins(x, na.rm=na.rm) - offsets
            checkIdentical(target, current)
            target <- .naive_view_summary(.which.max2, x, na.rm=na.rm)
            current <- viewWhichMaxs(x, na.rm=na.rm) - offsets
            checkIdentical(target, current)
        }
    }
}

