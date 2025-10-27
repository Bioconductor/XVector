## TODO: Add more tests.

test_XDoubleViews_equality <- function() {
    x <- rnorm(100)
    bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
    bounds2 <- IRanges(c(10, 30, 50, 80), width=c(5, 8, 15, 18))
    v <- Views(x, bounds)
    v2 <- Views(x, bounds2)
  
    checkTrue(all(v == v))
    checkTrue(all((v != v2) == c(TRUE, TRUE, FALSE, FALSE)))
}

test_XDoubleViews_viewApply <- function() {
    x1 <- Views(c(4.2, NA, NA, 2:4, Inf, -pi, NaN),
                start=c(1:8, 8), end=c(2:8, 8, 7), names=LETTERS[1:9])
    x2 <- Views(rnorm(100), IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18)))
    for (x in list(x1, x2)) {
        target <- viewMins(x)
        current1 <- suppressWarnings(viewApply(x, min))
        checkEqualsNumeric(target, current1)
        current2 <- suppressWarnings(viewApply(x, min, simplify=FALSE))
        checkTrue(is(current2, "CompressedNumericList"))
        checkIdentical(unlist(current2), current1)

        target <- viewMaxs(x)
        current1 <- suppressWarnings(viewApply(x, max))
        checkEqualsNumeric(target, current1)
        current2 <- suppressWarnings(viewApply(x, max, simplify=FALSE))
        checkTrue(is(current2, "CompressedNumericList"))
        checkIdentical(unlist(current2), current1)

        target <- viewSums(x)
        current1 <- viewApply(x, sum)
        checkEqualsNumeric(target, current1)
        current2 <- suppressWarnings(viewApply(x, sum, simplify=FALSE))
        checkTrue(is(current2, "CompressedNumericList"))
        checkIdentical(unlist(current2), current1)

        target <- viewMeans(x)
        current1 <- viewApply(x, mean)
        checkEqualsNumeric(target, current1)
        current2 <- suppressWarnings(viewApply(x, mean, simplify=FALSE))
        checkTrue(is(current2, "CompressedNumericList"))
        checkIdentical(unlist(current2), current1)
    }
}

