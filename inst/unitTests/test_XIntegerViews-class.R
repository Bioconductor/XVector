## TODO: Add more tests.

test_XIntegerViews_viewApply <- function() {
    x <- Views(c(4L, NA, NA, 2:5),
               start=1:7, end=c(2:6, 6, 6), names=LETTERS[1:7])

    target <- viewSums(x)
    current1 <- viewApply(x, sum)
    checkIdentical(target, current1)
    current2 <- suppressWarnings(viewApply(x, sum, simplify=FALSE))
    checkTrue(is(current2, "CompressedIntegerList"))
    checkIdentical(unlist(current2), current1)

    target <- viewMeans(x)
    current1 <- viewApply(x, mean)
    checkEqualsNumeric(target, current1)
    current2 <- suppressWarnings(viewApply(x, mean, simplify=FALSE))
    checkTrue(is(current2, "CompressedNumericList"))
    checkIdentical(unlist(current2), current1)
}

