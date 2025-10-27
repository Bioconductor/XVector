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

