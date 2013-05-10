\name{view-summarization-methods}
\alias{view-summarization-methods}

\alias{slice}
\alias{slice,integer-method}
\alias{slice,XInteger-method}
\alias{slice,numeric-method}
\alias{slice,XDouble-method}

\alias{viewMins,XIntegerViews-method}
\alias{viewMins,XDoubleViews-method}
\alias{viewMaxs,XIntegerViews-method}
\alias{viewMaxs,XDoubleViews-method}
\alias{viewSums,XIntegerViews-method}
\alias{viewSums,XDoubleViews-method}
\alias{viewMeans,XIntegerViews-method}
\alias{viewMeans,XDoubleViews-method}
\alias{viewWhichMins,XIntegerViews-method}
\alias{viewWhichMins,XDoubleViews-method}
\alias{viewWhichMaxs,XIntegerViews-method}
\alias{viewWhichMaxs,XDoubleViews-method}


\title{Summarize views on an XInteger or XDouble object}

\description{
  The \code{\link[IRanges]{slice}} methods for \link{XInteger} and
  \link{XDouble} objects create views corresponding to the indices where
  the data are within the specified bounds. The views are returned in a
  \link{XIntegerViews} or \link{XDoubleViews} object.

  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, \code{viewMeans}
  functions calculate the minima, maxima, sums, and means on views,
  respectively.
}

\usage{
## "slice" methods:
## ----------------

\S4method{slice}{integer}(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)

\S4method{slice}{XInteger}(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)

\S4method{slice}{numeric}(x, lower=-Inf, upper=Inf,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)

\S4method{slice}{double}(x, lower=-.Machine$double.xmax, upper=.Machine$double.xmax,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)


## "viewMins" methods:
## -------------------

\S4method{viewMins}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewMins}{XDoubleViews}(x, na.rm=FALSE)


## "viewMaxs" methods:
## -------------------

\S4method{viewMaxs}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewMaxs}{XDoubleViews}(x, na.rm=FALSE)


## "viewSums" methods:
## -------------------

\S4method{viewSums}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewSums}{XDoubleViews}(x, na.rm=FALSE)


## "viewMeans" methods:
## --------------------

\S4method{viewMeans}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewMeans}{XDoubleViews}(x, na.rm=FALSE)


## "viewWhichMins" methods:
## ------------------------

\S4method{viewWhichMins}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewWhichMins}{XDoubleViews}(x, na.rm=FALSE)


## "viewWhichMaxs" methods:
## ------------------------

\S4method{viewWhichMaxs}{XIntegerViews}(x, na.rm=FALSE)

\S4method{viewWhichMaxs}{XDoubleViews}(x, na.rm=FALSE)
}

\arguments{
  \item{x}{
    An integer vector, or \link{XInteger} object, or numeric vector,
    or \link{XDouble} object for \code{slice}.

    An \link{XIntegerViews} or \link{XDoubleViews} object for any of the
    \code{view*} functions.
  }
  \item{lower, upper}{
    The lower and upper bounds for the slice.
  }
  \item{includeLower, includeUpper}{
    Logical indicating whether or not the specified boundary is open or closed.
  }
  \item{rangesOnly}{
    A logical indicating whether or not to drop the original data from the
    output.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\value{
  For \code{slice}: An \link{XIntegerViews} or \link{XDoubleViews} object
  if \code{rangesOnly=FALSE}. An \link[IRanges]{IRanges} object if
  \code{rangesOnly=TRUE}.

  For the \code{view*} functions: A numeric vector of the length of \code{x}.
}

\note{
  For convenience, methods for \code{min}, \code{max}, \code{sum},
  \code{mean}, \code{which.min} and \code{which.max} are provided as
  wrappers around the corresponding \code{view*} functions (which might
  be deprecated at some point).
}

\author{P. Aboyoun}

\seealso{
  \link[IRanges]{view-summarization-methods},
  \link{XInteger-class}, \link{XIntegerViews-class},
  \link{XDouble-class}, \link{XDoubleViews-class}
}

\examples{
vec <- as.integer(c(19, 5, 0, 8, 5))
slice(vec, lower=5, upper=8)

set.seed(0)
vec <- sample(24)
vec_views <- slice(vec, lower=4, upper=16)
vec_views

viewApply(vec_views, function(x) diff(as.integer(x)))

viewMins(vec_views)
viewMaxs(vec_views)

viewSums(vec_views)
viewMeans(vec_views)

viewWhichMins(vec_views)
viewWhichMaxs(vec_views)
}

\keyword{methods}
\keyword{arith}
