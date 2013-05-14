\name{view-summarization-methods}

\alias{view-summarization-methods}

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
  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, and \code{viewMeans}
  methods described here calculate respectively the minima, maxima, sums,
  and means of the views in an \link{XIntegerViews} or \link{XDoubleViews}
  object.
}

\usage{
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
    An \link{XIntegerViews} or \link{XDoubleViews} object.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\value{
  A numeric vector of the length of \code{x}.
}

\note{
  For convenience, methods for \code{min}, \code{max}, \code{sum},
  \code{mean}, \code{which.min} and \code{which.max} are provided as
  wrappers around the corresponding \code{view*} functions (which might
  be deprecated at some point).
}

\author{P. Aboyoun}

\seealso{
  \itemize{
    \item \link{slice-methods} for slicing an \link{XInteger} or
          \link{XDouble} object.

    \item \link[IRanges]{view-summarization-methods} in the IRanges package
          for the view summarization generics.

    \item The \link{XIntegerViews} and \link{XDoubleViews} classes.
  }
}

\examples{
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
