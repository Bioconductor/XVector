\name{slice-methods}

\alias{slice-methods}

\alias{slice,integer-method}
\alias{slice,XInteger-method}
\alias{slice,numeric-method}
\alias{slice,XDouble-method}


\title{Slice an XInteger or XDouble object}

\description{
  The \code{\link[IRanges]{slice}} methods for \link{XInteger} and
  \link{XDouble} objects create views corresponding to the indices where
  the data are within the specified bounds. The views are returned in a
  \link{XIntegerViews} or \link{XDoubleViews} object.
}

\usage{
\S4method{slice}{integer}(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)

\S4method{slice}{XInteger}(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)

\S4method{slice}{numeric}(x, lower=-Inf, upper=Inf,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)

\S4method{slice}{XDouble}(x, lower=-.Machine$double.xmax, upper=.Machine$double.xmax,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
}

\arguments{
  \item{x}{
    An \link{XInteger} or \link{XDouble} object. Alternatively, it can also
    be an integer or numeric vector.
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
}

\value{
  An \link{XIntegerViews} or \link{XDoubleViews} object
  if \code{rangesOnly=FALSE}.

  An \link[IRanges]{IRanges} object if \code{rangesOnly=TRUE}.
}

\author{P. Aboyoun}

\seealso{
  \itemize{
    \item \link{view-summarization-methods} for summarizing the views
          returned by \code{slice}.

    \item \link[IRanges]{slice-methods} in the IRanges package
          for more \code{slice} methods.

    \item The \link{XInteger}, \link{XIntegerViews}, \link{XDouble}, and
          \link{XDoubleViews} classes.
  }
}

\examples{
vec <- as.integer(c(19, 5, 0, 8, 5))
slice(vec, lower=5, upper=8)

set.seed(0)
vec <- sample(24)
slice(vec, lower=4, upper=16)
}

\keyword{methods}
