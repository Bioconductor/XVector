\name{reverse-methods}

\alias{reverse-methods}

\alias{reverse,SharedRaw-method}
\alias{reverse,SharedVector_Pool-method}
\alias{reverse,XVector-method}
\alias{rev,XVector-method}
\alias{reverse,XVectorList-method}


\title{Reverse an XVector or XVectorList object}

\description{
  Methods for reversing an \link{XVector} or \link{XVectorList} object.
}

\usage{
\S4method{reverse}{XVector}(x, ...)
\S4method{reverse}{XVectorList}(x, ...)
}

\arguments{
  \item{x}{
    An \link{XVector} or \link{XVectorList} object.
  }
  \item{...}{
    Additional arguments. Currently ignored.
  }
}

\details{
  On an \link{XVector} object, \code{reverse} and \code{\link[base]{rev}}
  are equivalent, i.e. they both reverse the order of their elements.

  On an \link{XVectorList} object, \code{reverse} reverses each element
  individually, without modifying the top-level order of the elements.
  It's equivalent to, but more efficient than, doing \code{endoapply(x, rev)}.
}

\value{
  An object of the same class and length as the original object.
}

\seealso{
  \link{XVector-class},
  \link{XVectorList-class},
  \code{\link[S4Vectors]{endoapply}},
  \code{\link[base]{rev}}
}

\examples{
## On an XInteger object:
x <- as(12:-2, "XInteger")
reverse(x)

## On an XIntegerViews object:
v <- successiveViews(x, 1:5)
v
reverse(v)

## On an XVectorList object:
if (require(Biostrings) && require(drosophila2probe)) {
  library(Biostrings)
  library(drosophila2probe)
  probes <- DNAStringSet(drosophila2probe)
  reverse(probes)
}
}

\keyword{methods}
\keyword{manip}
