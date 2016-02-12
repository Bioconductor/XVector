\name{intra-range-methods}

\alias{intra-range-methods}

\alias{narrow}
\alias{narrow,XVectorList-method}

\alias{threebands}
\alias{threebands,XVectorList-method}


\title{Intra range transformations of an XVectorList object}

\description{
  The \emph{intra range transformations} are a set of generic functions
  defined in the \pkg{IRanges} package.
  Only 2 of them have methods for \link{XVectorList} objects:
  \code{\link[IRanges]{narrow}} and \code{\link[IRanges]{threebands}}.
  This man page describes those 2 methods only.

  See \code{?`\link[IRanges]{inter-range-methods}`} for more information.
}

\usage{
\S4method{narrow}{XVectorList}(x, start=NA, end=NA, width=NA, use.names=TRUE)

\S4method{threebands}{XVectorList}(x, start=NA, end=NA, width=NA)
}

\arguments{
  \item{x}{
    An \link{XVectorList} object.
  }
  \item{start, end, width}{
    Vectors of integers, possibly with NAs. See the SEW (Start/End/Width)
    interface in the \pkg{IRanges} package for the details
    (\code{?\link[IRanges]{solveUserSEW}}).
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
}

\details{
  \code{narrow} is equivalent to \code{subset} on an \link{XVectorList} object.

  \code{threebands} extends the capability of \code{narrow} by returning
  the 3 \link{XVectorList} objects associated with the narrowing operation.
  The returned value \code{y} is a list of 3 \link{XVectorList} objects
  named \code{"left"}, \code{"middle"} and \code{"right"}.
  The middle component is obtained by calling \code{narrow} with the
  same arguments (except that names are dropped). The left and right
  components are also instances of the same class as \code{x} and they
  contain what has been removed on the left and right sides (respectively)
  of the original ranges during the narrowing.
}

\author{H. Pagès}

\seealso{
  \itemize{
    \item \link[IRanges]{intra-range-methods} in the \pkg{IRanges} package
          for intra range transformations.
    \item \code{\link[IRanges]{solveUserSEW}} in the \pkg{IRanges} package
          for the SEW (Start/End/Width) interface.
    \item The \link{XVectorList} class.
  }
}

\examples{
## ---------------------------------------------------------------------
## narrow()
## ---------------------------------------------------------------------

#TODO: show examples

## ---------------------------------------------------------------------
## threebands()
## ---------------------------------------------------------------------

#TODO: show examples
}

\keyword{utilities}
