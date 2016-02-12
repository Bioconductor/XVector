\name{XRawList-comparison}

\alias{XRawList-comparison}

\alias{pcompare}
\alias{pcompare,XRawList,XRawList-method}

\alias{==,XRawList,XRawList-method}
\alias{<=,XRawList,XRawList-method}

\alias{duplicated.XRawList}
\alias{duplicated,XRawList-method}

\alias{match,XRawList,XRawList-method}

\alias{is.unsorted,XRawList-method}
\alias{order,XRawList-method}
\alias{rank,XRawList-method}


\title{Comparing and ordering the list elements of XRawList objects}

\description{
  Methods for comparing and ordering the elements in one or more
  \link{XRawList} objects.
}

\usage{
## Element-wise (aka "parallel") comparison of 2 XRawList objects
## --------------------------------------------------------------

\S4method{==}{XRawList,XRawList}(e1, e2)

\S4method{<=}{XRawList,XRawList}(e1, e2)

## duplicated()
## ------------

\S4method{duplicated}{XRawList}(x, incomparables=FALSE, ...)

## match()
## -------

\S4method{match}{XRawList,XRawList}(x, table, nomatch=NA_integer_, incomparables=NULL)

## order() and related methods
## ---------------------------

\S4method{is.unsorted}{XRawList}(x, na.rm=FALSE, strictly=FALSE)

\S4method{order}{XRawList}(..., na.last=TRUE, decreasing=FALSE)

\S4method{rank}{XRawList}(x, na.last=TRUE,
     ties.method=c("average", "first", "random", "max", "min"))

## Generalized element-wise (aka "parallel") comparison of 2 XRawList objects
## --------------------------------------------------------------------------

\S4method{pcompare}{XRawList,XRawList}(x, y)
}

\arguments{
  \item{e1, e2, x, table, y}{
    \link{XRawList} objects.
  }
  \item{incomparables}{
    Not supported.
  }
  \item{...}{
    For \code{duplicated}: currently no additional arguments are allowed.

    For \code{order}: additional \link{XRawList} objects used for breaking
    ties.
  }
  \item{nomatch}{
    The value to be returned in the case when no match is found.
    It is coerced to an \code{integer}.
  }
  \item{na.rm, na.last}{
    Ignored.
  }
  \item{strictly}{
    \code{TRUE} or \code{FALSE}. Should the check be for \emph{strictly}
    increasing values?
  }
  \item{decreasing}{
    \code{TRUE} or \code{FALSE}.
  }
  \item{ties.method}{
    A character string specifying how ties are treated. Only \code{"first"}
    and \code{"min"} are supported for now.
  }
}

\details{
  [TODO]
}

\author{H. Pagès}

\seealso{
  \itemize{
    \item The \link{XRawList} class.

    \item \link[IRanges]{Ranges-comparison} in the IRanges
          package for comparing and ordering ranges.

    \item \code{\link{==}},
          \code{\link[BiocGenerics]{duplicated}},
          \code{\link[BiocGenerics]{unique}},
          \code{\link[BiocGenerics]{match}},
          \code{\link{\%in\%}},
          \code{\link[BiocGenerics]{order}},
          \code{\link[BiocGenerics]{sort}},
          \code{\link[BiocGenerics]{rank}} for general information about
          those functions.
  }
}

\examples{
## TODO
}

\keyword{methods}
