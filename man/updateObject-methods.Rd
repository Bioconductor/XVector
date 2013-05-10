\name{updateObject-methods}
\alias{updateObject-methods}

\alias{updateObject,XVector-method}
\alias{updateObject,XIntegerViews-method}

\title{Update an object of a class defined in the XVector package to its
       current class definition}

\description{
  The XVector package provides an extensive collection of
  \code{\link[BiocGenerics]{updateObject}} methods for updating
  almost any instance of a class defined in the package.
}
\usage{
## Showing usage of method defined for XVector objects only (usage
## is the same for all methods).

\S4method{updateObject}{XVector}(object, ..., verbose=FALSE)
}

\arguments{
  \item{object}{
    Object to be updated. Many (but not all) XVector classes are supported.
    If no specific method is available for the object, then the default
    method (defined in the BiocGenerics package) is used.
    See \code{?\link[BiocGenerics]{updateObject}} for a description of
    the default method.
  }
  \item{..., verbose}{
    See \code{?\link[BiocGenerics]{updateObject}}.
  }
}

\value{
  Returns a valid instance of \code{object}.
}

\seealso{
  \code{\link[BiocGenerics]{updateObject}}
}

\keyword{manip}
