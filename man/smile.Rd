\name{smile}
\alias{smile}
\title{ Spread out 1-d data such that all points are visible }
\description{Spread out a 1-dimensional vector into two dimensions such that all points are visible. }
\usage{
smile(x, xsize, ysize)
}
\arguments{
  \item{x}{ A vector of values. }
  \item{xsize}{ Size of the plotting character in the direction along which \code{x} is plotted. }
  \item{ysize}{ Size of the plotting character in the perpedicular direction}
}
\details{ ... }
\value{ A vector of the same length as \code{x}. }
\seealso{ \code{\link{beeswarm}} }
\examples{ 

## Plot points in one dimension
x <- rep(0, 100)
y <- rnorm(100)
plot(x, y, xlim = c(-1, 2))

## Add smile-transformed points
xs <- smile(y, xsize = yinch(0.08), ysize = xinch(0.08))
points(xs + 1, y, col = 2)


}
\keyword{ misc }
