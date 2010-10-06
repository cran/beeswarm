\name{beeswarm}
\alias{beeswarm}
\alias{beeswarm.numeric}
\alias{beeswarm.default}
\alias{beeswarm.formula}
\title{Bee swarm plot}
\description{Create a bee swarm plot.  A bee swarm plot is a one-dimensional scatter plot similar to "stripchart", except that would-be overlapping points are separated such that each is visible.}
\usage{

\method{beeswarm}{formula}(formula, data = NULL, subset, na.action = NULL, 
         pwpch = NULL, pwcol = NULL, pwbg = NULL, dlab, glab, ...)

\method{beeswarm}{default}(x, 
    method = c("center", "hex", "square", "smile"), 
    vertical = TRUE, horizontal = !vertical, 
    cex = par("cex"), spacing = 1, breaks = NULL,
    labels, at = NULL, 
    pch = 1, col = 1, bg = col,
    pwpch = NULL, pwcol = NULL, pwbg = pwcol,
    do.plot = TRUE, add = FALSE, 
    xlim, ylim, log = FALSE, 
    xlab = NULL, ylab = NULL, dlab = "", glab = "",
    ...)

}
\arguments{
  \item{formula}{A formula, such as \code{y ~ grp}, where \code{y} is a
    numeric vector of data values to be split into groups according to
    the grouping variable \code{grp} (usually a factor).}
  \item{data}{A data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{An optional vector specifying a subset of observations
    to be used for plotting.}
  \item{na.action}{A function which indicates what should happen
    when the data contain \code{NA}s.  The default is to ignore missing
    values in either the response or the group.}
  \item{x}{ A list of numeric vectors, each of which is plotted as an individual swarm.}
  \item{method}{ Method for arranging points (see Details). }
  \item{vertical, horizontal}{ Orientation of the plot. \code{horizontal} takes precedence if both are specified. }
  \item{cex}{ Relative size of points. Unlike other plotting functions, this must be a single value.}
  \item{spacing}{ Relative spacing between points. }
  \item{breaks}{ Breakpoints (optional).  If \code{NULL}, breakpoints are chosen automatically. If \code{NA}, bins are not used (similar to \code{stripchart} with \code{method = "stack"}).}
  \item{labels}{ Labels for each group.  By default, these are inferred from the data. }
  \item{at}{ Numeric vector giving the locations where the swarms should be drawn; defaults to 1:n where n is the number of groups. }
  \item{pch, col, bg}{ Plotting characters and colors, specified by group (see Details). }
  \item{pwpch, pwcol, pwbg}{ "Point-wise" plotting characters and colors, specified for each data point (see Details). }
  \item{do.plot}{ Draw a plot? }
  \item{add}{ Add to an existing plot? }
  \item{xlim, ylim}{ Limits of the plot. }
  \item{log}{ Use a logarithmic scale on the data axis? }
  \item{xlab, ylab}{ Axis labels. }
  \item{dlab, glab}{ An alternate way to specify axis labels (see Details). }
  \item{\dots}{ Further arguments passed to \code{\link{plot}}. }
}
\details{ 
This function is similar to \code{\link{stripchart}}, except with different methods for placing the points, and with a few extra features. 

Several methods for placing the points are available; each is designed to avoid overlapping points:

The first three methods first discretize the values along the data axis, in order to create more efficient packing: \code{square} stacks the points on a square grid, whereas \code{hex} uses a hexagonal grid.  \code{center} uses an adjusted square grid to produce a symmetric swarm.

By default, the number of breakpoints for discretization is determined by a combination of the available plotting area and the plotting character size. Therefore, in contrast to most plotting functions, changing the plotting window size will often change the position of the points.  

The discretization of the data can be explicitly controlled using \code{breaks}. If \code{breaks} is set to \code{NA}, the data will not be grouped into intervals; this may be a sensible option if the data is already discrete.

The fourth method, \code{smile}, does not discretize the data.  Instead the points are simply shifted sideways (along the group axis) by a minimal amount, producing a somewhat erratic looking figure. \code{breaks} is ignored.

The plotting characters and colors can be controlled in two ways. First, the arguments \code{pch}, \code{col} and \code{bg} can specify plotting characters and colors in the same way as \code{\link{stripchart}} and \code{\link{boxplot}}: in short, the arguments apply to each group as a whole (and are recycled if necessary).

Alternatively, the characteristics of each individual data point can be controlled using \code{pwpch}, \code{pwcol}, and \code{pwbg}, which override \code{pch}, \code{col} and \code{bg} if these are also specified.  These arguments can be specified as a list or vector.  If supplied using the formula method, the arguments can be specified as part of the formula interface; i.e. they are affected by \code{data} and \code{subset}.

  The \code{dlab} and \code{glab} labels may be used instead of \code{xlab}
  and \code{ylab} if those are not specified.  \code{dlab} applies to the 
  continuous data axis (the Y axis unless \code{horizontal} is \code{TRUE}),
  \code{glab} to the group axis.

This function is intended to be mostly compatible with calls to \code{\link{stripchart}} or \code{\link{boxplot}}.  Thus, code that works with these functions should work with \code{beeswarm} with minimal modification.

}
\value{ A data frame with plotting information, invisibly. }
\note{ 
\code{xlim} and \code{ylim} are reversed on horizontal plots.

\code{method = "smile"} is currently incompatible with a logarithmic scale.
}
\seealso{ \code{\link{stripchart}}, \code{\link{boxplot}} }
\examples{ 

  ## One of the examples from 'stripchart'
  beeswarm(decrease ~ treatment,
    data = OrchardSprays, log = TRUE, 
    pch = 16, col = rainbow(8))

  ## Demonstrating the use of 'pwcol'
  data(breast)
  beeswarm(ESR1 ~ ER, data = breast,
    pch = 16, pwcol = event_survival + 1)
  legend('topleft', legend = c('censored', 'metastasis'),
    pch = 16, col = 1:2)

  ## Demonstrating 'smile' method
  beeswarm(time_survival ~ event_survival, data = breast,
    method = 'smile',
    pch = 16, pwcol = as.numeric(ER),
    xlab = '', ylab = 'Follow-up time (months)',
    labels = c('Censored', 'Metastasis'))
  legend('topright', legend = levels(breast$ER),
    title = 'ER', pch = 16, col = 1:2)

  ## The list interface
  distributions <- list(runif = runif(100, min = -3, max = 3), 
                        rnorm = rnorm(100))
  beeswarm(distributions, col = 2:3)

  ## the "bg" argument only has an effect if "pch" is in 21:25
  beeswarm(distributions, pch = 21, bg = 2:3)

  ## the "smile" method
  beeswarm(distributions, col = 2:3, method = 'smile')

  ## use of pwcol with the list interface 
  myCol <- lapply(distributions, cut, breaks = 8)
  beeswarm(distributions, pch = 16, pwcol = myCol)
  
  ## log scale
  beeswarm(1:200, log = TRUE)

  
}
\keyword{ misc }
