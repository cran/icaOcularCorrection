\name{topo.ic}
\alias{topo.ic}
\concept{EEG}
\concept{electro-encephalogram}
\concept{electroencephalogram}
\concept{ERP}
\concept{event-related potentials}
\concept{event related potentials}
\concept{MEG}
\concept{magneto-encephalogram}
\concept{magnetoencephalogram}
\concept{event-related fields}
\concept{event related fields}
\concept{event-related magnetic fields}
\concept{event related magnetic fields}
\title{Plot the topographic map of an independent component.}
\description{coming soon}
\usage{\method{topo}{ic}(x, ic, coords = c("bosemi.32","egi.129", 
"neuromag.306.mag", "neuromag.306.plan1", "neuromag.306.plan2"), 
col = "topo", xlab="left to right", ylab="back to front", 
k = NULL, bs = "cr", too.far = NULL, ...)}
\arguments{
  \item{x}{An \code{icac} object as returned by the function of the same name.}
  \item{ic}{Integer. The number of the independent component to plot.}
  \item{coords}{Either one of "bosemi.32", "egi.129", "neuromag.306.mag",
	  "neuromag.306.plan1", or "neuromag.306.plan2", or a data frame containing
	  the x- and y-coordinates of each electrode. In the latter case, the data
	  frame has to have columns "x", "y", and "Channel" (the names of the
	  electrodes).}
  \item{col}{The colour scheme to use for plots. One of "topo", "heat", "cm",
	  "terrain", "gray" or "bw".  Schemes "gray", "grey", and "bw" also modify
	  the colors used.}
  \item{xlab, ylab}{Titles for the axes. Numbers will be coerced to character
	  strings. Defaults to "left to right" and "back to front", respectively.}
  \item{k}{The dimension of the basis used to represent the smooth term (for
	  interpolation). Defaults to \code{NULL}, i.e., its value depends on the
	  coordinate system. See \code{te} from package \code{mgcv} for details.}
  \item{bs}{A two letter character string indicating the smoothing basis to be
	  used for interpolation. Defaults to "cr". See \code{smooth.terms} from
	  package \code{mgcv} for an over view of what is available.}
  \item{too.far}{Numeric. Plot grid nodes that are too far from the channels
	  can be excluded from the plot. \code{too.far} determines what is too far.
	  Defaults to \code{NULL}, i.e., its value depends on the coordinate
	  system. \code{NA} to disable.}
  \item{...}{Further arguments to be passed to function \code{vis.gam} from
	  packages \code{mgcv}.}
}
\value{A topographic plot of an independent component.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot.avgba}};
\code{\link{plot.trba}};
\code{\link{plot.nic}};
\code{\link{plot.tric}};
\code{\link{summary.icac}};
\code{\link{topo.ic}};
\code{\link{update.icac}}.
}
\examples{
### See vignette for examples.
}
