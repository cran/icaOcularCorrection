\name{summary.icac}
\alias{summary.icac}
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
\title{Print and/or return the correction summary of an "icac" object.}
\description{When \code{noise.sig = NULL}, the number of trials where an independent component (IC) correlated above threshold is listed, as well as the mean correlation across these trials. If a value is passed to argument \code{ic} and \code{noise.sig = NULL}, the number of trials where the IC and each noise signal correlated above threshold is listed.}
\usage{\method{summary}{icac}(object, noise.sig = NULL, ic = NULL, 
print = TRUE, ...)}
\arguments{
  \item{object}{An \code{icac} object as returned by the function of the same
	  name.}
  \item{noise.sig}{One of the channels against which the independent components were correlated. Defaults to \code{NULL}.}
  \item{ic}{Integer. An independent component. Defaults to \code{NULL}.}
  \item{print}{Logical. Whether to print the summary. Defaults to \code{TRUE}.}
  \item{...}{Unused.}
}
\value{Invisibly returns a data frame containing the summary.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot.avgba}};
\code{\link{plot.trba}};
\code{\link{plot.nic}};
\code{\link{plot.tric}};
\code{\link{topo.ic}};
\code{\link{update.icac}}.	
}
\examples{
### See vignette for examples.
}

