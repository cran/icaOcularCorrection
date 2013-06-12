\name{plot.avgba}
\alias{plot.avgba}
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
\title{Plot the average waveforms at each channel before and after correction.}
\description{For each channel, the average for uncorrected (black line) and corrected (blue line) waveforms across all trials is computed and plotted.}
\usage{\method{plot}{avgba}(x, data, channel = NULL, n.win = NULL, 
new.page = TRUE, time.cn = "Time", ...)}
\arguments{
  \item{x}{An \code{icac} object as returned by the function of the same name.}
  \item{data}{The uncorrected data that was supplied to function \code{icac}.}
  \item{channel}{The channels that were corrected.}
  \item{n.win}{Integer. The number of time courses to plot. Defaults to \code{NULL}, which results in the number time courses per window equal to the number of channels to plot up to 10.}
  \item{new.page}{\code{NULL} or a logical value.  If \code{TRUE} (the default), the user will be prompted before a new page of output is started.}
  \item{time.cn}{The name of the column that contains time information.}
  \item{\dots}{Further arguments to be passed to \code{plot} and \code{lines}.}
}
\value{A plot.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
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
