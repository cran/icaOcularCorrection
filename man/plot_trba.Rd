\name{plot_trba}
\alias{plot_trba}
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
\title{Plot the corrected and uncorrected time course at a specific channel for each trial.}
\description{For each trial, the corrected (blue line) and uncorrected (black line) are plotted. Optionally, a noise signal can be superimposed (grey line).}
\usage{plot_trba(x, data, channel, noise.sig = NULL, 
n.win = 10, new.page = TRUE, trial.cn = "Trial", ...)}
\arguments{
  \item{x}{An \code{icac} object as returned by the function of the same name.}
  \item{data}{The uncorrected data that was supplied to function \code{icac}.}
  \item{channel}{The name of one of the channels that were corrected.}
  \item{noise.sig}{Optional. The name of one of the noise signals.}
  \item{n.win}{Integer. The number of time courses to plot. Defaults to 10.}
  \item{new.page}{\code{NULL} or a logical value.  If \code{TRUE} (the default), the user will be prompted before a new page of output is started.}
  \item{trial.cn}{The name of the column that contains trial information.}
  \item{\dots}{Further arguments to be passed to \code{plot} and \code{lines}.}
}
\value{A plot.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot_avgba}};
\code{\link{plot_nic}};
\code{\link{plot_tric}};
\code{\link{summary.icac}};
\code{\link{topo_ic}};
\code{\link{update.icac}}.
}
\examples{
### See vignette for examples.
}
