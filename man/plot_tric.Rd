\name{plot_tric}
\alias{plot_tric}
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
\title{Plot the time course of an independent component at each trial.}
\description{Plots an independent component at each trial optionally with a noise
signal overlaid on top of it.}
\usage{plot_tric(x, ic, noise.sig = NULL, S = "S0",
trial.cn = "Trial", trials = NULL, n.win = 10, 
new.page = TRUE, ...)}
\arguments{
  \item{x}{An \code{icac} object as returned by the function of the same name.}
  \item{ic}{Integer. An independent component.}
  \item{noise.sig}{The channel(s) against which the independent components (ICs) were correlated. Defaults to \code{NULL}, which means that no noise signall is superimposed on the IC time course.}
  \item{S}{The corrected ("S0") or uncorrected ("S") source matrix.}
  \item{trial.cn}{The quoted column name containing the trial information. Defaults to "Trial".}  
  \item{trials}{The trials to plot. Defaults to \code{NULL}, i.e., all trials will be plotted.}  
  \item{n.win}{Integer. The number of time courses to plot one on top of the other. Defaults to 10.}
  \item{new.page}{Integer. The number of time courses to plot. Defaults to \code{NULL}, which results in the number time courses per window equal to the number of channels to plot up to 10.}
  \item{...}{Unused.}
}
\details{The number in red to the far left indicates which trial is being plotted. The IC is plotted in black. If an IC correlated above threshold with the noise signal, the noise signal is plotted in blue. Otherwise, it is plotted in grey.}
\value{A plot.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot_avgba}};
\code{\link{plot_trba}};
\code{\link{plot_nic}};
\code{\link{summary.icac}};
\code{\link{topo_ic}};
\code{\link{update.icac}}.
}
\examples{
### See vignette for examples.
}
