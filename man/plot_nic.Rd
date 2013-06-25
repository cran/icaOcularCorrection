\name{plot_nic}
\alias{plot_nic}
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
\title{Plot an independent component with superimposed noise signal at a particular trial.}
\description{The function takes an \code{icac} object as returned by function \code{icac} and plots an independent component with superimposed noise signal at a particular trial.}
\usage{plot_nic(x, data, ic, trial, noise.sig, 
threshold = x$threshold, col = c("black", "blue"), 
main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, 
ylim = NULL, cex = 0.7, trial.cn = "Trial", ...)}
\arguments{
  \item{x}{An \code{icac} object as returned by the function of the same name.}
  \item{data}{The uncorrected data that was supplied to function \code{icac}.}
  \item{ic}{Numeric. An IC to plot.}
  \item{trial}{Numeric. A trial to plot.}
  \item{noise.sig}{Character. A noise signal. If missing, the first channel from \code{x$channel} will be taken.}
  \item{threshold}{Numeric. The threshold at which the correlation between an IC and a noise signal at a specific trial is deemed "significant". Defaults to \code{x$threshold}.}
  \item{col}{Vector. The colors to be used for plotting the noise signal (defaults to "black") and the IC if the correlation between the two reaches threshold (defaults to "blue").}
  \item{xlim, ylim}{x- and y-limits. These should be chosen to cover the range of values of the surface. Defaults to \code{NULL}.}
  \item{xlab, ylab}{Titles for the axes. Numbers will be coerced to character strings.}
  \item{main}{Main title. Defaults to \code{NULL}.}
  \item{cex}{A numerical value giving the amount by which plotting text and symbols should be magnified. Defaults to 0.7.}
  \item{trial.cn}{The quoted column name containing the trial information. Defaults to "Trial".}
  \item{\dots}{Further arguments to pass to \code{plot}, \code{lines},
	  \code{mtext}, and/or \code{legend}.}
}
\details{When plotting a trial where the IC did not correlate above threshold with the noise signal, the correlation and the line representing the IC will be gray by default. Otherwise, it will respectively be "blue". Note that if the correlation between an IC and a noise signal is negative, the noise signal is multiplied by -1 to make both waves comparable, but the correlation printed above the plot remains unchanged.} 
\value{A plot of the noise signal and the IC with which it correlates above threshold at a certain trial.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot_avgba}};
\code{\link{plot_trba}};
\code{\link{plot_tric}};
\code{\link{summary.icac}};
\code{\link{topo_ic}};
\code{\link{update.icac}}.
}
\examples{
# See vignette for examples.
}
