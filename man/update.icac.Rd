\name{update.icac}
\alias{update.icac}
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
\title{Update the correction performed by function \code{icac}.}
\description{The function takes an \code{icac} object as returned by the function of the same name and updates the correction as specified in argument \code{what}. Returns an \code{icac} object with the same slots as function \code{icac} does.}
\usage{\method{update}{icac}(object, what, verbosity = 2,
...)}
\arguments{
  \item{object}{An \code{icac} object as returned by the function of the same name.}
  \item{what}{Either a named three-element vector or a list of two-element vectors. In the first case, the first element should be "trial", the trial at which the user wishes to add/subtract an IC, then "ic", the IC which the user wishes to add/subtract, and finally "operation", the operation to perform, either "+" or "-" (put back or zero-out if \code{method = "zero-out"}. In the second case, this argument should be a list containing named two-element vectors (one or more). The first element of each vector should be the number corresponding to the IC that will operated on; the second  element should be the operation to perform \emph{on the whole IC} (put back, "+", or zero-out, "-").}
  \item{verbosity}{Numeric. The amount of information printed to screen during the modeling process. The higher the number, the more information is printed. \code{0} turns this option off. Defaults to \code{2}. Maximum value is \code{2}.}
  \item{...}{Unused.}
}
\value{An updated \code{icac} object with slots:
\item{x}{The corrected data.}
\item{channel}{The channels that were corrected.}
\item{noise.sig}{The noise signals for which the data were corrected.}
\item{threshold}{The correlation threshold above which the EEG/MEG data will be corrected.}
\item{n.comp}{The number of independent components used in the ICA.}
\item{S}{The updated (corrected) estimated source matrix.}
\item{A}{The estimated mixing matrix.}
\item{W}{The estimated un-mixing matrix.}
\item{K}{pre-whitening matrix that projects data onto th first n.comp principal components.}
\item{S0}{The un-updated estimated source matrix of the "icac" object passed
	to function \code{update.icac}.}
\item{col.means}{the mean of each channel.}
\item{correlations}{For each noise signal and each trial, the correlation between the IC and the noise signal.}
\item{correction.info}{A data frame with columns "NoiseSignal" (the noise signal with which ICs were compared), "IC" (the IC which correlated above threshold with the noise signal), "Trial" (the trial at which the noise signal and the IC correlated above threshold), and "Corr" (the correlation between the noise signal and the IC).}
\item{method}{The method used for correcting.}
\item{updated}{List. The first time an "icac" object is updated, a new slot "updated" is created containing the values in argument
	"what". Slot "updated" will be appended with the new values passed to argument "what" upon subsequent updates.}
\item{proctime}{If \code{proctime = TRUE} when running function \code{icac}, a data frame with processing time information.}
}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
\code{\link{mwd.thrsh}};
\code{\link{plot_avgba}};
\code{\link{plot_trba}};
\code{\link{plot_nic}};
\code{\link{plot_tric}};
\code{\link{summary.icac}};
\code{\link{topo_ic}}.
}
\examples{
### See vignette for examples.
}
