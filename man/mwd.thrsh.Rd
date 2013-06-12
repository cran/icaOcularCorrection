\name{mwd.thrsh}
\alias{mwd.thrsh}
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
\title{Multiple wavelet decomposition thresholding.}
\description{Applies hard or soft multiple wavelet thresholding to a signal.}
\usage{mwd.thrsh(data, verbosity = 3, ...)}
\arguments{
  \item{data}{A vector containing the data you wish to threshold. If the length of this vector is not a power of 2 times the dimension of the DMWT (multiplicity of wavelets), the vector will be padded with approximately the same number of 0's at each end for processing and then removed once done.}
  \item{verbosity}{Numeric. The amount of information printed to screen during the modeling process. The higher the number, the more information is printed. \code{0} turns this option off. Defaults to \code{3}. Maximum value is \code{3}.}
  \item{\dots}{Further arguments to pass to functions \code{mwd} and \code{wr}.}
}
\details{The signal of small amplitude will be thrown away and the bigger peaks will remain. This function is here to experiment with the mwd thresholding of noise signals, more specifically, to determine whether first thresholding one or more noise signals improves correction. Might be good for EOG channels, i.e., would keep the bigger eye-movements and blinks and get rid of the other stuff, potentially resulting in the ICs that correlate with the thresholded EOGs being (close to) purely eye-movement artifacts. See \code{?mwd} and \code{?wr} of package \code{wavethresh} for details about these functions.}
\value{The thresholded data vector (without the 0 padding).}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\examples{
if(try(require(wavethresh,quietly=TRUE))){
    blinks <- rep(c(rep(0,5),sin(1:10)*35,rep(0,15)),5)
    noise <- runif(length(blinks), min = -12, max = 12)
    blinks.thrsh <- mwd.thrsh(blinks + noise)
    
    ylimit <- c(-45, 80)
    plot(blinks + noise, type = "l", ylim = ylimit,
      ylab = "Amplitude", col = grey(0.4))
    lines(blinks.thrsh, col = "red", lty = 2)
    lines(blinks, col = "blue", lty = 3)
    legend("topleft", legend = c("Blinks + Noise", 
      "Thresholded Blinks + Noise", "Blinks only"), 
      lty = c(1, 2, 3), col = c(grey(0.4), "red", 
      "blue"), bty = "n", cex = 0.75)
}
}
