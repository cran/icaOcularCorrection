\name{icaOcularCorrection-package}
\alias{icaOcularCorrection-package}
\alias{icaOcularCorrection}
\docType{package}
\title{Package to perform Independent Components Analysis (ICA) based eye-movement correction.}
\description{Removes eye-movements artifacts as well as (a portion of) of other noise using the fastICA package. The correction methods is based on Flexer, Bauer, Pripfl, and Dorffner (2005). Using the \code{icaDEN.fnc} function, the EEG is first decomposed into indepedent components (ICs), then the correlation between the EOG and each of the ICs is calculated. Any IC with a correlation at or above threshold (default is 0.4) is zeroed out and the EEG is finally reconstructed. The functions \code{batch.icDEN.fnc} and \code{mc.batch.icaDEN.fnc} allow for batch processing.}
\details{
\tabular{ll}{
Package: \tab icaOcularCorrection\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 02-11-2011\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
icaDEN.fnc
batch.icaDEN.fnc
mc.batch.icaDEN.fnc
plotICs.fnc
batch.plotICs.fnc
defineElectrodeSet.fnc
}
\author{
Antoine Tremblay, IWK Health Center

Maintainer: Antoine Tremblay <trea26@gmail.com>
}
\references{Flexer, A., Bauer, H., Pripfl, J. & Dorffner, G. (2005). Using ICA for removal of ocular artifacts in EEG recorded from blind subjects. Neural Networks, 18, 998-1005.}
\keyword{ package }
\seealso{
\code{\link{defineElectrodeSet.fnc}};
\code{\link{eeg}};
\code{\link{den}};
\code{\link{icaDEN.fnc}};
\code{\link{batch.icaDEN.fnc}};
\code{\link{mc.batch.icaDEN.fnc}};
\code{\link{plotICs.fnc}};
\code{\link{batch.plotICs.fnc}}
}
\examples{
data(eeg)
head(eeg)
# Create icaOcularCorrection object
den = icaDEN.fnc(eeg, epoch.length = 192, 
    method = "all", electrodes = c("Fp1", 
    "Fp2", "AF3", "AF4", "F7", "F3", "Fz", 
    "F4", "F8", "FC5", "FC1", "FC2", "FC6", 
    "T7", "C3", "Cz", "C4", "T8", "CP5", 
    "CP1", "CP2", "CP6", "P7", "P3", "Pz", 
    "P4", "P8", "PO3", "PO4", "O1", "Oz", 
    "O2"), eogs = list(VEOG = c("BE", "TE"), 
    HEOG = c("LC", "RC")), threshold = 0.4, 
    n.comp = 16, ret.SAWK = TRUE, 
    verbose = TRUE)
names(den)
den$snr.ica

# Plot ICs
plotICs.fnc(den, epoch.length = 192, cor.only = 
    FALSE, whichEOG = c("VEOG", "HEOG"), 
    method = "all", dir.create.path = paste(getwd(), 
    "/figs/", sep = ""), dir.create.name = "IC.plots", 
    threshold = NA, nplots = c(2, 2), ask = TRUE, 
    plot.EOG = TRUE, pdf.it = FALSE, pdf.prefix = 
    "ICs_")
}
