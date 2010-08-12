\name{icaDEN.fnc}
\alias{icaDEN.fnc}
\title{Independent Components Analysis based eye-movement correction.
}
\description{
    Removes eye-movements artifacts as well as (a portion of) of other noise using the fastICA package. The correction methods is based on Flexer, Bauer, Pripfl, and Dorffner (2005). The first step is to decompose the EEG into indepedent components (ICs), then calculate the correlation between the EOG and each of the ICs. Any IC with a correlation at or above threshold (default is 0.4) is zeroed out and the EEG is then reconstructed.
}
\usage{
icaDEN.fnc(object, epoch.length = NA, method = "by.trial", electrodes = defineElectrodeSet.fnc(bottom.eye = NA)$electrodes, eogs = list(VEOG = c("BE", "TE"), HEOG = c("LC", "RC")), threshold = 0.4, n.comp = NA, ret.SAWK = TRUE, plot.it = FALSE, fn.pdf = "default", verbose = TRUE)
}
\arguments{
  \item{object}{A data frame containing EEG recordings (one electrode per column), as well as EOG channels (one per column) and a column with subject identifiers.}
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to NA).}
  \item{method}{For one subject and one electrode, either perform by-trial correction (default) or correction on the whole vector (all).}
  \item{electrodes}{Supply a vector of electrodes to be corrected. The default is a vector of electrode names supplied by defineElectrodeSet.fnc$electrodes.}
  \item{eogs}{The default is a list identifying which electrodes form the vertical EOG (VEOG) and the horizontal EOG (HEOG). Only one EOG can be supplied (e.g., list(VEOG=c("BE","TE"))) or a vector containing the names of the EOG channels (e.g., c("BE","LC")).}
  \item{threshold}{The correlation threshold between EOG and IC. Default is 0.4. Can be set to anything between 0 (will zero-out every IC) and 1 (will most probably zero-out nothing).}
  \item{n.comp}{Number of components. Defaults to the number of EEG channels.}
  \item{ret.SAWK}{Return the S, A, W, and K matrices? Default is TRUE.}
  \item{plot.it}{Whether the IC and the uncorrected EEG should be plotted. Defaults to FALSE.}
  \item{fn.pdf}{fn stands for "file name". There is a default file name which is given by paste("icaDEN_Subject",unique(eeg$Subject),".pdf",sep=""). You can supply whatever you want here.}
  \item{verbose}{Whether you want to see a verbal indication of where in the process you are.}
}
\value{
\item{eeg}{The data frame containing the corrected EEG.}
\item{snr.ica}{A matrix showing for each corrected electrode the siganl-to-noise ration.}
\item{threshold}{The correlation threshold at which the ICs were zeroed-out.}
\item{S}{The estimated source matrix.}
\item{A}{The estimated mixing matrix.}
\item{W}{The estimated un-mixing matrix.}
\item{K}{pre-whitening matrix that projects data onto th first n.comp principal components.}
}
\references{
Flexer, A., Bauer, H., Pripfl, J. & Dorffner, G. (2005). Using ICA for removal of ocular artifacts in EEG recorded from blind subjects. Neural Networks, 18, 998-1005.
}
\author{Antoine Tremblay <trea26@gmail.com>}
\seealso{
\code{\link{defineElectrodeSet.fnc}}
\code{\link{eeg}}
\code{\link{den}}
\code{\link{batch.icaDEN.fnc}}
\code{\link{plotICs.fnc}}
\code{\link{batch.plotICs.fnc}}
}
\examples{
data(eeg)
head(eeg)
den=icaDEN.fnc(eeg, epoch.length = 192, method = "by.trial", electrodes = c("Fp1","Fp2","AF3","AF4","F7","F3","Fz","F4","F8","FC5","FC1","FC2","FC6","T7","C3","Cz","C4","T8","CP5","CP1","CP2","CP6","P7","P3","Pz","P4","P8","PO3","PO4","O1","Oz","O2"), eogs = list(VEOG = c("BE", "TE"), HEOG = c("LC", "RC")), threshold = 0.4, n.comp = NA, ret.SAWK = TRUE, plot.it = FALSE, fn.pdf = "default", verbose = TRUE)
names(den)
den$snr.ica
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }


