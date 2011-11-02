\name{eeg}
\alias{eeg}
\concept{EEG}
\concept{electro-encephalogram}
\concept{electroencephalogram}
\concept{ERP}
\concept{event-related potentials}
\concept{event related potentials}
\docType{data}
\title{Data frame from subject 21.}
\description{Recorded from subject 21 during chunk recall experiment with Biosemi Mk II system, 32 electrodes + 4 EOGs + 2 mastoids. Original sampling rate: 8000 Hz. Downsampled to 128 Hz. Referenced on-line to mastoids. Filtered off-line 0.01 to 30 Hz with Butterworth filter in Analyzer v. 1.05. Epoch length is 192. There are 10 4-word sequences of the original 432 (thus 10 items * 192 samples per item = 1920 data points).}
\usage{data(eeg)}
\format{A data frame with 1920 observations on the following 41 variables.
  \describe{
    \item{\code{Fp1}}{a numeric vector}
    \item{\code{AF3}}{a numeric vector}
    \item{\code{F7}}{a numeric vector}
    \item{\code{F3}}{a numeric vector}
    \item{\code{FC1}}{a numeric vector}
    \item{\code{FC5}}{a numeric vector}
    \item{\code{T7}}{a numeric vector}
    \item{\code{C3}}{a numeric vector}
    \item{\code{CP1}}{a numeric vector}
    \item{\code{CP5}}{a numeric vector}
    \item{\code{P7}}{a numeric vector}
    \item{\code{P3}}{a numeric vector}
    \item{\code{Pz}}{a numeric vector}
    \item{\code{PO3}}{a numeric vector}
    \item{\code{O1}}{a numeric vector}
    \item{\code{Oz}}{a numeric vector}
    \item{\code{O2}}{a numeric vector}
    \item{\code{PO4}}{a numeric vector}
    \item{\code{P4}}{a numeric vector}
    \item{\code{P8}}{a numeric vector}
    \item{\code{CP6}}{a numeric vector}
    \item{\code{CP2}}{a numeric vector}
    \item{\code{C4}}{a numeric vector}
    \item{\code{FC6}}{a numeric vector}
    \item{\code{T8}}{a numeric vector}
    \item{\code{FC2}}{a numeric vector}
    \item{\code{F4}}{a numeric vector}
    \item{\code{F8}}{a numeric vector}
    \item{\code{AF4}}{a numeric vector}
    \item{\code{Fp2}}{a numeric vector}
    \item{\code{Fz}}{a numeric vector}
    \item{\code{Cz}}{a numeric vector}
    \item{\code{LM}}{a numeric vector}
    \item{\code{RM}}{a numeric vector}
    \item{\code{LC}}{a numeric vector}
    \item{\code{RC}}{a numeric vector}
    \item{\code{TE}}{a numeric vector}
    \item{\code{BE}}{a numeric vector}
    \item{\code{UnknownChannel1}}{a numeric vector}
    \item{\code{UnknownChannel2}}{a numeric vector}
    \item{\code{Subject}}{a character vector}
  }
}
\examples{
data(eeg)
head(eeg)
dim(eeg)
}
\keyword{datasets}

