\name{plotICs.fnc}
\alias{plotICs.fnc}
\title{Function to plot the ICA-corrected EEG against uncorrected EEG.}
\description{
Function to plot the ICA-corrected EEG against uncorrected EEG. Also plots the EOGs in a different X11 window.
}
\usage{
plotICs.fnc(ica.object, epoch.length = NA, cor.only = FALSE, whichEOG = c("VEOG", "HEOG"), method = "by.trial", dir.create.path = paste(getwd(), "/figs/", sep = ""), dir.create.name = "IC.plots", recursive = TRUE, threshold = NA, nplots = c(2, 2), ask = TRUE, plot.EOG = TRUE, pdf.it = FALSE, pdf.prefix = "ICs_")
}
\arguments{
  \item{ica.object}{An icaDEN.fnc denoised object.}
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to NA).}
  \item{cor.only}{Logical. Defaults to FALSE. If TRUE, will plot only the ICs that correlated with the EOGs at or above threshold.}
  \item{whichEOG}{Vector of EOG channels to plot.}
  \item{method}{For one subject and one electrode, either perform by-trial correction (default) or correction on the whole vector (all).}
  \item{dir.create.path}{The path where a directory will be created, in which the corrected files will be placed. Defaults to "paste(getwd(), "/figs/", sep = "")".}
  \item{dir.create.name}{The name of the directory where the corrected files will be placed. Defaults to "IC.plots".}
  \item{recursive}{Logical. Should elements of the path other than the last be created?  If TRUE, like Unix's 'mkdir -p'. Value passed to "recursive" argument of "dir.create".}
  \item{threshold}{The correlation threshold between EOG and IC. Default is 0.4. Can be set to anything between 0 (will zero-out every IC) and 1 (will most probably zero-out nothing).}
  \item{nplots}{Number of panels per page.}
  \item{ask}{Logical.  Defaults to TRUE. If TRUE (and the R session is interactive) the user is asked for input, before a new figure is drawn.}
  \item{plot.EOG}{Logical. If TRUE, a second X11 window will be opened to plot the EOG channels named in the argument "whichEOG".}
  \item{pdf.it}{Logical. Defaults to FALSE. If TRUE, the plots will be saved in a pdf file.}
  \item{pdf.prefix}{A prefix to add in front of the name of the corrected file. Defaults to "ICs_".}
}
\value{Plots and pdf files.}
\author{Antoine Tremblay <trea26@gmail.com>}
\seealso{
\code{\link{defineElectrodeSet.fnc}}
\code{\link{eeg}}
\code{\link{den}}
\code{\link{batch.icaDEN.fnc}}
\code{\link{icaDEN.fnc}}
\code{\link{batch.plotICs.fnc}}
}
\examples{
data(den)
# plot ICs
plotICs.fnc(den, epoch.length = 192, cor.only = FALSE, whichEOG = c("VEOG", "HEOG"), method = "by.trial", dir.create.path = paste(getwd(), "/figs/", sep = ""), dir.create.name = "IC.plots", threshold = 0.4, nplots = c(2, 2), ask = TRUE, plot.EOG = TRUE, pdf.it = FALSE, pdf.prefix = "ICs_")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
