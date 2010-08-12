\name{batch.plotICs.fnc}
\alias{batch.plotICs.fnc}
\title{Batch processing for plotICs.fnc.}
\description{Batch processing for plotICs.fnc.}
\usage{
batch.plotICs.fnc(epoch.length = NA, cor.only = FALSE, method = "by.trial", fn = NA, fn.path = paste(getwd(), "/dataEEG/ICA.denoised/", sep = ""), fn.pattern = "^ICA_.*\\\.rda", fn.full.names = TRUE, fn.object.name = "ica", whichEOG = c("VEOG", "HEOG"), threshold = NA, nplots = c(2, 2), ask = FALSE, plot.EOG = TRUE, pdf.it = TRUE, pdf.prefix = "ICs_", begin = 1, finish = NA, dir.create.path = paste(getwd(), "/figs/", sep = ""), dir.create.name = "IC.plots", other.things.to.do = NULL, recursive = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to NA).}
  \item{cor.only}{Logical. Defaults to FALSE. If TRUE, will plot only the ICs that correlated with the EOGs at or above threshold.}
  \item{method}{For one subject and one electrode, either perform by-trial correction (default) or correction on the whole vector (all).}
  \item{fn}{The names of the .rda files to be batch processed. By default, the function will get the names of all the files in the directory provided in the "fn.path" argument.}
  \item{fn.path}{The path where the uncorrected .rda files are located. Set to "paste(getwd(), "/dataEEG/ICA.denoised/", sep = "")" by default. Value passed to "path" argument of "list.files".}
  \item{fn.pattern}{Whether the full names of the to-be-batch-processed files should be retrieved, that is, the full path. Default is "^ICA_.*\\.rda". Value passed to "pattern" argument of "list.files".}
  \item{fn.full.names}{Logical (defaults to TRUE). Value passed to "full.names" argument of "list.files".}
  \item{fn.object.name}{The name of the to-be-corrected object saved in the .rda files. Default is "ica".}
  \item{whichEOG}{Vector of EOG channels to plot.}
  \item{threshold}{The correlation threshold between EOG and IC. Default is 0.4. Can be set to anything between 0 (will zero-out every IC) and 1 (will most probably zero-out nothing).}
  \item{nplots}{Number of panels per page.}
  \item{ask}{Logical.  Defaults to TRUE. If TRUE (and the R session is interactive) the user is asked for input, before a new figure is drawn.}
  \item{plot.EOG}{Logical. If TRUE, a second X11 window will be opened to plot the EOG channels named in the argument "whichEOG".}
  \item{pdf.it}{Logical. Defaults to FALSE. If TRUE, the plots will be saved in a pdf file.}
  \item{pdf.prefix}{A prefix to add in front of the name of the corrected file. Defaults to "ICs_".}
  \item{begin}{Within the file name vector, begin the correction process at which point. Default is to begin with the first object of the vector.}
  \item{finish}{Within the file name vector, finish correction process as which point. Default is to finish with the last item in the file name vector.}
  \item{dir.create.path}{The path where a directory will be created, in which the corrected files will be placed. Defaults to "paste(getwd(), "/figs/", sep = "")".}
  \item{dir.create.name}{The name of the directory where the corrected files will be placed. Defaults to "IC.plots".}
  \item{other.things.to.do}{You can tell the function in the form of a vector (c(...)) to performing other things before correction. For instance: c('eeg$Recalled=as.factor(eeg$Recalled)','eeg$LogitABCD=as.numeric(eeg$LogitABCD)').}
  \item{recursive}{Logical. Should elements of the path other than the last be created?  If TRUE, like Unix's 'mkdir -p'. Value passed to "recursive" argument of "dir.create".}
  \item{verbose}{Whether you want to see a verbal indication of where in the process you are.}
}
\value{A pdf file containing the IC plots for each subject as well as a separate pdf file for the EOGs.}
\author{Antoine Tremblay <trea26@gmail.com>}
\seealso{
\code{\link{defineElectrodeSet.fnc}}
\code{\link{eeg}}
\code{\link{icaDEN.fnc}}
\code{\link{batch.icaDEN.fnc}}
\code{\link{plotICs.fnc}}
\code{\link{den}}
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
