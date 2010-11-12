\name{batch.icaDEN.fnc}
\alias{batch.icaDEN.fnc}
\title{Batch processing for icaDEN.fnc.}
\description{
Performs batch processing for icaDEN.fnc.
}
\usage{
batch.icaDEN.fnc(epoch.length = NA, method = "by.trial", fn = NA, fn.path = "./dataEEG/formattedEEG", fn.pattern = "^Formatted.*\\\.rda", fn.full.names = TRUE, fn.object.name = "eeg", begin = 1, finish = NA, threshold = 0.4, n.comp = NA, dir.create.path = paste(getwd(), "/dataEEG", sep = ""), dir.create.name = "ICA.denoised", fn.save.prefix = "ICA_", other.things.to.do = NULL, electrodes = defineElectrodeSet.fnc()$electrodes, eogs = list(VEOG = c("BE", "TE"), HEOG = c("LC", "RC")), recursive = TRUE, ret.SAWK = TRUE, plot.it = TRUE, fn.pdf = "default", verbose = TRUE, gen.log = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to NA).}
  \item{method}{For one subject and one electrode, either perform by-trial correction (default) or correction on the whole vector (all).}
  \item{fn}{The names of the .rda files to be batch processed. By default, the function will get the names of all the files in the directory provided in the "fn.path" argument.}
  \item{fn.path}{The path where the uncorrected .rda files are located. Set to "./dataEEG/formattedEEG" by default. Value passed to "path" argument of "list.files".}
  \item{fn.pattern}{Whether the full names of the to-be-batch-processed files should be retrieved, that is, the full path. Default is "^Formatted.*\\\.rda". Value passed to "pattern" argument of "list.files".}
  \item{fn.full.names}{Logical (defaults to TRUE). Value passed to "full.names" argument of "list.files".}
  \item{fn.object.name}{The name of the to-be-corrected object saved in the .rda files. Default is "eeg".}
  \item{begin}{Within the file name vector, begin the correction process at which point. Default is to begin with the first object of the vector.}
  \item{finish}{Within the file name vector, finish correction process as which point. Default is to finish with the last item in the file name vector.}
  \item{threshold}{The correlation threshold at or above which the IC will be zeroed-out. Correlation is between one IC and (a portion of) the data for one electrode. Default is 0.4. Value passed to "threshold" argument of "icaDEN.fnc".}
  \item{n.comp}{Number of components. Defaults to the number of EEG channels. Value passed to "n.comp" argument of "icaDEN.fnc".}
  \item{dir.create.path}{The path where a directory will be created, in which the corrected files will be placed. Defaults to "paste(getwd(), "/dataEEG", sep = "")".}
  \item{dir.create.name}{The name of the directory where the corrected files will be placed. Defaults to "ICA.denoised".}
  \item{fn.save.prefix}{A prefix to add in front of the name of the corrected file. Defaults to "ICA_".}
  \item{other.things.to.do}{You can tell the function in the form of a vector (c(...)) to performing other things before correction. For instance: c('eeg$Recalled=as.factor(eeg$Recalled)','eeg$LogitABCD=as.numeric(eeg$LogitABCD)').}
  \item{electrodes}{Supply a vector of electrodes to be corrected. The default is a vector of electrode names supplied by defineElectrodeSet.fnc$electrodes. Value supplied to "electrodes" argument of "icaDEN.fnc".}
  \item{eogs}{The default is a list identifying which electrodes form the vertical EOG (VEOG) and the horizontal EOG (HEOG). Only one EOG can be supplied (e.g., list(VEOG=c("BE", "TE"))) or a vector containing the names of the EOG channels (e.g., c("BE", "LC")). Value supplied to "eogs" argument of "icaDEN.fnc".}
  \item{recursive}{Logical. Should elements of the path other than the last be created?  If TRUE, like Unix's 'mkdir -p'. Value passed to "recursive" argument of "dir.create".}
  \item{ret.SAWK}{Return the S, A, W, and K matrices? Default is TRUE. Value passed to "ret.SAWK" argument of "icaDEN.fnc".}
  \item{plot.it}{Whether the IC and the uncorrected EEG should be plotted. Defaults to FALSE. Value passed to "plot.it" argument of "icaDEN.fnc".}
  \item{fn.pdf}{fn stands for "file name". There is a default file name which is given by paste("icaDEN_Subject",unique(eeg$Subject),".pdf",sep=""). You can supply whatever you want here. Value passed to "fn.pdf" argument of "icaDEN.fnc".}
  \item{verbose}{Whether you want to see a verbal indication of where in the process you are.}
  \item{gen.log}{Logical. If TRUE (defalt) generates log of batch processing in file paste(getwd(), "/logs/batch.icaDEN.fnc_log_", gsub(" ", "_",date()), ".txt", sep = "").}
}
\value{The function saves one .rda file for each subject containing the corrected EEG in the specified directory.}

\author{
Antoine Tremblay, IWK Health Center, <trea26@gmail.com>
}

\seealso{
\code{\link{defineElectrodeSet.fnc}}
\code{\link{icaDEN.fnc}}
\code{\link{eeg}}
\code{\link{den}}
\code{\link{plotICs.fnc}}
\code{\link{batch.plotICs.fnc}}
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
