\name{mc.batch.icaDEN.fnc}
\alias{mc.batch.icaDEN.fnc}
\concept{EEG}
\concept{electro-encephalogram}
\concept{electroencephalogram}
\concept{ERP}
\concept{event-related potentials}
\concept{event related potentials}
\title{Multicore batch processing for \code{icaDEN.fnc}.}
\description{Performs multicore batch processing for \code{icaDEN.fnc}.}
\usage{mc.batch.icaDEN.fnc(epoch.length = NULL, method = "by.trial", fn = NA, 
fn.path = file.path(getwd(), "data", "dataEEG", "formatted"), 
fn.pattern = "^formatted.*\\\.rda", fn.object.name = "eeg", begin = 1, 
finish = NA, threshold = 0.4, n.comp = 16, dir.create.path = file.path(getwd(), 
"data", "dataEEG"), dir.create.name = "ICA.denoised", fn.save.prefix = "ICA_", 
other.things.to.do = NULL, electrodes = defineElectrodeSet.fnc()$electrodes, 
eogs = list(VEOG = c("BE", "TE"), HEOG = c("LC", "RC")), recursive = TRUE, 
ret.SAWK = TRUE, pdf.it = FALSE, pdf.prefix = "ICs_", pdf.compress = FALSE, 
verbose = TRUE, gen.log = c(TRUE, FALSE))}
\arguments{
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to \code{NULL}).}
  \item{method}{For one subject and one electrode, either perform by-trial correction (default) or correction on the whole vector (all).}
  \item{fn}{The names of the \code{.rda} files to be batch processed. By default, the function will get the names of all the files in the directory provided in the "fn.path" argument.}
  \item{fn.path}{The path where the uncorrected \code{.rda} files are located. Set to \code{file.path(getwd(),"data", "dataEEG","formatted")} by default. Value passed to \code{path} argument of \code{list.files}.}
  \item{fn.pattern}{A regular expression. Only file names which match the regular expression will be returned. Default is \code{"^formatted.*\\\.rda"}. Value passed to \code{pattern} argument of \code{list.files}.}
  \item{fn.object.name}{The name of the to-be-corrected object saved in the \code{.rda} files. Default is \code{"eeg"}.}
  \item{begin}{Within the file name vector, begin the correction process at which point. Default is to begin with the first object of the vector.}
  \item{finish}{Within the file name vector, finish correction process as which point. Default is to finish with the last item in the file name vector.}
  \item{threshold}{The correlation threshold at or above which the IC will be zeroed-out. Correlation is between one IC and (a portion of) the data for one electrode. Default is 0.4. Value passed to \code{threshold} argument of \code{icaDEN.fnc}.}
  \item{n.comp}{Number of components. Defaults to 16. Value passed to \code{n.comp} argument of \code{icaDEN.fnc}.}
  \item{dir.create.path}{The path where a directory will be created, in which the corrected files will be placed. Defaults to \code{file.path(getwd(), "data", "dataEEG")}.}
  \item{dir.create.name}{The name of the directory where the corrected files will be placed. Defaults to \code{"ICA.denoised"}.}
  \item{fn.save.prefix}{A prefix to add in front of the name of the corrected file. Defaults to \code{"ICA_"}.}
  \item{other.things.to.do}{You can tell the function in the form of a vector (\code{c(...)}) to performing other things before correction. For instance: \code{c('eeg$Recalled = as.factor(eeg$Recalled)', 'eeg$LogitABCD = as.numeric(eeg$LogitABCD)')}.}
  \item{electrodes}{Supply a vector of electrodes to be corrected. The default is a vector of electrode names supplied by defineElectrodeSet.fnc$electrodes. Value supplied to \code{electrodes} argument of \code{icaDEN.fnc}.}
  \item{eogs}{The default is a list identifying which electrodes form the vertical EOG (VEOG) and the horizontal EOG (HEOG). Only one EOG can be supplied (e.g., \code{list(VEOG = c("BE", "TE"))}) or a vector containing the names of the EOG channels (e.g., \code{c("BE", "LC")}). Value supplied to \code{eogs} argument of \code{icaDEN.fnc}.}
  \item{recursive}{Logical. Should elements of the path other than the last be created?  If \code{TRUE}, like Unix's \code{mkdir -p}. Value passed to \code{recursive} argument of \code{dir.create}.}
  \item{ret.SAWK}{Return the S, A, W, and K matrices? Default is \code{TRUE}. Value passed to \code{ret.SAWK} argument of \code{icaDEN.fnc}.}
  \item{pdf.it}{Logical. Defaults to \code{FALSE}. If \code{TRUE}, the plots will be saved in a \code{pdf} file.}
  \item{pdf.prefix}{A prefix to add in front of the name of the corrected files. Defaults to \code{"ICs_"}.}
  \item{pdf.compress}{Logical (defaults to \code{FALSE}). If you have the \code{pdftk} tool installed on your system, whether to compress \code{.pdf} files.}
  \item{verbose}{Whether you want to see a verbal indication of where in the process you are.}
  \item{gen.log}{Vector of logicals. If the first element of \code{gen.log} is \code{TRUE} (default), generates log of batch processing in file for each participant where the name of the file contains the name of the participant's EEG data. If the second element of the vector is \code{TRUE}, the output will go to the log file and the screen; if \code{FALSE} (default), the output to the screen will be supressed.}
}
\value{The function saves one \code{.rda} file for each subject containing the corrected EEG in the specified directory and \code{pdf} files if enabled.}
\note{You need to have package \code{multicore} installed on your machine to use this function. You will probably encounter the following warning: \emph{There were 50 or more warnings (use warnings() to see the first 50)}. Do not worry, these warnings relate to the function trying to calculate a correlation between the EOGs and a zeroed-out IC.

If the \code{pdftk} tool is not installed on your machine, go to \url{http://www.pdflabs.com/docs/install-pdftk/} to install it.
}
\author{Antoine Tremblay, IWK Health Center, \email{trea26@gmail.com}}
\seealso{
\code{\link{defineElectrodeSet.fnc}};
\code{\link{icaDEN.fnc}};
\code{\link{batch.icaDEN.fnc}};
\code{\link{eeg}};
\code{\link{den}};
\code{\link{plotICs.fnc}};
\code{\link{batch.plotICs.fnc}}
}
