\name{plotICs.fnc}
\alias{plotICs.fnc}
\concept{EEG}
\concept{electro-encephalogram}
\concept{electroencephalogram}
\concept{ERP}
\concept{event-related potentials}
\concept{event related potentials}
\title{Function to plot the ICA-corrected EEG against uncorrected EEG.}
\description{Function to plot the ICA-corrected EEG against uncorrected EEG. Also plots the EOGs in a different X11 window.}
\usage{ plotICs.fnc(ica.object, epoch.length = NULL, cor.only = FALSE, 
whichEOG = c("VEOG", "HEOG"), method = "by.trial", dir.create.path = 
file.path(getwd(), "figs"), dir.create.name = "IC.plots", 
recursive = TRUE, threshold = NA, nplots = c(2, 2), ask = TRUE, 
plot.EOG = TRUE, pdf.it = FALSE, pdf.prefix = "ICs_", 
pdf.compress = FALSE)}
\arguments{
  \item{ica.object}{An \code{icaDEN.fnc} denoised object.}
  \item{epoch.length}{If performing by-trial correction, then supply the length of each trial in number of data points (e.g., 192, 179). If performing ICA correction on whole data at once, do not need this (set to \code{NULL}).}
  \item{cor.only}{Logical. Defaults to \code{FALSE}. If \code{TRUE}, will plot only the ICs that correlated with the EOGs at or above threshold.}
  \item{whichEOG}{Vector of EOG channels to plot.}
  \item{method}{For one subject and one electrode, either perform \code{"by-trial"} correction (default) or correction on the whole vector (\code{"all"}).}
  \item{dir.create.path}{The path where a directory will be created, in which the corrected files will be placed. Defaults to \code{file.path(getwd(), "figs")}.}
  \item{dir.create.name}{The name of the directory where the corrected files will be placed. Defaults to \code{"IC.plots"}.}
  \item{recursive}{Logical. Should elements of the path other than the last be created?  If \code{TRUE}, like Unix's \code{mkdir -p}. Value passed to \code{recursive} argument of \code{dir.create}.}
  \item{threshold}{The correlation threshold between EOG and IC. Default is 0.4. Can be set to anything between 0 (will zero-out every IC) and 1 (will most probably zero-out nothing).}
  \item{nplots}{Number of panels per page.}
  \item{ask}{Logical.  Defaults to \code{TRUE}. If \code{TRUE} (and the R session is interactive) the user is asked for input, before a new figure is drawn.}
  \item{plot.EOG}{Logical. If \code{TRUE}, a second X11 window will be opened to plot the EOG channels named in the argument \code{whichEOG}.}
  \item{pdf.it}{Logical. Defaults to \code{FALSE}. If \code{TRUE}, the plots will be saved in a \code{pdf} file.}
  \item{pdf.prefix}{A prefix to add in front of the name of the corrected files. Defaults to \code{"ICs_"}.}
  \item{pdf.compress}{Logical (defaults to \code{FALSE}). If you have the \code{pdftk} tool installed on your system, whether to compress \code{.pdf} files.}
}
\note{If the \code{pdftk} tool is not installed on your machine, go to \url{http://www.pdflabs.com/docs/install-pdftk/} to install it.}
\value{One PDF file per page.}
\author{Antoine Tremblay, IWK Health Center, \email{trea26@gmail.com}}
\seealso{
\code{\link{defineElectrodeSet.fnc}};
\code{\link{eeg}};
\code{\link{den}};
\code{\link{batch.icaDEN.fnc}};
\code{\link{mc.batch.icaDEN.fnc}};
\code{\link{icaDEN.fnc}};
\code{\link{batch.plotICs.fnc}}
}
\examples{
data(den)
# plot ICs
plotICs.fnc(den, epoch.length = 192, cor.only = 
    FALSE, whichEOG = c("VEOG", "HEOG"), 
    method = "by.trial", dir.create.path = 
    paste(getwd(), "/figs/", sep = ""), 
    dir.create.name = "IC.plots", threshold = 
    0.4, nplots = c(2, 2), ask = TRUE, plot.EOG = 
    TRUE, pdf.it = FALSE, pdf.prefix = "ICs_")
}
