\name{icac}
\alias{icac}
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
\title{ICA noise correction.}
\description{By-trial correction of EEG/MEG data for known (i.e., recorded) and
	unknown (i.e., not recorded) sources of noise.}
\usage{icac(x, channel, noise.sig, trial.cn = "Trial", 
include = TRUE, threshold = 0.4, n.comp = length(channel), 
ica.method = "R", correct = TRUE, ica.only = FALSE, 
proctime = TRUE, seed = NULL, verbosity = 5, ...)}
\arguments{
  \item{x}{A data frame containing the EEG/MEG  data to be corrected.
	  Measurements for each channel/electrode should be arranged in columns
	  with the channel/electrode to which it pertains as their names (e.g.,
	  \code{Fp1}, \code{Fp2}, \code{AF3}, \code{AF4}, \ldots). If the noise
	  channels (e.g., VEOG, HEOG, ECG, \ldots) are present in this data frame,
	  they will be removed unless argument \code{include} is set to
	  \code{TRUE}. Note that there must be a column containing trial
	  information.}
  \item{channel}{The channels to correct. You can use the output of function
	  \code{des} from package \code{eRp}, e.g., des("biosemi.32")$electrodes.}
  \item{noise.sig}{The channel(s) against which each independent component (IC)
	  will be correlated. Can be anything really, HEOG, VEOG, ECG, \ldots}
  \item{trial.cn}{The name of the column containing trial information.
	  Defaults to "Trial".}
  \item{include}{Whether to include the noise channels in the ICA. Defaults to
	  \code{TRUE}.}
  \item{threshold}{The correlation threshold between noise signal(s) and IC above which the EEG/MEG data will be corrected. Default is 0.4 (as in Flexer et al., 2005). Can be set to anything between 0 (will zero-out every IC) and 1 (will most probably zero-out nothing).}
  \item{n.comp}{Number of components. Defaults to the number of channels used.}
  \item{ica.method}{If \code{method == "R"} then computations are done exclusively in R (default). The code allows the interested R user to see exactly what the algorithm does. If \code{method == "C"} then C code is used to perform most of the computations, which makes the algorithm run faster. During compilation the C code is linked to an optimized BLAS library if present, otherwise stand-alone BLAS routines are compiled.}
  \item{correct}{Logical. Defaults to \code{TRUE}. Whether to correct the data or to simply get information pertaining to the what IC correlated with what noise signal at what trial.}
  \item{ica.only}{Logical. Defaults to \code{FALSE}. Whether to perform fastICA only without noise correction.}
  \item{proctime}{Logical. Defaults to \code{TRUE}. Determines how much real and CPU time (in minutes) the currently running R process has already taken.}
  \item{seed}{Defaults to \code{NULL}, which means that \code{set.seed} is randomly set for every run. There is the possibility, however, to pass an arbitrary seed, e.g., 0 to insure replicability between runs for instance. In this later case, if fastICA doesn't converge (e.g., it returns matrices and data frames with NAs), try setting the seed to another value (e.g., 1).}
  \item{verbosity}{Numeric. The amount of information printed to screen during the modeling process. The higher the number, the more information is printed. \code{0} turns this option off. Defaults to \code{5}. Maximum value is \code{6}.}
  \item{\dots}{Further arguments to pass to function \code{fastICA}.}
}
\details{If the verbosity level is high enough, the output will contain the noise signal beeing processed, the trial, the IC, and the correlation between the noise signal and the IC at that trial. For example:
\preformatted{
     ...
     noise signal = Temp; trial = 19; IC = 6; cor = -0.307971687318979 
     noise signal = Temp; trial = 19; IC = 7; cor = 0.111036533642789 
     noise signal = Temp; trial = 19; IC = 8; cor = -0.0226991408620133 
     noise signal = Temp; trial = 19; IC = 9; cor = 0.233890667361682 
     noise signal = Temp; trial = 19; IC = 10; cor = 0.878635491834294 
     noise signal = Temp; trial = 19; IC = 11; cor = 0.0891185123593569 
     noise signal = Temp; trial = 19; IC = 12; cor = 0.524880913590867 
     noise signal = Temp; trial = 19; IC = 13; cor = -0.126156352285347 
     noise signal = Temp; trial = 19; IC = 14; cor = -0.312246072685998 
     ...
} 

If one wishes to simply know what ICs correlate at or above threshold with what
noise signal at what trial (i.e., no correction), set \code{correct = FALSE}.
This would be done if one only wished to zero-out entire ICs without
zeroing-out anything else. 
}
\value{
\item{data}{If \code{correct = TRUE}, the corrected data. Otherwise, the Pre-processed data.}
\item{channel}{The channels that were corrected.}
\item{noise.sig}{The noise signals for which the data were corrected.}
\item{threshold}{The correlation threshold above which the EEG/MEG data will be corrected.}
\item{n.comp}{The number of independent components used in the ICA.}
\item{X}{Pre-processed data.}
\item{K}{Pre-whitening matrix that projects data onto th first n.comp principal components.}
\item{W}{The estimated un-mixing matrix.}
\item{A}{The estimated mixing matrix.}
\item{S}{If \code{correct = TRUE}, the corrected estimated source matrix. Otherwise, the original (uncorrected) source matrix (which will be equal to S0).}
\item{S0}{The uncorrected estimated source matrix.}
\item{col.means}{The mean of each channel.}
\item{correlations}{For each noise signal and each trial, the correlation between the IC and the noise signal.}
\item{correction.info}{A data frame with columns "NoiseSignal" (the noise signal with which ICs were compared), "IC" (the IC which correlated above threshold with the noise signal), "Trial" (the trial at which the noise signal and the IC correlated above threshold), and "Corr" (the correlation between the noise signal and the IC).}
\item{proctime}{If \code{proctime = TRUE}, a data frame with processing time information.}
}
\references{
Flexer, A., Bauer, H., Pripfl, J. & Dorffner, G. (2005). Using ICA for removal of ocular artifacts in EEG recorded from blind subjects. \emph{Neural Networks}, \emph{18}, 998--1005.

Hyvarinen, Aapo & Oja, Erkki. (1999). \emph{Independent Component Analysis: A Tutorial}. Available at \url{http://cis.legacy.ics.tkk.fi/aapo/papers/IJCNN99_tutorialweb/}.
}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{fastICA}};
\code{\link{mwd.thrsh}};
\code{\link{plot_avgba}};
\code{\link{plot_trba}};
\code{\link{plot_nic}};
\code{\link{plot_tric}};
\code{\link{summary.icac}};
\code{\link{topo_ic}};
\code{\link{update.icac}}.
}
\examples{
### See vignette for examples.
}
