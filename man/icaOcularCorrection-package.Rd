\name{icaOcularCorrection-package}
\alias{icaOcularCorrection-package}
\alias{icaOcularCorrection}
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
\docType{package}
\title{Independent Components Analysis (ICA) based eye-movement correction (HEOG and VEOG) and correction of other known (i.e., recorded; e.g., ECG, GSR, ...) or unknown (i.e., not recorded) sources of noise.}
\description{Removes eye-movement and other types of known (i.e., recorded) or unknown (i.e., not recorded) artifacts using the fastICA package. The correction method proposed in this package is largely based on the method described in on Flexer, Bauer, Pripfl, and Dorffner (2005). The process of correcting electro- and magneto-encephalographic data (EEG/MEG) begins by running function ``icac'', which first performs independent components analysis (ICA) to decompose the data frame into independent components (ICs) using function ``fastICA'' from the package of the same name. It then calculates for each trial the correlation between each IC and each one of the noise signals -- there can be one or more, e.g., vertical and horizontal electro-oculograms (VEOG and HEOG), electro-myograms (EMG), electro-cardiograms (ECG), galvanic skin responses (GSR), and other noise signals. Subsequently, portions of an IC corresponding to trials at which the correlation between it and a noise signal was at or above threshold (set to 0.4 by default; Flexer et al., 2005, p. 1001) are zeroed-out in the source matrix, ``S''. The user can then identify which ICs correlate with the noise signals the most by looking at the summary of the ``icac'' object (using function ``summary.icac''), the scalp topography of the ICs (using function ``topo.ic''), the time courses of the ICs (using functions ``plot.tric'' and ``plot.nic''), and other diagnostic plots. Once these ICs have been identified, they can be completely zeroed-out using function ``update.icac'' and the resulting correction checked using functions ``plot.avgba'' and ``plot.trba''. Some worked-out examples with R code are provided in the package vignette. Please contact the package maintainer to obtain the data to run the examples.}
\details{
\tabular{ll}{
Package: \tab icaOcularCorrection\cr
Type: \tab Package\cr
Version: \tab 2.2.4\cr
Date: \tab 2013-06-25\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
You can access the PDF version of the package vignette by typing at an R prompt \code{browseVignettes(package = "icaOcularCorrection")}. This will open a web browser with links to the vignette PDF as well as a plain-text R file containing the code used in the vignette. The vignette files, both the PDF and the Rnw sources document, are located in the \code{doc} directory of an installed package (\code{inst/doc} for an uninstalled package tarball). The PDF, R, and Rnw versions of the vignette are located in \code{paste(system.file(package = "icaOcularCorrection"), "doc/", sep = "/")}. See <http://www.bioconductor.org/help/package-vignettes/> for more details.
}
\author{
Antoine Tremblay, NeuroCognitive Imaging Lab, Dalhousie University
Maintainer: Antoine Tremblay <trea26@gmail.com>
}
\references{
Flexer, A., Bauer, H., Pripfl, J. & Dorffner, G. (2005). Using ICA for removal of ocular artifacts in EEG recorded from blind subjects. Neural Networks, 18, 998-1005.

Hyvarinen, Aapo & Oja, Erkki. (1999). \emph{Independent Component Analysis: A Tutorial}. Available at \url{http://cis.legacy.ics.tkk.fi/aapo/papers/IJCNN99_tutorialweb/}.
}
\keyword{ package }
\seealso{
\code{\link{fastICA}};
\code{\link{icac}};
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
