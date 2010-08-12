\name{defineElectrodeSet.fnc}
\alias{defineElectrodeSet.fnc}
\title{Generate a vector of channel names and plotting information.}
\description{
    Description: Generates a vector of channel names of a specific kind of cap ("Biosemi", "Easy Cap") as well as information for plotting namely, panel skips, x and y ticks, x and y coordinates, and a plotting matrix.
}
\usage{
defineElectrodeSet.fnc(which.cap = "biosemi.32", bottom.eye = "BE")
}
\arguments{
  \item{which.cap}{
    One of "easy.cap.28", "easy.cap.64.partial", "easy.cap.64.full", or "biosemi.32". Defaults to "biosemi.32".
}
  \item{bottom.eye}{
    Adds the specified EOG channel to the vector of EEG channels. Defaults to "BE", i.e., bottom eye EOG. Can be set to any other EOG channel (e.g., "TE", "LC", "RC", etc.). Can also be "NA", in which case, no EOG channel is added to the EEG channel vector.
}
}
\details{
}
\value{
  \item{electrodes}{A vector of EEG channel names.}
  \item{xticks}{A vector of comprised of "s" or "n" indicating, for each channel in the electrodes vector, the x-axis should be drawn.}
  \item{yticks}{A vector of comprised of "s" or "n" indicating, for each channel in the electrodes vector, the y-axis should be drawn.}
  \item{skips}{A named numeric vector indicating how many panels should be left blank before plotting the specific EEG channel (e.g., draw 2 blank panels before plotting channel O1 in the "biosemi.32" cap setup).}
  \item{pmatrix}{A matrix of NAs with number of rows equal to the number of channel rows on cap (e.g., 9 rows from Fp1 to O1 in "biosemi.32" setup) and number of columns equal to the number of channel columns on cap (e.g., 5 columns from T7 to T8 in "biosemi.32" setup).}
  \item{xcords}{A named vector indicating the column number to plot a specific EEG channel (e.g., plot channel Fp1 in second panel; plot channel Fp2 in fourth panel).}
  \item{ycords}{A named vector indicating the row number to plot a specific EEG channel (e.g., plot channels Fp1 and Fp2 in first row; plot channel Fz in third row).}
  \item{cap.name}{The name of the cap setup used.}
}
\author{
  R. Harald Baayen <baayen@ualberta.ca>, Peter Hendrix <hendrix@ualberta.ca>, and Antoine Tremblay <trea26@gmail.com>.
}
\seealso{
\code{\link{eeg}}
\code{\link{den}}
\code{\link{batch.icaDEN.fnc}}
\code{\link{icaDEN.fnc}}
}
\examples{
defineElectrodeSet.fnc(which.cap="biosemi.32",bottom.eye=NA)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
