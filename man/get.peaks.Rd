\name{get.peaks}
\alias{get.peaks}
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
\title{Get the time value of one or more peaks.}
\description{Get the time value of one or more points of a time series by
	clicking on the location using a mouse. This function was designed to get
	the peaks of blinks and eye-movements, but could be used for any other type
	of time series. See section \code{Details} for more info on how to use it.}
\usage{get.peaks(data, channel, trials = NULL, 
trial.cn = "Trial", time.cn = "Time")}
\arguments{
  \item{data}{A data frame containing the data. Measurements for each
	  channel/electrode should be arranged in columns with the
	  channel/electrode to which it pertains as their names (e.g., \code{Fp1},
	  \code{Fp2}, \code{AF3}, \code{AF4}, \ldots).}
  \item{channel}{The channel from which time values for specific points on the time series are to be obtained.}
  \item{trials}{The trial(s) for which the function is to run through. Defaults
	  to \code{NULL}, which means that the function will run though all the
	  trials in the data frame.}
  \item{trial.cn}{The quoted column name containing the trial information.
	  Defaults to "Trial".}
  \item{time.cn}{The quoted column name containing the time information.
	  Defaults to "Time".}
}
\details{The function is build on function \code{locator}. As such,
	\code{get.peaks} is only supported on screen devices such as \code{X11},
	\code{windows}, and \code{quartz}. On other devices the call will do
	nothing. 
	
	For each one of the specified trials, the function plots the time course of
	EEG activity for one of the specified trials. The user then points to the
	desired location on the time series and clicks once with the left mouse
	button to fetch the coordinates. More than 1 point can be gathered this
	way. Once the user has finished selecting points, s/he presses on the right
	mouse button to move on to the next trial and repeat these steps until all
	of the trials specified in argument \code{trials} have been processed. Note
	that the y-coordinate of the selected points are irrelevant.}
\value{A named list of length equal to \code{length(trials)}. Each list element contains the time values corresponding to the locations where the user clicked. The names are of the for \code{trialXXX}, where \code{XXX} is a specific trial.}
\author{Antoine Tremblay, Dalhousie University, \email{trea26@gmail.com}}
\seealso{
\code{\link{identify}};
\code{\link{dev.capabilities}} to see if it is supported;
\code{\link{locator}}.
}
\examples{
### See vignette for examples.
}
