% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSignalFunctions.R
\name{readEdfSignals}
\alias{readEdfSignals}
\alias{readBdfSignals}
\title{Reads signals from an EDF(+)/BDF(+) file}
\usage{
readEdfSignals(hdr, signals = "All", from = 0, till = Inf,
  physical = TRUE, fragments = FALSE, recordStarts = FALSE,
  mergeASignals = TRUE, simplify = TRUE)
}
\arguments{
\item{hdr}{An ebdfHeader object read with the readEdfHeader() function.}

\item{signals}{a vector with one or more of the following signal designations:
'All' (default), to include all signals;
'Ordinary', to include all ordinary signals;
'Annotations', to include all annotation signals;
signal labels and/or signal names; or
signal numbers (numeric or as character).}

\item{from}{numeric, the time in seconds from which the signals shall be read.}

\item{till}{numeric, the time in seconds till which the signals shall be read.
The value may exceed the total duration of the recoding.}

\item{physical}{logical, if TRUE (the default) digital samples values are mapped to their physical values,
If not, the digital values are returned.}

\item{fragments}{logical, if TRUE discontinuously recorded signals are stored as a list of continuous
fragments. If FALSE (the default), a signal is stored as one numeric vector with NA values filling the gaps.}

\item{recordStarts}{logical, if TRUE a data frame with the empty annotations with the data record start
time will be included. If FALSE (the default), not.}

\item{mergeASignals}{logical, if TRUE all annotations will be merged into one data frame. If FALSE
there will be one data frame per annotation signal.}

\item{simplify}{logical, if TRUE and if there is only one signal read, the signal itself is returned
(in stead of a list with that signal as the only one element).
If FALSE, this simplification is not performed.}
}
\value{
Either a list of one or more signals or a single signal. \cr
The list of signals returned is of class ebdfSignals and a single signal object is of one of the
following classes:
\itemize{
  \item ebdfASignal, for an annotation signal
  \item ebdfFSignal, for a fragmented ordinary signal
  \item ebdfCSignal, for a continuous ordinary signal (possible supplemented with NA values)
}
All classes have supporting print and summary functions.
For object details see the package vignette.
}
\description{
The function reads ordinary or annotation signals from an EDF(+)/BDF(+) file.
}
\details{
For ordinary signals the from and till parameters are interpreted as [from, till).
  For annotation signals from-till has to overlap the onset-(onset+duration) period.
  For for details see the package vignette.
}
\section{Acknowledgement}{

   This package has used code from:
   \itemize{
     \item edf.R version 0.3 (27-11-2013), http://feschet.fr/?p=11
     \item the work of Henelius Andreas as of July 2015, https://github.com/bwrc/edf
   }
}

\examples{
# Examples from the vignette
libDir <- system.file ("extdata", package="edfReader")
# a continuous recording
CFile <- paste (libDir, '/edfPlusC.edf', sep='')
CHdr  <- readEdfHeader (CFile)
CSignals <- readEdfSignals (CHdr)            # to read all signals
# read 3 differently designated signals from 5.1 till 18 seconds period
someCSignalsPeriod <- readEdfSignals (CHdr, signals=c(3, "5", "sine 8.5 Hz"), from=5.1, till=18)
someCSignalsPeriod                           # print the signals
summary(someCSignalsPeriod)                  # print singals summary
someCSignalsPeriod$`sine 8.5 Hz`             # print the `sine 8.5 Hz` signal
summary(someCSignalsPeriod$`sine 8.5 Hz`)    # print a `sine 8.5 Hz` signal summary
str(CSignals$`sine 8.5 Hz`)                  # look to the details
# a discontinuous recording
DFile <- paste (libDir, '/edfPlusD.edf', sep='')
DHdr  <- readEdfHeader (DFile)
DSignals <- readEdfSignals (DHdr, fragments=TRUE)    # to read all signals
DSignals$`sine 8.5 Hz`                       # print fragmented signal
summary (DSignals$`sine 8.5 Hz`)             # print fragmented signal summary
str(DSignals$`sine 8.5 Hz`)                  # look to the details
}
\seealso{
\code{\link{edfReader}}, \code{\link{readEdfHeader}}\cr
   For the vignette use the console command:\cr
   \code{vignette('edfReaderVignette', package = "edfReader")}\cr
   or click on \code{Index} below.
}
