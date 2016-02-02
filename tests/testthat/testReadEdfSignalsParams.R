#
# Purpose   :    Test the use of readEdfSignals parameters
#
# Copyright :   (C) 2015, Vis Consultancy, the Netherlands
#               This program is free software: you can redistribute it and/or modify
#               it under the terms of the GNU General Public License as published by
#               the Free Software Foundation, either version 3 of the License, or
#               (at your option) any later version.
#
#               This program is distributed in the hope that it will be useful,
#               but WITHOUT ANY WARRANTY; without even the implied warranty of
#               MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#               GNU General Public License for more details.
#
#               You should have received a copy of the GNU General Public License
#               along with edf package for R.  If not, see <http://www.gnu.org/licenses/>.
#
#
# History    :
#   Jan16 - Created
# ------------------------------------------------------------------------------
#
require (testthat)
require (edfReader)

context ("Compare reading whole files with the export from EDFBrowser.")

libDir <- system.file("extdata", package="edfReader")
sampleDir <- paste (libDir, '/', sep='')
sFns <- c('edfPlusC.edf', 'bdfPlusD.bdf')

sFFns <- character (length = length(sFns))
for (i in 1:length(sFns)) sFFns[i] <- paste (sampleDir, sFns[i], sep='')

sHdrs <- vector (mode='list', length = length(sFns))
for (i in 1:length(sFns)) sHdrs[[i]] <- readEdfHeader(sFFns[i])

#                            Test ranges
# ------------------------------------------------------------------------------

testMinMax <- function  (fileNo) {
    hdr         <- sHdrs[[fileNo]]

    #  digital range
    that <- "the digital/physical Min value must be smaller than the digital Max value"

    hdr <- sHdrs[[1]]
    hdr$sHeaders$digitalMin[2]  <-  hdr$sHeaders$digitalMax[2]
    expect_error (readEdfSignals (hdr), "Illegal digital min/max, use physical=FALSE",
                  info="the digitalMin should be less than digitalMax")

    #  physical range
    hdr <- sHdrs[[1]]
    hdr$sHeaders$physicalMin[3]  <-  hdr$sHeaders$physicalMax[3]
    expect_error (readEdfSignals (hdr), "Illegal physical min/max, use physical=FALSE",
                  info="the physicalMin should be less than physicalMax")

    # both ranges
    hdr <- sHdrs[[1]]
    hdr$sHeaders$digitalMin[2]   <-  hdr$sHeaders$digitalMax[2]
    hdr$sHeaders$physicalMin[3]  <-  hdr$sHeaders$physicalMax[3]
    expect_error (readEdfSignals (hdr), "Illegal digital/physical min/max, use physical=FALSE",
                  info="both the digital/physicalMin should be less than digital/physicalMax")
}

#                           Test label selections
# ------------------------------------------------------------------------------
testLabelSelection <- function (fileNo) {
    that <- 'signal label selections'
    hdr         <- sHdrs[[fileNo]]
    nSignals <- nrow (hdr$sHeaders)

    s <- readEdfSignals(hdr, signals='Annotations')
    expect_equal (s$label, "EDF Annotations",
                  info="the only signal read should be the 'EDF Annotations' signal")

    s <- readEdfSignals(hdr, signals='Ordinary')
    expect_equal(length(s), nSignals-1,
                 info="the number of signals should be one less than the total with the annotations")
    expect_null(s$`EDF Annotations`,
                info="there shouldn't be an annotation signal")
    expect_equal (s$`sine 8 Hz`$label, "sine 8 Hz",
                 info="the signal named `sine 8 Hz` should have the label 'sine 8 Hz'")

    s <- readEdfSignals(hdr, signals=8)
    expect_equal (s$signalNumber, 8)

    expect_error (readEdfSignals(hdr, signals='Bla'), "Unkown signal designation Bla")
    expect_error(readEdfSignals(hdr, signals=""), "Unkown signal designation")

    expect_error(readEdfSignals(hdr, signals=13), "Signal number out of bound: 13")
    expect_error(readEdfSignals(hdr, signals=c(100, 200)), "Signal numbers out of bound: 100 200")

    s <- readEdfSignals(hdr, signals=c(8, '8', 'sine 8.5 Hz'), simplify=FALSE)
    expect_equal (length(s), 1)
    expect_equal (s[[1]]$signalNumber, 8)

    s <- readEdfSignals(hdr, signals=c(3, '5', 'Annotations', 'sine 8.5 Hz'), simplify=FALSE)
    expect_equal (length(s), 4)
    expect_equal (s[[1]]$signalNumber, 3)
    expect_equal (s[[2]]$signalNumber, 5)
    expect_equal (s[[3]]$signalNumber, 8)
    expect_equal (s[[4]]$signalNumber, 12)
}

testMinMax(1)
testLabelSelection (1)

