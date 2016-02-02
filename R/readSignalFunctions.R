#                      EDF(+)/BDF(+) file signals reader functions
#
# Purpose   :   Reading signals in  .edf(+)/.bdf(+) files
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
#               along with edfReader package for R.  If not, see <http://www.gnu.org/licenses/>.
#
# History    :
#   Oct15 - Created
#   Feb16 - Version 1.0.0
#
# Suffixes (not always used):
# RS and RT : Recoridng Sample number en Time in seconds, i.e. relative to the start of the recording
# DS and DT : Data record Sample number en Time in seconds, i.e. relative to the start of record
# FS and FT : Fragment Sample number en Time in seconds, i.e. relative to the start of signal fragment
# L and D   : Length (number of samples) and duration (in seconds)
#
# ------------------------------------------------------------------------------
#                        read ordinary / annotation signals
# ------------------------------------------------------------------------------
#' Reads signals from an EDF(+)/BDF(+) file
#'
#' The function reads ordinary or annotation signals from an EDF(+)/BDF(+) file.
#'
#' The list of signals returned is of class ebdfSignals and a single signal object is of one of the
#' following classes:
#' \itemize{
#'   \item ebdfASignal, for an annotation signal
#'   \item ebdfFSignal, for a fragmented ordinary signal
#'   \item ebdfCSignal, for a continuous ordinary signal (possible supplemented with NA values)
#' }
#' All classes have supporting print and summary functions.
#' For object details see the package vignette.
#' @param hdr An ebdfHeader object read with the readEdfHeader() function.
#' @param signals a vector with one or more of the following signal designations:
#'  'All' (default), to include all signals;
#'  'Ordinary', to include all ordinary signals;
#'  'Annotations', to include all annotation signals;
#'  signal labels; and signal numbers (numeric or as character).
#' @param from numeric, the time in seconds from which the signals shall be read.
#' @param till numeric, the time in seconds till which the signals shall be read.
#'   The value may exeed the total duration of the recoding.
#' @param physical logical, if TRUE (the default) digital samples values are mapped to their physical values,
#'   If not, the digital values are returned.
#' @param fragments logical, if TRUE discontinuously recorded signals are stored as a list of continuous
#'   fragments. If FALSE, a signal is stored as one numeric vector with NA values filling the gaps.
#' @param recordStarts logical, if TRUE the empty annotations with the data record start time will be
#'   included in the list of annotations.
#'   If FALSE (the default), they will be omitted.
#' @param simplify logical, if TRUE and if there is only one signal read, the signal itsels is returned
#'   (in stead of a list with that signal as the only one object).
#'   If FALSE, this simplification is not performed.
#' @return Either a list of one or more signals or a single signal.
#' @section Note:
#'   For ordinary signals the from and till parameters are interpreted as [from, till).
#'   For for details see the package vignette.
#' @section Acknowledgement:
#'    This package has used code from:
#'    \itemize{
#'      \item edf.R version 0.3 (27-11-2013), http://feschet.fr/?p=11
#'      \item the work of Henelius Andreas as of July 2015, https://github.com/bwrc/edf
#'    }
#' @seealso
#'    \code{\link{edfReader}}, \code{\link{readEdfHeader}}\cr
#'    For the vignette use the console command:\cr
#'    \code{vignette('edfReaderVignette', package = "edfReader")}\cr
#'    or click on \code{Index} below.
#' @aliases readBdfSignals
#' @examples
#' # Examples from the vignette
#' libDir <- system.file ("extdata", package="edfReader")
#' # a continuous recording
#' CFile <- paste (libDir, '/edfPlusC.edf', sep='')
#' CHdr  <- readEdfHeader (CFile)
#' CSignals <- readEdfSignals (CHdr)    # to read all signals
#' # read 3 differently designated signals from 5.1 till 18 seconds period
#' someCSignalsPeriod <- readEdfSignals (CHdr, signals=c(3, "5", "sine 8.5 Hz"), from=5.1, till=18)
#' someCSignalsPeriod                           # print the signals
#' summary(someCSignalsPeriod)                  # print singals summary
#' someCSignalsPeriod$`sine 8.5 Hz`             # print the `sine 8.5 Hz` signal
#' summary(someCSignalsPeriod$`sine 8.5 Hz`)    # print a `sine 8.5 Hz` signal summary
#' str(CSignals$`sine 8.5 Hz`)                  # look to the details
#' # a discontinuous recording
#' DFile <- paste (libDir, '/bdfPlusD.bdf', sep='')
#' DHdr  <- readEdfHeader (DFile)
#' DSignals <- readEdfSignals (DHdr, fragments=TRUE)    # to read all signals
#' DSignals$`sine 8.5 Hz`                       # print fragmented signal
#' summary (DSignals$`sine 8.5 Hz`)             # print fragmented signal summary
#' str(DSignals$`sine 8.5 Hz`)                  # look to the details
#' @export
readEdfSignals <- function (hdr, signals='All', from=0, till=Inf, physical=TRUE,
                            fragments=FALSE, recordStarts=FALSE, simplify=TRUE) {
    # check signals and get the indices
    idxAndErr <- edfProcessSignalDesignations (hdr, signals)
    if (is.null(idxAndErr)) stop ("Illegal 'signals' parameter")
    errs <- sum (idxAndErr$errors)
    if (errs) {
        if (errs==1)    pre <- 'Unkown signal designation'
        else            pre <- 'Unkown signal designations'
        stop (paste (pre, signals[idxAndErr$errors]))
    }
    if (length(idxAndErr$outOfBound)) {
        if (length(idxAndErr$outOfBound) == 1)  pre <- "Signal number out of bound:"
        else                                    pre <- "Signal numbers out of bound:"
        stop (paste(pre, paste(idxAndErr$outOfBound, collapse = " ")))
    }

    if (physical) {                                                             # conversion mut be possible
       digitalOk    <- hdr$sHeaders$digitalMin  <  hdr$sHeaders$digitalMax
       physicalOk   <- hdr$sHeaders$physicalMin < hdr$sHeaders$physicalMax
       digitalErr   <- sum (!digitalOk)
       physicalErr  <- sum (!physicalOk)
       if (digitalErr | physicalErr) {
           if (digitalErr & physicalErr) {
               msg <- "Illegal digital/physical min/max, use physical=FALSE"
           } else if (digitalErr) {
               msg <- "Illegal digital min/max, use physical=FALSE"
           } else if (physicalErr) {
               msg <- "Illegal physical min/max, use physical=FALSE"
           }
           stop (msg)
       }
    }

    # check for annotation and non continuous signals in EDF+ and BDF+
    isPlus       <- hdr$isPlus
    isContinuous <- hdr$isContinuous

    if (isPlus & !sum(hdr$sHeaders$isAnnotation)) {
        fp <- paste (hdr$fileType, '+', sep='')
        cat (fp, 'ERROR: required annotation signal is missing, signals will be retrieved as for an',
             hdr$fileType, 'file.\n')
        isPLus <- FALSE
        isContinuous <- TRUE
    }

    # check and justify 'from' and 'till'
    from <- max (0, from)
    if (till < from) {
        stop ("Illegal from-till range.")
    }
    if (isContinuous & from > hdr$recordedPeriod) {
        stop ("From after the continuously recorded period.")
    }

    # elaborate from-till boundaries per signal
    DFromRS         <- ceiling (hdr$sHeaders$sRate * from) + 1
    DFromRT         <- (DFromRS-1) / hdr$sHeaders$sRate
    DTillRS         <- ceiling (hdr$sHeaders$sRate * till)
    DTillRT         <- (DTillRS-1) / hdr$sHeaders$sRate

    sgn          <- idxAndErr$signals                                           # signals to be returned
    annotations1 <- which.max (hdr$sHeaders$isAnnotation)

    # open file and skip the header
    if (file.exists (hdr$fileName)) inFile <- file(hdr$fileName, "rb", encoding="UTF-16LE")
    else stop (paste ("File '", hdr$fileName, "' doesn't exists.", sep=''))
    readBin (inFile, 'raw', n=hdr$headerLength , size=1)                        # the header is not needed;
                                                                                #seek is not advised for Windows
    # initialise data objects to be read
    signals <- vector (mode='list', length=hdr$nSignals)                        # new list with a element per signal
    names (signals) <- hdr$sHeaders$label
    nextInCSignal   <- integer (length=hdr$nSignals) + 1                        # used for ordinary continuous signals only
    for (sn in 1:hdr$nSignals) if (sgn[sn]) {                                   # => NULL if not in sgn
        signals[[sn]]$signalNumber  <- sn
        signals[[sn]]$label         <- hdr$sHeaders$label[sn]
        signals[[sn]]$isContinuous  <- isContinuous
        signals[[sn]]$isAnnotation  <- hdr$isAnnotation
        signals[[sn]]$from          <- from
        signals[[sn]]$till          <- till
        if (!hdr$sHeaders$isAnnotation[sn]) {
            range <- paste (hdr$sHeaders$physicalMin[sn], " : ", hdr$sHeaders$physicalMax[sn],
                            ' ', hdr$sHeaders$physicalDim[sn], sep = '')
            signals[[sn]]$start         <- DFromRT[sn]
            signals[[sn]]$fromSample    <- DFromRS[sn]
            signals[[sn]]$transducerType<- hdr$sHeaders$transducerType[sn]
            signals[[sn]]$sampleBits    <- hdr$sampleBits
            signals[[sn]]$sRate         <- hdr$sHeaders$sRate[sn]
            signals[[sn]]$range         <- range
            signals[[sn]]$preFilter     <- hdr$sHeaders$preFilter[sn]
        }
        if (hdr$sHeaders$isAnnotation[sn]) {
            signals[[sn]]$annotations <- vector("list", length=hdr$nRecords)    # initialise for raw data per record
            class(signals[[sn]]) <- 'ebdfASignal'
        } else if (isContinuous) {
            maxTill <- min (DTillRS[sn], hdr$sHeaders$sLength[sn])
            l       <- max (0, maxTill-DFromRS[sn]+1)
            signals[[sn]]$signal <- integer (length=l)                          # initialise with 0 signal
            if (l==0) {
                signals[[sn]]$start         <- as.numeric (NA)
                signals[[sn]]$fromSample    <- as.numeric (NA)
            }
            class(signals[[sn]]) <- 'ebdfCSignal'
        } else {
            signals[[sn]]$rFragments <- vector("list", length=hdr$nRecords)     # initialise for raw data per record
            class(signals[[sn]]) <- 'ebdfFSignal'
        }
    }

    # read
    # cat ("Reading file", hdr$fileName, "signals, please wait.\n")
    sampleSize      <- hdr$sampleBits / 8

    nextCSample     <- 1
    for (rn in 1:hdr$nRecords) {
        # read record
        samples <- vector (mode='list', length=hdr$nSignals)
        for (sn in 1:hdr$nSignals) {                                            # read all signals (i.e. don't use a seek)
            n  <- hdr$sHeaders$samplesPerRecord[sn]
            # read all record data
            if (sampleSize ==2 & !hdr$sHeaders$isAnnotation[sn]) {              # ordinary 16 bits signal
                samples[[sn]] <- readBin (inFile, integer(), n=n, size=sampleSize,   signed=TRUE, endian="little")
            } else {                                                            # annotation or 24 bits
                samples[[sn]] <- readBin (inFile, integer(), n=n*sampleSize, size=1, signed=TRUE, endian="little")
            }
        }
        # get the start time
        recordStartRT <- hdr$recordDuration * (rn-1)
        if (isPlus) {
            annorecordStartRT <- getrecordStartRT (samples[[annotations1]])
            if (!hdr$isContinuous) {
                # if '+' & continuous the record duration is used instead
                # this shouldn't make a difference (but see check below)
                recordStartRT <- annorecordStartRT
            } else { # check for a rounding errror
                minSamplePeriod <- min(1/hdr$sHeaders$sRate)
                if (abs(recordStartRT - annorecordStartRT) >  minSamplePeriod/2) {
                    cat ("RECORD START ROUNDING ERROR: annotation start=", annorecordStartRT,
                         "Record * recordDuration=", recordStartRT, '\n')
                }
            }
        }
        # (pre)process the singals to be read
        for (sn in 1:hdr$nSignals) if (sgn[sn]) {
            if (hdr$sHeaders$isAnnotation[sn]) {
                signals[[sn]]$annotations[[rn]] <- samples[[sn]]
            } else {                                                            # an ordinary signal
                # set read boundaries for this record
                recordL <- hdr$sHeader$samplesPerRecord[sn]
                # get first sample since start recording
                if (isContinuous) recordStartRS <- (rn-1)*recordL + 1
                else              recordStartRS <- ceiling (recordStartRT * hdr$sHeader$sRate[sn]) + 1

                skipRecord      <- FALSE
                wholeRecord     <- TRUE
                rSignalStartRS  <- recordStartRS                                # if wholeRecord
                rSignalStartRT  <- recordStartRT
                if (from | till != Inf) {
                    fromDS      <- max (0, DFromRS[sn] - recordStartRS + 1)
                    tillDS      <- min (recordL, DTillRS[sn] - recordStartRS + 1)
                    skipRecord  <- (fromDS > recordL) | (tillDS <= 0)
                    wholeRecord <- (fromDS == 1) & (tillDS == recordL)
                    if (fromDS > 1) {
                        rSignalStartRS   <- recordStartRS + fromDS - 1
                        rSignalStartRT   <- (rSignalStartRS -1) / hdr$sHeaders$sRate[[sn]]
                    }
                }

                # convert sample values
                if (!skipRecord) {
                    # FIRST: for bdf, convert to integer; then samples[[sn]] will get the right length too
                    if (hdr$fileType == 'BDF') samples[[sn]] <- int1sToInt3s (samples[[sn]])
                    # truncate, if required
                    if (!wholeRecord) {
                        samples[[sn]] <- samples[[sn]] [fromDS:tillDS]
                    }
                    # convert digital to physical, if required
                    if (physical) {
                        samples[[sn]] <- hdr$sHeaders$gain[sn] * samples[[sn]] + hdr$sHeaders$offset[sn]
                    }
                    # add the samples to signals / rFragments
                    if (isContinuous) {
                        lastOne <- nextInCSignal[sn] + length (samples[[sn]]) -1
                        signals[[sn]]$signal[nextInCSignal[sn]:lastOne] <- samples[[sn]]  # copy to signals[[sn]]$signal
                        nextInCSignal[sn] <- lastOne + 1
                    } else {                                                    # not a continuous signal
                        signals[[sn]]$rFragments[[rn]] <- list (recordStartRT=recordStartRT, rSignalStartRS=rSignalStartRS,
                                                                rSignalStartRT=rSignalStartRT,signal=samples[[sn]])
                        # note the signal lenght depends on the sRate and may be 0 (due to from / till values)
                    }
                }
            }
        }
    }
    close(inFile)

    totalPeriod <- recordStartRT + hdr$recordDuration                           # recordStartRT = start last record
    if (till >= totalPeriod)  {
        for (sn in 1:hdr$nSignals) {
            DTillRT[sn] <- totalPeriod
            DTillRS[sn] <- ceiling (hdr$sHeader$sRate[sn] * DTillRT[sn])
        }

    }
    # process annotations
    for (sn in 1:hdr$nSignals) if (sgn[sn] & hdr$sHeaders$isAnnotation[sn]) {
        signals[[sn]]$annotations <- edfProcesAnnotations (
            hdr=hdr, ASignal=signals[[sn]],
            # hdr=hdr, ASignal=signals[[sn]]$annotations,from = from, till = till,
            isFirstASignal = sn==annotations1, recordStarts=recordStarts
        )
    }

    # process D signals
    nDSignals <- 0
    if (!isContinuous) {                                                        # i.e.  EDF/BDF-D file
        dSignals <- sgn & !hdr$sHeaders$isAnnotation                            # the DSignals
        nDSignals <- sum (dSignals)
    }
    if (nDSignals) {
        for (sn in 1:hdr$nSignals) if (dSignals [sn]) {

            # remove empty signal rFragments
            usedRFragments <- !sapply(signals[[sn]]$rFragments, is.null)
            signals[[sn]]$rFragments <- signals[[sn]]$rFragments[usedRFragments]

            # and check that no empty signals remain
            signalsL <- sapply (signals[[sn]]$rFragments, function(F) {length (F$signal)})
            noSignal <- signalsL == 0
            if (sum(noSignal)) cat ("readEdfSignal ERROR: Fragments with no signal !!!!!!!!!\n")

            if (!fragments) { # create a continuous signal with NAs
                tillRS <-DTillRS[[sn]]
                if (tillRS==Inf) tillRS <- totalPeriod
                signals[[sn]] <- fragmentsToSignal (signals[[sn]], DFromRT[[sn]], DFromRS[[sn]], tillRS)
            } else {  # fragmented, concatenate contigious DSignal rFragments
                signals[[sn]]$fragments <- concatenateFragments (signals[[sn]]$rFragments, hdr$recordDuration)
            }
            signals[[sn]]$rFragments <- NULL
        }
    }

    # save total period, recorded period (which may be part of the total period for EDF-D files)
    for (sn in 1:hdr$nSignals) {
        signals[[sn]]$recordedPeriod<- hdr$recordedPeriod                       # i.e. excluding gaps for dSignals
        signals[[sn]]$totalPeriod   <- totalPeriod                              # including gaps01
    }
    # return the signals requested
    signals <- signals [sgn]
    class (signals) <- c("ebdfSignals", "list")
    if (simplify & length (signals)==1) signals <- signals[[1]]
    signals
}
# ------------------------------------------------------------------------------
#                        supplementary functions
# ------------------------------------------------------------------------------
# Maps signal designations to signal indices
#
# The function maps a list a signal designations into an signal index vector.
# It also reports illegal designations.
#
# @param edf The edf object with the EDF file header
# @param signals='All' which may have one of the following values:
#  - 'All', to include all signals
#  - 'Signals', to include all signal signals only
#  - 'Annotations', to include all annotation signals only
#  - A vector with signal designations consisting of labels and/or signal numbers (numeric or as character)
# @return A list with two logical vectors:
#   - One to indicate the selected signals
#   - One to indicate erroneous signal values
#
# @keywords internal
edfProcessSignalDesignations <- function (hdr, signals='All') {
    errors      <- (rep (FALSE, length(signals)))
    indices     <- (rep (FALSE, hdr$nSignals))
    outOfBound  <- integer (length=0)
    for (sn in 1:length(signals)) {
        if      (signals[sn] == 'All')           indices[] <- TRUE
        else if (signals[sn] == 'Ordinary')      indices <- indices | !hdr$sHeaders$isAnnotation
        else if (signals[sn] == 'Annotations')   indices <- indices | hdr$sHeaders$isAnnotation
        else  {
            idx <- suppressWarnings (as.integer(signals[sn]))                   # try a (coerced) integer
            if (is.na(idx)) idx <- match (signals[sn], hdr$sHeaders$label)      # if not an integer, search for the name
            if (is.na (idx)) {
                errors [sn] <- TRUE
            } else if (idx>hdr$nSignals) {                                       # out of bound, report
                outOfBound <- c(outOfBound, idx)
            } else {                                                            # success, include
                indices [idx] <- TRUE
            }
        }
    }
    list (signals=indices, errors=errors, outOfBound=outOfBound)
}

int1sToInt3s <- function (int1s) {                                              # int1s: array of *signed* 1 byte integers
    ni <- length (int1s) / 3
    samples <- numeric(length = ni)
    for (i in 1:ni) {
        j <- i * 3                                                              # the last and most signifcant sample byte
        if (int1s[j-1] < 0)  int1s[j-1] <- int1s[j-1] + 256                     # correct the less significant int1s
        if (int1s[j-2] < 0)  int1s[j-2] <- int1s[j-2] + 256
        samples [i] <- int1s[j]*65536 + int1s[j-1]*256 + int1s[j-2]
    }
    samples
}
# ------------------------------------------------------------------------------
#                        Annotation signal functions
# ------------------------------------------------------------------------------
# get the start time for a data record
#
# The function retieves the first tal from (the first) annotation signal
#
# @param annotationSignal The first annoation signal from the data record
# @return The start time in seconds
# @keywords internal
getrecordStartRT <- function (annotationSignal) {
    endings <- which(annotationSignal==0)                                       # the TAL endings / separators 0
    tal1    <- annotationSignal [1:(endings[1]-1)]
    pt      <- parseTal (tal1, isRecordStart=TRUE)
    pt$onset
}

# Copies all annotations into a data frame
#
# The function copies all annotation in an 'EDF Annotations' signal
#     into a data frame.
#
# @param hdr The hdr object with the EDF / BDF file header
# @param signal The index of the signal with the annotations
# @return A list with the following values: record, onset, duration,
#      isRecordStart, annotations
#
# @keywords internal
edfProcesAnnotations <- function (hdr, ASignal, from=0, till=Inf, isFirstASignal, recordStarts) {
    nTals   <- 0
    from <- ASignal$from
    till <- ASignal$till
    annots <- ASignal$annotations

    for (rn in 1:hdr$nRecords) {                                                # for each record
        rnAnnots <- annots[[rn]]
        # remove trailing zero's
        i <- length(rnAnnots)
        while (rnAnnots[i]==0) i <- i-1
        rnAnnots  <- rnAnnots[1:(i+1)]                                          # keep last TAL delimiter
        # add the number of TALs endings, i.e. the number of '0's
        nTals   <- nTals + length (which(rnAnnots==0))
        annots[[rn]] <- rnAnnots
    }
    if (isFirstASignal & !recordStarts) nTals <- nTals -hdr$nRecords    # ommit the recordstart times
    # create empty data frame (possibly too  long because of a from - till range)
    annotations <- data.frame(record=integer(nTals), onset=numeric(nTals), duration=numeric(nTals),
                              isRecordStart=logical(nTals), annotations=character(nTals), stringsAsFactors=FALSE)
    nextAnnon <- 1
    for (rn in 1:hdr$nRecords) {                                                # for each record
        rnAnnots <- annots[[rn]]
        endings <- which (rnAnnots==0)                                          # the TAL endings / separators 0
        RTals   <- length (endings)

        fromChar    <- 1
        for (tn in 1:RTals) {
            tal  <- rnAnnots[fromChar:(endings[tn]-1)]                          # tal wich trailing 20
            fromChar <- endings[tn] +1
            isRecordStart <- isFirstASignal & tn==1
            if (!isRecordStart | recordStarts) {
                pt   <- parseTal (tal, isRecordStart=isRecordStart)
                if (from <=pt$onset & pt$onset < till) {
                    annotations$record[nextAnnon]       <- rn
                    annotations$onset[nextAnnon]        <- pt$onset
                    annotations$duration[nextAnnon]     <- pt$duration
                    annotations$isRecordStart[nextAnnon]<- isRecordStart
                    annotations$annotations[nextAnnon]  <- pt$annotations
                    nextAnnon <- nextAnnon +1
                }
            }
        }
    }
    if (nextAnnon == 1) {
        # remove if record == 0, i.e. remove the empty row
        annotations <- annotations[annotations$record, ]
    } else if (nextAnnon - 1 < nTals) annotations <- annotations[1:(nextAnnon - 1),]
    annotations
}

#  parses a TAL
#
# The function parses a TAL (Time-stamped Annotations List)
#
# @param tal A raw TAL from the edf / bdf file
# @param isRecordStart True if the TAL is the first one in a record, FALSE if not
# @return A list with the following values: onset, duration, annotations
# @keywords internal
parseTal <- function (tal, isRecordStart) {
    endings <- which(tal==20)                                                   # locate 'phrase' trailing delimiters '20'
    onsetPlusDuration <- tal[1:(endings[1]-1)]                                  # without trailing 20
    di <- which(onsetPlusDuration==21)                                          # locate start delimiter for 'duration'
    if (length(di)) {
        onset   <- as.numeric (intToUtf8 (onsetPlusDuration[1:(di-1)]))
        duration<- as.numeric (intToUtf8 (onsetPlusDuration[(di+1):length(onsetPlusDuration)]))
    } else {
        onset   <- as.numeric (intToUtf8 (onsetPlusDuration))
        duration<- NA
    }
    if (isRecordStart) {                                                        # the firts annoatation must be empty
        if (endings[1] +1 != endings[2]) {
            cat ('Illegal annotation signal, the start time annotation must be empty\n')
        }
    }
    l <-length (endings)
    if (l-1)    annotations <- character(l-1)                                   # l-1 = number of annotations
    else        annotations <- ''
    if (l>1) for (i in 2:l) {
        if (endings[i-1] == endings[i]-1) annotations[i-1] <- ''                # an empty annotation
        else annotations[i-1] <- intToUtf8 (tal[(endings[i-1]+1):(endings[i]-1)])
    }
    list (onset=onset, duration=duration, annotations=annotations)
}
# ------------------------------------------------------------------------------
#                          D signal functions
# ------------------------------------------------------------------------------
fragmentsToSignal <- function (fsignal, fromRT, fromRS, tillRS) {
    # replace the rFragment in fSignal
    fsignal$signal       <- fragmentsSignalToSignal (fsignal$rFragments, fromRS, tillRS)
    if (length(fsignal$signal)) {
        fsignal$start       <- fromRT
        fsignal$fromSample  <- fromRS
    }
    else {
        fsignal$start       <- as.numeric(NA)
        fsignal$fromSample  <- as.numeric(NA)
    }
    class(fsignal)       <- 'ebdfCSignal'
    fsignal
}

fragmentsSignalToSignal <- function (rFragments, fromRS, tillRS) {
    nSamples        <- max (0, tillRS - fromRS + 1)
    signal          <- numeric (length=nSamples)                                # memory check ??
    if (nSamples) {
        signal[1:nSamples] <- NA
        if (length(rFragments)) for (fn in 1:length(rFragments)) {
            rFragment   <- rFragments[[fn]]
            rSStartRS   <- rFragment$rSignalStartRS
            sFromFS     <- rSStartRS - fromRS + 1                               # from in from-till signal fragment
            sTillFS     <- sFromFS + length (rFragment$signal) - 1
            signal[sFromFS:sTillFS] <- rFragment$signal
        }
    }
    signal
}

concatenateFragments <- function (rFragments, recordD) {
    nOld <- length (rFragments)
    fragments <- vector("list", length=0)
    if (nOld) {
        oldToNew <- getOldToNew (rFragments, recordD)
        fragments <- getNewFragments (rFragments, oldToNew)
    }
    fragments
}

getOldToNew <- function (rFragments, recordD) {
    nOld        <- length (rFragments)
    oldToNew    <- integer (length=nOld)
    tol         <- 1e-6

    newCnt      <- 1
    oldToNew[1] <- newCnt
    if (nOld > 1) for (oldCnt in 2:nOld) {
        t1 <- rFragments[[oldCnt-1]]$rSignalStartRT + recordD
        t2 <- rFragments[[oldCnt  ]]$rSignalStartRT
        if (abs(t1 - t2) > tol) newCnt <- newCnt +1
        oldToNew [oldCnt] <- newCnt
    }
    oldToNew
}

getNewFragments <- function (rFragments, oldToNew) {                            # per signal
    nOld        <- length (oldToNew)
    nNew        <- oldToNew [nOld]

    oldFLength  <- sapply (rFragments, function (x){length(x$signal)} )
    newFragments<- vector("list", length=nNew)                                  # initialise for new fragments
    # class(newFragments) <- 'ebdfFragments'

    for (nfi in 1:nNew) {
        toCopyL <- oldToNew == nfi
        toCopyN <- which (toCopyL)

        newSignal   <- numeric (length = sum(oldFLength[toCopyL]))
        newFrom     <- 1
        for (j in toCopyN) {
            nextFrom <- newFrom + oldFLength[j]
            newSignal [newFrom:(nextFrom-1)] <- rFragments[[j]]$signal
            newFrom <- nextFrom
        }

        fromRT <- rFragments[[toCopyN[1]]]$rSignalStartRT
        fromRS <- rFragments[[toCopyN[1]]]$rSignalStartRS
        newFragments[[nfi]] <- list (start=fromRT, fromSample=fromRS, signal= newSignal)
    }
    newFragments
}
