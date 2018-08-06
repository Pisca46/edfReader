#                      EDF(+)/BDF(+) file signals reader functions
#
# Purpose   :   Reading signals in  .edf(+)/.bdf(+) files
#
# Copyright :   (C) 2015-2018, Vis Consultancy, the Netherlands
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
#   Mar16 - Version 1.1.0 with:
#               - support for multiple annotation signals; multiple annatations per TAL
#               - annotations data.frame with only one annotation per row (resolves a bug)
#               - support sub second start specification in first data record (in + file)
#               - signal names copied from sHeaders
#   Apr16 - Version 1.1.1, no changes
#   May17 - Version 1.1.2, no changes
#   Nov17 - Annotation signals: onset within from-till changed into overlap with duration
#           dealt with consequences of NA values for annotation signals
#           R signal numbers (RSignalNumber) added to signal elements
#           nSamples (total number of samples) added for fragmented signals
#           improved margin for 'till' based on getNumericLowerSpacing()
#   Feb18 - reading a aSignal range without annotation improved; encoding="UTF-16LE" removed; 
#           RSignalNumber element relocated next to the signalNumber; 
#           superfluous and confusion isRecordStart removed from annotation data frame
#           onset of annotations corrected: old onset renamed to onsetHT, new onset equals onsetRT  
#   Mar18 - version 1.2.0
#
# Suffixes (not always used):
# HT        : header time, i.e. time in sec relative to start of recording as specified in the header.
# RT        : recording reference time, i.e. HT + the subsecond specified as start for the first EDF+ data start record
#             the 'fromm' and 'till' parameters are interpreted as RTs
# ST        : sample time, based on data record durartion and samples per record, origin equals RT origing
#             ST is used to also to check the correcness of data data record start times for  continuous recordings
# RS        : Sample number relative signal sampling startin at RT=0
#
# DS and DT : Data record Sample number en Time in seconds, i.e. relative to the start of a data record
# FS and FT : Fragment Sample number and Time in seconds, i.e. relative to the start of signal fragment
# L and D   : Length (number of samples) and duration (in seconds)
#
# prefixes indicate objects (not always used)
# wRec      : the whole recording
# dRec      : a data record  (using RT)
# recS      : a data record signal (using ST)
# sel       : the selection made with the from and till parameters
#
# ------------------------------------------------------------------------------
#                        read ordinary / annotation signals
# ------------------------------------------------------------------------------
#' Reads signals from an EDF(+)/BDF(+) file
#'
#' The function reads ordinary or annotation signals from an EDF(+)/BDF(+) file.
#' @param hdr An ebdfHeader object read with the readEdfHeader() function.
#' @param signals a vector with one or more of the following signal designations:
#'  'All' (default), to include all signals;
#'  'Ordinary', to include all ordinary signals;
#'  'Annotations', to include all annotation signals;
#'  signal labels and/or signal names; or
#'  signal numbers (numeric or as character).
#' @param from numeric, the time in seconds from which the signals shall be read.
#' @param till numeric, the time in seconds till which the signals shall be read.
#'   The value may exceed the total duration of the recoding.
#' @param physical logical, if TRUE (the default) digital samples values are mapped to their physical values,
#'   If not, the digital values are returned.
#' @param fragments logical, if TRUE discontinuously recorded signals are stored as a list of continuous
#'   fragments. If FALSE (the default), a signal is stored as one numeric vector with NA values filling the gaps.
#' @param recordStarts logical, if TRUE a data frame with the empty annotations with the data record start
#'  time will be included. If FALSE (the default), not.
#' @param mergeASignals logical, if TRUE all annotations will be merged into one data frame. If FALSE
#'   there will be one data frame per annotation signal.
#' @param simplify logical, if TRUE and if there is only one signal read, the signal itself is returned
#'   (in stead of a list with that signal as the only one element).
#'   If FALSE, this simplification is not performed.
#' @details
#'   For ordinary signals the from and till parameters are interpreted as [from, till).
#'   For annotation signals from-till has to overlap the onset-(onset+duration) period.
#'   For for details see the package vignette.
#' @return Either a list of one or more signals or a single signal. \cr
#' The list of signals returned is of class ebdfSignals and a single signal object is of one of the
#' following classes:
#' \itemize{
#'   \item ebdfASignal, for an annotation signal
#'   \item ebdfFSignal, for a fragmented ordinary signal
#'   \item ebdfCSignal, for a continuous ordinary signal (possible supplemented with NA values)
#' }
#' All classes have supporting print and summary functions.
#' For object details see the package vignette.
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
#' CSignals <- readEdfSignals (CHdr)            # to read all signals
#' # read 3 differently designated signals from 5.1 till 18 seconds period
#' someCSignalsPeriod <- readEdfSignals (CHdr, signals=c(3, "5", "sine 8.5 Hz"), from=5.1, till=18)
#' someCSignalsPeriod                           # print the signals
#' summary(someCSignalsPeriod)                  # print singals summary
#' someCSignalsPeriod$`sine 8.5 Hz`             # print the `sine 8.5 Hz` signal
#' summary(someCSignalsPeriod$`sine 8.5 Hz`)    # print a `sine 8.5 Hz` signal summary
#' str(CSignals$`sine 8.5 Hz`)                  # look to the details
#' # a discontinuous recording
#' DFile <- paste (libDir, '/edfPlusD.edf', sep='')
#' DHdr  <- readEdfHeader (DFile)
#' DSignals <- readEdfSignals (DHdr, fragments=TRUE)    # to read all signals
#' DSignals$`sine 8.5 Hz`                       # print fragmented signal
#' summary (DSignals$`sine 8.5 Hz`)             # print fragmented signal summary
#' str(DSignals$`sine 8.5 Hz`)                  # look to the details
#' @export
readEdfSignals <- function (hdr, signals='All', from=0, till=Inf, physical=TRUE,
                            fragments=FALSE, recordStarts=FALSE, mergeASignals=TRUE,
                            simplify=TRUE) {

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
    
    isPlus       <- hdr$isPlus
    isContinuous <- hdr$isContinuous
    
    # check and justify 'from' and 'till'
    if (from > till) {
        stop ("Illegal from-till range.")
    }
    if (isContinuous & from > hdr$recordedPeriod) {
        stop ("From after the continuously recorded period.")
    }
    
    isAnnotation    <- hdr$sHeaders$isAnnotation
    # check for annotation and non continuous signals in EDF+ and BDF+
    sgn          <- idxAndErr$signals                                           # logocal vector indicating signals to be returned
    oSignals     <- sgn & !isAnnotation                                         # the selected OSignals
    aSignals     <- sgn &  isAnnotation                                         # the selected ASignals
    # add record start times
    if (sum (isAnnotation)) {
        annotations1 <- which.max (isAnnotation)
        aSignals[annotations1] <- TRUE                                          # contains record start times 
    } else if (isPlus) {
        fp <- paste (hdr$fileType, '+', sep='')
        cat (fp, 'ERROR: required annotation signal is missing, signals will be retrieved as for an',
             hdr$fileType, 'file.\n')
        isPLus <- FALSE
        isContinuous <- TRUE
    }
    
    if (physical) {                                                             # conversion must be possible
        digitalOk   <- hdr$sHeaders$digitalMin  <  hdr$sHeaders$digitalMax
        physicalOk  <- hdr$sHeaders$physicalMin != hdr$sHeaders$physicalMax
        digitalErr  <- sum (!digitalOk [!isAnnotation])
        physicalErr <- sum (!physicalOk[!isAnnotation])
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

    # open file; if +C get start time first record and reopen; skip the header  NOTE : seek is not advised for Windows
    if (file.exists (hdr$fileName)) inFile <- file(hdr$fileName, "rb")          # encoding="UTF-16LE" removed; is standard
    else stop (paste ("File '", hdr$fileName, "' doesn't exists.", sep=''))
    readBin (inFile, 'raw', n=hdr$headerLength , size=1)                        # the header is not needed;

    startOfRecHT    <- hdr$startSecondFraction                                  # start of recording (as specified in first data record)

    # initialise signals
    signals <- vector (mode='list', length=hdr$nSignals)                        # new list with a element per signal
    names (signals) <- row.names (hdr$sHeaders)                                 # signalNames
    nextInCSignal   <- integer (length=hdr$nSignals) + 1                        # used for ordinary continuous signals only

    # intialise signal boundary parameters
    # aligned start of recording per signal
    wRecFromST      <- 0                                                        # aligned first recorded sample

    # aligned selection per signal
    selFromRT       <- max (0, from)
    selTillRT       <- rep (till, hdr$nSignals)
    if (isContinuous) {
        selTillRT   <- min (hdr$recordDuration * hdr$nRecords, till)
    }
    maxFromErr      <- 2 * getNumericLowerSpacing (selFromRT)                   # i.e. spacing between two successive numerical values
    selFromRS       <- ceiling (hdr$sHeaders$sRate * (selFromRT - maxFromErr)) +1# aligned first sample in selection per signal
    selFromST       <- (selFromRS-1) / hdr$sHeaders$sRate                       # aligned start of selection per signal
    if (!isContinuous && till == Inf)  {
        selTillRS   <- selTillRT
    } else {
        maxTillErr  <- 2 * getNumericLowerSpacing (selTillRT)
        selTillRS   <- ceiling (hdr$sHeaders$sRate * (selTillRT - maxTillErr)) # aligned last sample in selection per signal
    }  
    selTillST    <- (selTillRS-1) / hdr$sHeaders$sRate                         # aligned end of selection per signal

    # initialise data objects to be read
    for (sn in 1:hdr$nSignals) if (sgn[sn]) {                                   # => NULL if not in sgn
        signals[[sn]]$startTime     <- hdr$startTime                            # RT origin, i.e. incl startOfRecHT
        signals[[sn]]$signalNumber  <- sn
        signals[[sn]]$RSignalNumber <- 0                                        # to be set later (for str() readability)
        signals[[sn]]$label         <- hdr$sHeaders$label[sn]
        signals[[sn]]$name          <- row.names (hdr$sHeaders) [sn]
        signals[[sn]]$isContinuous  <- isContinuous
        signals[[sn]]$isAnnotation  <- hdr$sHeaders$isAnnotation[sn]
        if (oSignals[sn]) {
            signals[[sn]]$recordedPeriod<- hdr$recordedPeriod
        }
        signals[[sn]]$totalPeriod   <- as.numeric(NA)
        signals[[sn]]$from          <- from
        signals[[sn]]$till          <- till
        if (oSignals[sn]) {
            range <- paste (hdr$sHeaders$physicalMin[sn], " : ", hdr$sHeaders$physicalMax[sn],
                            ' ', hdr$sHeaders$physicalDim[sn], sep = '')
            signals[[sn]]$start         <- selFromST[sn]
            signals[[sn]]$fromSample    <- selFromRS[sn]
            signals[[sn]]$transducerType<- hdr$sHeaders$transducerType[sn]
            signals[[sn]]$sampleBits    <- hdr$sampleBits
            signals[[sn]]$sRate         <- hdr$sHeaders$sRate[sn]
            signals[[sn]]$range         <- range
            signals[[sn]]$preFilter     <- hdr$sHeaders$preFilter[sn]
        }
        if (aSignals[sn]) {
            signals[[sn]]$annotations <- vector("list", length=hdr$nRecords)    # initialise for raw data per record
            class(signals[[sn]]) <- 'ebdfASignal'
        } else if (isContinuous) {
            for (sn in 1:hdr$nSignals) if (oSignals[sn]) {
                l   <- max (0, selTillRS[sn] - selFromRS[sn] + 1)
                signals[[sn]]$signal <- integer (length=l)                      # initialise with 0 signal
                if (l==0) {
                    signals[[sn]]$start         <- as.numeric (NA)
                    signals[[sn]]$fromSample    <- as.numeric (NA)
                }
                class(signals[[sn]]) <- 'ebdfCSignal'
            }
        } else {   # +D signal
            signals[[sn]]$rFragments <- vector("list", length=hdr$nRecords)     # initialise for raw data per record
            class(signals[[sn]]) <- 'ebdfFSignal'
        }
    }

    # read
    # cat ("Reading file", hdr$fileName, "signals, please wait.\n")
    for (rn in 1:hdr$nRecords) {
        samples <- readNextDataRecord (hdr, inFile)                             # read next data record
        # get the start data
        dRecFromRT  <- hdr$recordDuration * (rn-1)                              # if continuous
        dRecFromRS  <- hdr$sHeaders$samplesPerRecord * (rn-1) + 1               # idem
        dRecFromST  <- rep (dRecFromRT, times= hdr$nSignals)                    # idem
        if (isPlus) {
            dRecFromHT <- getAnnoRecordStartRT (samples[[annotations1]])        # stated record start (in annotations1)
            if (hdr$isContinuous) {
                maxStartTDiff   <- 1e-8                                         # more tolerant, only used for a warning
                if ( abs(dRecFromHT - dRecFromRT -  startOfRecHT) > maxStartTDiff) {
                    cat ("WARNING: Ambiguous data record start:\n",
                         "- start according to annotation signal:", sprintf("%.10f", dRecFromHT - startOfRecHT), '\n',
                         "- start based on recordDuration:", sprintf("%.10f", dRecFromRT), '\n')
                }
            } else {                                                            # if not conintuous
                dRecFromRT  <- dRecFromHT - startOfRecHT
                maxFromErr  <- 2 * getNumericLowerSpacing (dRecFromRT)          # i.e. two successive numerical values
                dRecFromRS  <- ceiling (hdr$sHeader$sRate * (dRecFromRT - maxFromErr)) + 1  # a numeric vector
                dRecFromST  <- (dRecFromRS-1) / hdr$sHeaders$sRate              # sRate alligned sample time first sample
            } # if (hdr$isContinuous)
        } # if (isPlus)

        # (pre)process the signals selected
        for (sn in 1:hdr$nSignals) if (sgn[sn]) {
            if (hdr$sHeaders$isAnnotation[sn]) {
                signals[[sn]]$annotations[[rn]] <- samples[[sn]]                # NOTE, formally an annotation onset may not relate to the record start
            } else {                                                            # an ordinary signal
                # set read boundaries for this record
                sLength <- hdr$sHeader$samplesPerRecord[sn]                     # signal samples per record
                skipRecord      <- FALSE
                wholeRecord     <- TRUE
                drsFromRS       <- dRecFromRS[sn]                               # dataRecordSignal
                drsFrommST      <- dRecFromST[sn]
                if (selFromRS[sn] > 0 | till < Inf) {
                    fromDS      <- max (1, selFromRS[sn] -  drsFromRS + 1)
                    tillDS      <- min (sLength, selTillRS[sn] - drsFromRS + 1)
                    skipRecord  <- (fromDS > sLength) | (tillDS <= 0)
                    wholeRecord <- (fromDS == 1) & (tillDS == sLength)
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
                        fsFromRS    <- drsFromRS + fromDS - 1
                        fsFromST    <- (fsFromRS-1) / hdr$sHeaders$sRate[sn]
                        fsFromRT    <- max (dRecFromRT, selFromRT)
                        signals[[sn]]$rFragments[[rn]] <- list (fsFromST=fsFromST, fsFromRS=fsFromRS,
                                                                fsFromRT=fsFromRT,
                                                                drsFromRS=drsFromRS,
                                                                signal=samples[[sn]])
                    }  # if (isContinuous)
                } # if (!skipRecord)
            } # if (hdr$sHeaders$isAnnotation[sn])
        } # for (sn in 1:hdr$nSignals) if (sgn[sn]) 
    } # for (rn in 1:hdr$nRecords) 
    close(inFile)

    wRecTillRT  <- dRecFromRT + hdr$recordDuration
    wRecTillRS  <- dRecFromRS + hdr$sHeader$samplesPerRecord - 1                # recordStartRS = aligned first sample last record
    wRecTillST  <- wRecTillRS / hdr$sHeaders$sRate                              # = total recording period
    # save total period, recorded period (which may be part of the total period for EDF-D files)
    for (sn in 1:hdr$nSignals) if (sgn[sn]) {
        signals[[sn]]$totalPeriod       <- wRecTillRT                           # including gaps
    }

    # process annotations
    for (sn in 1:hdr$nSignals) if (sgn[sn] & hdr$sHeaders$isAnnotation[sn]) {
        a <- edfProcesAnnotations ( hdr=hdr, ASignal=signals[[sn]],
                                    isFirstASignal=sn==annotations1, recordStarts=recordStarts  
        )
        signals[[sn]]$annotations       <- a$annotations
        signals[[sn]]$recordStartTimes  <- a$recordStartTimes
    }
    # merge ASignals if requested
    if (mergeASignals) {
        annoSL <- hdr$sHeaders$isAnnotation & sgn
        if (sum(annoSL) > 1) {
            annoSN              <- which (annoSL)
            aSN1                <- which.max (annoSL)
            signals[[aSN1]]     <- doMergeASignals (signals[annoSN], annoSN)    # the RST, present, are already part of aSN1
            annoSL[aSN1]        <- FALSE
            sgn                 <- sgn & !annoSL

            names               <- names (signals)
            SN1name             <- substr (names[aSN1], 1, 15)                  # remove suffix
            names[aSN1]         <- SN1name
            names(signals)      <- names
            signals[[aSN1]]$name<- SN1name
        }
    }
    # sort annotations and add 'end' column
    annoSL <- hdr$sHeaders$isAnnotation & sgn
    if (sum(annoSL)) {
        for (sn in 1:hdr$nSignals) if (annoSL[sn]) {
            annots      <- signals[[sn]]$annotations
            if (nrow(annots) > 0) {
                annots      <- annots[order(annots$onset),]
                # cat ("readEdfSignals:", "length(annots)=", length(annots), "nrow(annots)=", nrow(annots), '\n')
                # cat ("readEdfSignals:", "annots$duration=", annots$duration, '\n')
                annots$end  <- annots$onset + annots$duration
                signals[[sn]]$annotsFrom    <- annots$onset[1]
                signals[[sn]]$lastOnset     <- annots$onset[nrow (annots)]
                ends                        <- annots$end
                nonNAs                      <- !is.na(ends)
                lastEnd                     <- as.numeric (NA)
                if (sum(nonNAs)) lastEnd    <- max (ends[nonNAs])
                signals[[sn]]$lastEnd       <- lastEnd
            } else {
                annots$end <- numeric()                                         # same columns
            }                          
            signals[[sn]]$annotations       <- annots
        }
    }

    # process D signals
    if (!isContinuous & sum(oSignals)) {                                        # i.e.  +D file with ordinary siganals
        for (sn in 1:hdr$nSignals) if (oSignals [sn]) {

            # remove empty signal rFragments
            usedRFragments <- !sapply(signals[[sn]]$rFragments, is.null)
            signals[[sn]]$rFragments <- signals[[sn]]$rFragments[usedRFragments]

            # and check that no empty signals remain
            signalsL <- sapply (signals[[sn]]$rFragments, function(F) {length (F$signal)})
            noSignal <- signalsL == 0
            if (sum(noSignal)) cat ("readEdfSignals ERROR: Fragments with no signal !!!!!!!!!\n")

            if (!fragments) { # create a continuous signal with NAs
                till <- min (wRecTillRS[sn], selTillRS[sn])
                signals[[sn]] <- fragmentsToSignal (signals[[sn]], selFromRT, selFromRS[sn], till)
            } else {  # fragmented, concatenate contiguous DSignal rFragments
                cFragments <- concatenateFragments (signals[[sn]]$rFragments, hdr$sHeaders$samplesPerRecord[sn])
                signals[[sn]]$fragments <- cFragments
                if (length(cFragments) > 0) {
                    signals[[sn]]$nSamples <- sum (sapply (cFragments, function(X) length(X$signal)))
                } else {
                    signals[[sn]]$nSamples <- 0
                }
            }
            signals[[sn]]$rFragments <- NULL
        }
    }

    # return the signals requested
    signals <- signals [sgn]
    for (i in 1:length(signals)) signals[[i]]$RSignalNumber <- i
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
    errorsL     <- logical (length=length(signals))
    indicesL    <- logical (length=hdr$nSignals)
    outOfBound  <- integer (length=0)
    for (sn in 1:length(signals)) {
        if      (signals[sn] == 'All')           indicesL[] <- TRUE
        else if (signals[sn] == 'Ordinary')      indicesL <- indicesL | !hdr$sHeaders$isAnnotation
        else if (signals[sn] == 'Annotations')   indicesL <- indicesL |  hdr$sHeaders$isAnnotation
        else  {
            idx <- suppressWarnings (as.integer(signals[sn]))                   # try a (coerced) integer
            if (!is.na(idx))
                if (idx <= hdr$nSignals) indicesL [idx] <- TRUE
                else                     outOfBound     <- c(outOfBound, idx)
            else {
                labels      <- hdr$sHeaders$label
                labelsL     <- labels == signals[sn]
                rNames      <- row.names(hdr$sHeaders)
                rNamesL     <- rNames == signals[sn]
                someFound   <- sum (labelsL) + sum (rNamesL)
                if (someFound) {
                    indicesL    <- indicesL | labelsL |  rNamesL
                } else{
                    errorsL[sn] <- TRUE
                }
            }
        }
    }
    list (signals=indicesL, errors=errorsL, outOfBound=outOfBound)
}

readNextDataRecord <- function (hdr, inFile) {
    samples     <- vector (mode='list', length=hdr$nSignals)
    sampleSize  <- hdr$sampleBits / 8
    for (sn in 1:hdr$nSignals) {                                                # read all signals (i.e. don't use a seek)
        n  <- hdr$sHeaders$samplesPerRecord[sn]
        # read all record data
        if (sampleSize ==2 & !hdr$sHeaders$isAnnotation[sn]) {                  # ordinary 16 bits signal
            samples[[sn]] <- readBin (inFile, integer(), n=n, size=sampleSize,   signed=TRUE, endian="little")
        } else {                                                                # annotation or 24 bits
            samples[[sn]] <- readBin (inFile, integer(), n=n*sampleSize, size=1, signed=TRUE, endian="little")
        }
    }
    samples
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
getAnnoRecordStartRT <- function (annotationSignal) {
    endings <- which(annotationSignal==0)                                       # the TAL endings / separators 0
    tal1    <- annotationSignal [1:(endings[1]-1)]
    pt      <- parseTal (tal1)
    pt$onsetHT
}

# Copies all annotations into a data frame
#
# The function copies all annotation in an 'EDF Annotations' signal
#     into a data frame.
#
# @param hdr The hdr object with the EDF / BDF file header
# @param signal The index of the signal with the annotations
# @return A list with two data frames: annotations and recordStartTimes
#
# @keywords internal
edfProcesAnnotations <- function (hdr, ASignal, isFirstASignal, recordStarts) {
    mergeRecordStarts <- FALSE                                                  # for future use, if at all. Require update of print annotaion signal(s)!!
    nAnnots <- 0
    nTals   <- 0
    nRSTs   <- 0
    maxFromErr    <- 2 * getNumericLowerSpacing (ASignal$from)                  # i.e. two successive numerical values
    maxTillErr    <- 2 * getNumericLowerSpacing (ASignal$till)
    from <- ASignal$from - maxFromErr                                           # to mitigate rounding errors
    till <- ASignal$till + maxTillErr
    annots <- ASignal$annotations                                               # raw record annotations

    for (rn in 1:hdr$nRecords) {                                                # strip & count for each record
        rnAnnots <- annots[[rn]]
        # remove trailing zero's
        i <- length(rnAnnots)
        while (i>0 && rnAnnots[i]==0) i <- i-1                                  #  !!!! or check for last '20' !!!!!!!!!!
        rnAnnots  <- rnAnnots[1:(i+1)]                                          # keep last TAL delimiter
        # add the number of TALs endings, i.e. the number of '0's
        if (length(rnAnnots) > 1) {
            nTals   <- nTals   + sum (rnAnnots==0)
            nAnnots <- nAnnots + sum (rnAnnots==20)                             # still needs adjustment
        }
        annots[[rn]] <- rnAnnots
    }
    # adjust nAnnots
    nAnnots <- nAnnots - nTals                                                  # per TAL: annots in between '20's
    nRSTs   <- ifelse (isFirstASignal, hdr$nRecords, 0)                         # number of record start time annotations
    if (!recordStarts) {                                                        # ommit the record start times, if any
        nAnnots <- nAnnots - nRSTs
        nRSTs   <- 0
    } else if (mergeRecordStarts) {
        nRSTs   <- 0                                                            # store with other annotations
    } else {
        nAnnots <- nAnnots - nRSTs                                              # store seperately
    }

    # create empty data frame (possibly too  long because of a from - till range)
    # cat ("edfProcesAnnotations: recordStarts", recordStarts, "nAnnots=", nAnnots, "nRSTs=", nRSTs, '\n')
    annotations <- data.frame(record=integer(nAnnots), onset=numeric(nAnnots),
                              duration=numeric(nAnnots), annotation=character(nAnnots),
                              stringsAsFactors=FALSE
                              )
    RSTs        <- data.frame(record=integer(nRSTs), startTime=numeric(nRSTs), stringsAsFactors=FALSE
    )
    nextAnnon   <- 1
    nextRST     <- 1
    for (rn in 1:hdr$nRecords) {                                                # for each record
        rnAnnots    <- annots[[rn]]
        endings     <- which (rnAnnots==0)                                      # the TAL endings / separators 0
        ending1     <- endings[1]
        fromChar    <- 1
        if (length(rnAnnots) > 1) for (it in endings) {
            tal         <- rnAnnots[fromChar:(it-1)]                            # tal which trailing 20
            fromChar    <- it + 1
            pt          <- parseTal (tal)
            onsetHT     <- pt$onsetHT
            onsetRT     <- onsetHT - hdr$startSecondFraction
            duration    <- pt$duration
            endRT       <- onsetRT + duration
            if (is.na (duration)) {
                include <- (from <= onsetRT & onsetRT <= till)
            } else {
                include <-  (onsetRT <= till & from  <= endRT)                  # overlap
            }
            if (include) {
                anns    <- pt$annotations
                for (ia in 1:length(anns)) {
                    isRecordStart <- isFirstASignal & it==ending1 & ia ==1
                    if (isRecordStart) {                                        # the firts annoatation must be empty
                        if (anns[ia]!="") {
                            cat ('Illegal annotation signal, a start time annotation must be empty\n')
                        }
                    }
                    if (!isRecordStart || recordStarts) {
                        # save the annottion
                        if (isRecordStart && !mergeRecordStarts) {
                            RSTs$record[nextRST]                <- rn
                            RSTs$startTime[nextRST]             <- onsetRT
                            nextRST <- nextRST+1
                        } else {
                            annotations$record[nextAnnon]       <- rn
                            annotations$onset[nextAnnon]        <- onsetRT
                            annotations$duration[nextAnnon]     <- duration
                            annotations$end[nextAnnon]          <- endRT
                            annotations$annotation[nextAnnon]   <- anns[ia]
                            nextAnnon <- nextAnnon +1
                        }
                        
                    }
                }
            }
        }
    }
    if (nextAnnon == 1) {
        # remove all rows
        annotations <- annotations[FALSE, ]
    } else {
        if (nextAnnon - 1 < nAnnots) annotations <- annotations[1:(nextAnnon-1),]
    }
    if (nextRST == 1) {
        #  remove all rows
        RSTs <-RSTs[FALSE, ]
    } else {
        if (nextRST - 1 < nRSTs) RSTs <- RSTs[1:(nextRST-1),]
    }
    # cat ("edfProcesAnnotations end: RSTs$record=", RSTs$record, '\n')
    # cat ("edfProcesAnnotations end: RSTs$onsetHT=" , RSTs$onsetHT , '\n')
    list (annotations=annotations, recordStartTimes = RSTs )
}

#  parses a TAL
#
# The function parses a TAL (Time-stamped Annotations List)
#
# @param tal A raw TAL from the edf / bdf file
# @param isRecordStart True if the TAL is the first one in a record, FALSE if not
# @return A list with the following values: onsetHT, duration, annotations
# @keywords internal
parseTal <- function (tal) {
    endings <- which(tal==20)                                                   # locate 'phrase' trailing delimiters '20'
    onsetPlusDuration <- tal[1:(endings[1]-1)]                                  # without trailing 20
    di <- which(onsetPlusDuration==21)                                          # locate start delimiter for 'duration'
    if (length(di)) {
        onsetHT     <- as.numeric (intToUtf8 (onsetPlusDuration[1:(di-1)]))
        duration    <- as.numeric (intToUtf8 (onsetPlusDuration[(di+1):length(onsetPlusDuration)]))
    } else {
        onsetHT     <- as.numeric (intToUtf8 (onsetPlusDuration))
        duration    <- NA
    }
    l <-length (endings)
    if (l-1)    annotations <- character(l-1)                                   # l-1 = number of annotations
    else        annotations <- ''
    if (l>1) for (i in 2:l) {
        if (endings[i-1] == endings[i]-1) annotations[i-1] <- ''                # an empty annotation
        else annotations[i-1] <- intToUtf8 (tal[(endings[i-1]+1):(endings[i]-1)])
    }
    list (onsetHT=onsetHT, duration=duration, annotations=annotations)
}

doMergeASignals <- function (ASignals, annoSN) {
    for (sn in 1:length(ASignals)) {
        ASignals[[sn]]$annotations$fromSignal <- annoSN[sn]                     # add source signal number
    }
    ASignal1 <- ASignals[[1]]
    mergedAnnos <- ASignal1$annotations
    l <- length(ASignals)
    if (l>1) for (i in 2:l) {
        mergedAnnos <- rbind (mergedAnnos, ASignals[[i]]$annotations)
    }
    ASignal1$annotations    <- mergedAnnos
    ASignal1$signalNumber   <- annoSN
    ASignal1
}

# ------------------------------------------------------------------------------
#                          D signal functions
# ------------------------------------------------------------------------------
fragmentsToSignal <- function (fsignal, sSelFromRT, sSelFromRS, sSelTillRS) {
    fsignal$signal      <- fragmentsSignalToSignal (fsignal$rFragments, sSelFromRS, sSelTillRS)
    fsignal$nSamples    <- sum (!is.na (fsignal$signal))
    if (length(fsignal$signal)) {
        fsignal$start       <- sSelFromRT
        fsignal$fromSample  <- sSelFromRS
    }
    else {
        fsignal$start       <- as.numeric (NA)
        fsignal$fromSample  <- as.numeric (NA)
    }
    class(fsignal)  <- 'ebdfCSignal'
    fsignal
}

fragmentsSignalToSignal <- function (rFragments, sSelFromRS, sSelTillRS) {
    signal      <- integer (length=0)
    nSamples    <- max (0, sSelTillRS - sSelFromRS + 1)
    if (nSamples) {
        signal          <- rep (as.numeric(NA), nSamples)                       # memory check ??
        if (length(rFragments)) for (fn in 1:length(rFragments)) {
            rFragment   <- rFragments[[fn]]
            fStartRS    <- rFragment$fsFromRS
            fFromFS     <- fStartRS - sSelFromRS + 1                            # from in from-till signal fragment
            fTillFS     <- fFromFS + length (rFragment$signal) - 1
            signal[fFromFS:fTillFS] <- rFragment$signal
        }
    }
    signal
}

concatenateFragments <- function (rFragments, recordL) {
    nOld <- length (rFragments)
    fragments <- vector("list", length=0)
    if (nOld) {
        oldToNew    <- getOldToNew (rFragments, recordL)
        fragments   <- getNewFragments (rFragments, oldToNew)
    }
    fragments
}

getOldToNew <- function (rFragments, recordL) {
    nOld        <- length (rFragments)
    oldToNew    <- integer (length=nOld)
    tol         <- 1e-6

    newCnt      <- 1
    oldToNew[1] <- newCnt
    if (nOld > 1) for (oldCnt in 2:nOld) {
        s1  <- rFragments[[oldCnt-1]]$drsFromRS + recordL
        s2  <- rFragments[[oldCnt  ]]$drsFromRS
        if (s1 != s2) newCnt    <- newCnt +1
        oldToNew [oldCnt]       <- newCnt
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
            nextFrom    <- newFrom + oldFLength[j]
            newSignal [newFrom:(nextFrom-1)] <- rFragments[[j]]$signal
            newFrom     <- nextFrom
        }

        fromST  <- rFragments[[toCopyN[1]]]$fsFromST
        fromRS  <- rFragments[[toCopyN[1]]]$fsFromRS
        fromRT  <- rFragments[[toCopyN[1]]]$fsFromRT
        newFragments[[nfi]] <- list (start=fromST, fromSample=fromRS,
                                     recordingStart=fromRT, signal= newSignal)
    }
    newFragments
}
