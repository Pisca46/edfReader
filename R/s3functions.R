#                           S3 function for edfReader
#
# Purpose   :   print and summarize functions for ebdf objects
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
#
# History    :
#   Oct15 - Created
#   Jan16 - Revised, version 1.0.0
# ------------------------------------------------------------------------------
#                           s3 header functions
#                        objects: ebdfHeader & ebdfSHeader
# ------------------------------------------------------------------------------
# Print summary of an EDF/BDF file header
#
# Printing a brief summary of the data contained in an EDF/BDF file header.\cr
# For a less brief summary use summary(), for details use str().
#
# @param x An \code{\link{ebdfHeader}} object
# @param ... Ignored
#' @export
print.ebdfHeader <- function (x, ...) {                                         # excluding edf/bdf encoding data
    cat (' Patient               :', x$patient, '\n')
    cat (' RecordingId           :', x$recordingId, '\n')
    cat (' StartTime             :', format (x$startTime), '\n')
    cat (' Continuous recording  :', x$isContinuous, '\n')
    cat (' Signal labels         :', x$sHeaders$label, '\n')
}

# Summarizing an EDF/BDF file header
#
# Summary method for the data contained in an EDF/BDF file header.\cr
# For even more brief summary use print(), for details used str().
#
# @param object An \code{\link{ebdfHeader}} object
# @param ... Ignored
#' @export
summary.ebdfHeader <- function (object, ...) {
    suffix <- ifelse (object$isPlus, '+', '')
    ft     <- paste (object$fileType, suffix, sep='')
    cat (' File name             :', object$fileName, '\n')
    cat (' File type             :', ft, '\n')
    cat (' Version               :', object$version, '\n')
    cat (' Patient               :', object$patient, '\n')
    cat (' RecordingId           :', object$recordingId, '\n')
    cat (' StartTime             :', format (object$startTime), '\n')
    cat (' Continuous recording  :', object$isContinuous, '\n')
    if (!is.na(object$recordedPeriod)) {
        secPlus <- secPlusHHMMSS (object$recordedPeriod)
        cat (' Recording period      :', secPlus, '\n')
    }
    aCh <- sum (object$sHeaders$isAnnotation)
    cat (' Ordinary signals      :', object$nSignals - aCh, '\n')
    cat (' Annotation signals    :', aCh, '\n')
    cat (' Signal labels         :', object$sHeaders$label, '\n')
}

# Prints the labels of the signals in an EDF/BDF file
#
# Printing the labels of all signals in an EDF/BDF file.\cr
# For a less brief summary use summary(), for details use str().
#
# @param x An \code{\link{ebdfSHeader}} object
# @param ... Ignored
#' @export
print.ebdfSHeaders <- function (x, ...) {
    cat (' Signal labels         :', x$label, '\n')
}

# Summary of the singal data in the EDF/BDF file header
#
# Summary method for the signal data contained in an EDF/BDF file header.\cr
# For less details use print(), for all details used str().
#
# @param object An \code{\link{ebdfSHeader}} object
# @param ... Ignored
#' @export
summary.ebdfSHeaders <- function (object, ...) {
    sns <- seq (to=nrow(object))
    sdf <- data.frame (signal=sns, label=object$label, transducer=object$transducerType,
                       sampleRate=object$sRate, preFilter= object$preFilter, stringsAsFactors=FALSE)
    annons <- which(sdf$label=='EDF Annotations' | sdf$label=='BDF Annotations')
    if (length(annons)) {
        sdf$transducer[annons]  <- ' '
        sdf$sampleRate[annons]  <- NA
        sdf$preFilter[annons]   <- ' '
    }
    sdf
}
# ------------------------------------------------------------------------------
#                        s3 multiple signal functions
#                            object: ebdfSignals
# ------------------------------------------------------------------------------
# Print summary of the signals recorded in an EDF/BDF file
#
# Printing a brief summary of the signals recorded in an EDF/BDF file.\cr
# It is the same as summary(), for details use str().
#
# @param x An \code{\link{ebdfSignals}} object
# @param ... Ignored
#' @export
print.ebdfSignals <- function (x, ...) {
        printSummarySignals (x, print=TRUE)
}

# Summarizing the signal recorded in an EDF/BDF file
#
# Summary method for the signals recorded in an EDF/BDF file.\cr
# It is the same print(), for all details used str().
#
# @param object An \code{\link{ebdfSignals}} object
# @param ... Ignored
#' @export
summary.ebdfSignals <- function (object, ...) {
        printSummarySignals (object, print=FALSE)
}

printSummarySignals <- function (signals, print=TRUE) {
    asn  <- logical( length = length(signals))
    csn  <- asn
    dsn  <- asn
    for (sn in 1:length(signals)) {
        if (class (signals[[sn]]) == 'ebdfASignal') asn[sn] <- TRUE
        if (class (signals[[sn]]) == 'ebdfCSignal') csn[sn] <- TRUE
        if (class (signals[[sn]]) == 'ebdfFSignal') dsn[sn] <- TRUE
    }
    osn <- csn | dsn
    printSummaryCommonSignalsData      (signals, osn=osn, print=print)
    if (sum(asn)) printSummaryASignals (signals, asn=asn, print=print)
    if (sum(csn)) printSummaryCSignals (signals, csn=csn, print=print)
    # if (sum(asn)) printSummaryDSignals (signals, dsn=dsn, print=print)
}

printSummaryCommonSignalsData <- function (signals, osn, print=TRUE) {
    cat ("Continuous recording ", signals[[1]]$isContinuous,   '\n')
    if (!signals[[1]]$isContinuous) {
        cat ("Total recording time ", signals[[1]]$totalPeriod,    '\n')
    }
    cat ("Recorded period      ", signals[[1]]$recordedPeriod, '\n')
    till <- ifelse (signals[[1]]$till < signals[[1]]$totalPeriod, signals[[1]]$till, Inf)
    cat ("Recording segment    ", getSelectedSegmentText (signals[[1]]$from, till), '\n')
    if (sum(osn)) {
        cat ("bits per sample      ", signals[[1]]$sampleBits, '\n')
    }
}

printSummaryASignals <- function (signals, asn, print) {
    nr  <- sum(asn)
    cat (ifelse(nr==1,'Annotation signal:', 'Annotation signals:' ), '\n')
    sns  <- integer (length=nr)
    rss  <- integer (length=nr)
    os   <- integer (length=nr)
    r <- 1
    for (sn in 1:length(signals)) if (asn[sn]) {
        sns[r] <- signals[[sn]]$signalNumber
        rss[r] <- sum  ( signals[[sn]]$annotations$isRecordStart)
        os[r]  <- sum  (!signals[[sn]]$annotations$isRecordStart)
        # oA [r] <- nrow (signals[[sn]]$annotations) - rsA[r]
        r <- r +1
    }
    pdf <- data.frame (signal=sns, recordStartAnnotations=rss,otherAnnotations=os ,stringsAsFactors = FALSE)
    print.data.frame  (pdf)
}

printSummaryCSignals <- function (signals, csn, print) {
    nr  <- sum(csn)
    cat (ifelse(nr==1,'Ordinary signal:', 'Ordinary signals:' ), '\n')

    sns  <- integer   (length=nr)
    lbs  <- integer   (length=nr)
    tds  <- character (length=nr)
    srs  <- numeric   (length=nr)
    sms  <- integer   (length=nr)
    pfs  <- character (length=nr)
    r <- 1
    for (sn in 1:length(signals)) if (csn[sn]) {
        sns[r] <- signals[[sn]]$signalNumber
        lbs[r] <- signals[[sn]]$label
        tds[r] <- signals[[sn]]$transducer
        srs[r] <- signals[[sn]]$sRate
        sms[r] <- length(signals[[sn]]$signal)
        pfs[r] <- signals[[sn]]$preFilter
        r <- r +1
    }
    psdf <- data.frame (signal=sns, label=lbs, transducer=tds, sampleRate=srs, samples=sms, preFilter=pfs,stringsAsFactors = FALSE)
    print.data.frame  (psdf)
}
# ------------------------------------------------------------------------------
#                        s3 single signal functions
#            objects: ebdfASignal, ebdfCSignal, & ebdfFSignal
# ------------------------------------------------------------------------------
# Print summary of an annotation signal
#
# Printing a brief summary of an annotation signal recorded in an EDF+/BDF+ file.\cr
# For a less brief summary use summary(), for details use str().
#
# @param x An \code{\link{ebdfASignal}} object
# @param ... Ignored
#' @export
print.ebdfASignal <- function (x, ...) {
    rsn <- sum (x$annotations$isRecordStart)
    on  <- nrow (x$annotations) - rsn
    cat (" Signal number            ", x$signalNumber, '\n')
    cat (" Signal label             ", x$label, '\n')
    if (!x$isContinuous) {
        cat (" Total recording time     ", secPlusHHMMSS(x$totalPeriod), '\n')
    }
    cat (" Recorded period          ", secPlusHHMMSS(x$recordedPeriod), '\n')
    till <- ifelse (x$till < x$totalPeriod, x$till, Inf)
    cat (" Recording segment        ", getSelectedSegmentText (x$from, till), '\n')
    cat (" Record start annotations ", rsn, '\n')
    cat (" Other annotations        ", on,  '\n')
}

# Summarizing an annotation signal
#
# Summary method for an annotation signal recorded in an EDF+/BDF+ file.\cr
# For less details use print(), for all details used str().
#
# @param object An \code{\link{ebdfASignal}} object
# @param ... Ignored
#' @export
summary.ebdfASignal <- function (object, ...) {
    rsn <- sum (object$annotations$isRecordStart)
    on  <- nrow (object$annotations) - rsn
    cat (" Signal number            ", object$signalNumber, '\n')
    cat (" Signal label             ", object$label, '\n')
    if (class (object) == 'ebdfFSignal') {
        cat (" Total recording time  ", secPlusHHMMSS(object$totalPeriod), '\n')
    }
    cat (" Recorded period          ", secPlusHHMMSS(object$recordedPeriod), '\n')
    till <- ifelse (object$till < object$totalPeriod, object$till, Inf)
    cat (" Recording segment        ", getSelectedSegmentText (object$from, till), '\n')
    cat (" Record start annotations ", rsn, '\n')
    cat (" Other annotations        ", on,  '\n')
}

# Print summary of an ordinary signal continuously recorded
#
# Printing a brief summary of an ordinary signal continuously recorded in an EDF/BDF file.\cr
# For a less brief summary use summary(), for details use str().
#
# @param x An \code{\link{ebdfCSignal}} object
# @param ... Ignored
#' @export
print.ebdfCSignal <- function (x, ...) {
    printSummaryCommonOSignalInfo (oSignal=x)
}

# Summarizing an ordinary signal continuously recorded
#
# Summary method for an ordinary signal continuously recorded in an EDF/BDF file.\cr
# For less details use print(), for all details used str().
#
# @param object An \code{\link{ebdfCSignal}} object
# @param ... Ignored
#' @export
summary.ebdfCSignal <- function (object, ...) {
    printSummaryCommonOSignalInfo (oSignal=object)
    cat (" Signal summary:\n")
    summary (object$signal)
}

# Print summary of an ordinary signal stored in fragments

#' @export
print.ebdfFSignal <- function (x, ...) {
    printSummaryCommonOSignalInfo (oSignal=x)
}

# Summarizing an ordinary signal stored in fragments

#' @export
summary.ebdfFSignal <- function (object, ...) {
    printSummaryCommonOSignalInfo (oSignal=object)
    nFragments <- length (object$fragments)
    rows <- ifelse(nFragments>10, 5, nFragments)                                # 5 if > 10
    psdf <- getFragmentSummaries (object, rows)
    print (psdf)
    if (nFragments > rows) cat ("....", nFragments-rows, "more\n")
    cat ("All fragments:\n")
    print (getFragmentsSummary(object))
}

printSummaryCommonOSignalInfo <- function (oSignal) {
    cat (" Signal number         ", oSignal$signalNumber, '\n')
    cat (" Label                 ", oSignal$label, '\n')
    cat (" Continuous recording  ", oSignal$isContinuous, '\n')
    if (!oSignal$isContinuous) {
        cat (" Total recording time  ", secPlusHHMMSS(oSignal$totalPeriod), '\n')
    }
    cat (" Recorded period       ", secPlusHHMMSS(oSignal$recordedPeriod), '\n')
    till <- ifelse (oSignal$till < oSignal$totalPeriod, oSignal$till, Inf)
    cat (" Recording segment     ", getSelectedSegmentText (oSignal$from, till), '\n')
    if (class (oSignal) == 'ebdfFSignal') {
        cat (" Number of fragments   ", length (oSignal$fragments), '\n')
    } else {
        cat (" Number of samples     ", length (oSignal$signal), '\n')
    }
    cat (" Sample rate           ", oSignal$sRate, '\n')
    cat (" Transducer            ", oSignal$transducerType, '\n')
    cat (" Range                 ", oSignal$range, '\n')
    cat (" Prefilter             ", oSignal$preFilter, '\n')
}

getFragmentsSummary <- function (object) {
    nFragments  <- length (object$fragments)
    totalLength <- 0
    for (i in 1:nFragments) {
        totalLength <- totalLength + length(object$fragments[[i]]$signal)
    }
    totalSignal <- numeric (length = totalLength)
    from <- 1
    for (i in 1:nFragments) {
        l <- length(object$fragments[[i]]$signal)
        totalSignal[from:(from+l-1)] <- object$fragments[[i]]$signal
        from <- from + l
    }
    summary (totalSignal)
}

getFragmentSummaries <- function (object, rows) {
    n       <- integer(length=rows)
    starts  <- numeric(length=rows)
    samples <- integer(length=rows)
    mins    <- numeric(length=rows)
    q1s     <- numeric(length=rows)
    medians <- numeric(length=rows)
    means   <- numeric(length=rows)
    q3s     <- numeric(length=rows)
    maxs    <- numeric(length=rows)
    for (i in 1:rows) {
        n[i]        <- i
        starts[i]   <- object$fragments[[i]]$start
        samples[i]  <- length(object$fragments[[i]]$signal)
        summ        <- summary (object$fragments[[i]]$signal)
        mins[i]     <- summ ["Min."]
        q1s[i]      <- summ ["1st Qu."]
        medians[i]  <- summ ["Median"]
        means[i]    <- summ ["Mean"]
        q3s[i]      <- summ ["3rd Qu."]
        maxs[i]     <- summ ["Max."]
    }
    psdf <- data.frame (fragment=n, start=starts, samples=samples, "Min."=mins, "1st Qu."=q1s,
                        "Median"=medians, "Mean"=means, "3rd Qu."=q3s, "Max."=maxs, stringsAsFactors = FALSE)
    psdf
}

# ------------------------------------------------------------------------------
#                        common functions
# ------------------------------------------------------------------------------
getSelectedSegmentText <- function (from, till, startS=0) {
    if (from == 0 & till == Inf) {
        txt <- 'whole recording'
    } else if (from == 0) {
        txt <- paste ('from start till ', secPlusHHMMSS(till), sep='')
    } else {
        fs <- ""
        if (startS) fs <- paste (' (sample ', startS, ')', sep = '')
        if (till == Inf) {
            txt <- paste ('from ', secPlusHHMMSS(from), fs, ' till the end', sep='')
        } else {
            txt <- paste ('from ', secPlusHHMMSS(from), fs, ' till ', secPlusHHMMSS(till), sep='')
        }
    }
    txt
}

secPlusHHMMSS <- function (sec) {
    decimals <- round(sec) != sec
    hhmmss <- secToHHMMSS (sec)
    # remove trailing '0's and '.'
    csec <- as.character (sec)
    if (decimals) {
        n <- nchar(csec)
        while (substr(csec, n, n) == '0') n <- n-1
        if    (substr(csec, n, n) == '.') n <- n-1
        sec <- substr(csec, 1, n)
    }
    paste (csec, ' sec (= ', hhmmss, ')', sep='')
}

secToHHMMSS <- function (sec) {
    hhmmss <- ''
    if (sec) {
        m   <- sec %/% 60    # rounded to minutes
        hhmmss <- sprintf ('%02d:%02d:%09.6f', m%/%60, m%%60, sec%%60)
        # remove trailing '0's and '.'
        n <- nchar(hhmmss)
        if (n > 8) {
            while (substr(hhmmss, n, n) == '0') n <- n-1
            if    (substr(hhmmss, n, n) == '.') n <- n-1
            hhmmss <- substr(hhmmss, 1, n)
        }
    }
    hhmmss
}
