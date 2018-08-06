#                           S3 function for edfReader
#
# Purpose   :   print and summarize functions for ebdf objects
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
#
# History    :
#   Oct15 - Created
#   Mar16 - Revised, version 1.1.0
#   Apr16 - version 1.1.1, no changes
#   May17 - Version 1.1.2, no changes
#   Feb18 - Based in part on 'printSupport.R'
#           print/summary signals revised, bug for discontiuous signals removed
#           diference between R and EDF signal number made explicit
#           MaxRows and file param added to print and summary functions 
#   Mar18 - version 1.2.0
# ------------------------------------------------------------------------------
#                           s3 header functions
#                      objects: ebdfHeader & ebdfSHeader
# ------------------------------------------------------------------------------

minHHmmss <- 300

#' @export
print.ebdfHeader <- function (x, ...) {                                         # excluding edf/bdf encoding data
    qL      <- newQLines()                                                            # quantity lines
    names   <- row.names(x$sHeaders)
    qL$add ('Patient'               , x$patient         )
    qL$add ('RecordingId'           , x$recordingId     )
    qL$add ('StartTime'             , x$startTime       )
    qL$add ('Continuous recording'  , x$isContinuous    )
    labels  <- x$sHeaders$label
    qL$add ('Labels'                , paste (labels, collapse = ', '))
    if (sum (labels != names)) {
        qL$add ('R signal names'    , paste (names , collapse = ', '))
        qL$add ('Note'              , 'duplicate label names')
    }
    ifs <- printQLines (qL, ...)
    invisible (ifs)
}

#' @export
summary.ebdfHeader <- function (object, ...) {
    x       <- object
    qL      <- newQLines()                                                      # quantity lines
    suffix  <- ifelse (x$isPlus, '+', '')
    ft      <- paste (x$fileType, suffix, sep='')
    aCh     <- sum (x$sHeaders$isAnnotation)
    labels  <- x$sHeaders$label
    names   <- row.names(x$sHeaders)

    qL$add ('File name'             , x$fileName        )
    qL$add ('File type'             , ft                )
    qL$add ('Version'               , x$version         )
    qL$add ('Patient'               , x$patient         )
    qL$add ('RecordingId'           , x$recordingId     )
    qL$add ('StartTime'             , x$startTime       )
    qL$add ('Continuous recording'  , x$isContinuous    )
    if (!is.na(x$recordedPeriod)) {
        qL$add ('Recorded period'   , x$recordedPeriod, etc = secEtc (x$recordedPeriod, ...))
    }
    qL$add ('Ordinary signals'      , x$nSignals - aCh  )
    qL$add ('Annotation signals'    , aCh               )
    qL$add ('Labels'                , paste (labels, collapse = ', '))
    if (sum (labels != names)) {
        qL$add ('R signal names'    , paste (names , collapse = ', '))
        qL$add ('Note'              , 'duplicate label names')
    }
    ifs <- printQLines (qL, ...)
    invisible (ifs)
}

#' @export
print.ebdfSHeaders <- function (x, ...) {
    qL      <- newQLines()
    labels  <- x$label
    names   <- row.names(x)
    qL$add ("Signal labels", paste(labels, collapse = ', '))
    if (sum (labels != names)) {                                                # different names, i.e. non-unique labels
        qL$add ("Signal names", paste(names, collapse = ', '))
        qL$add ("Note", "Duplicate label names")
    }
    ifs <- printQLines (qL, ...)
    invisible (ifs)
}

#' @export
summary.ebdfSHeaders <- function (object, maxRows=24, ...) {
    x       <- object
    names   <- row.names (x)
    qT      <- newQTable()

    qT$add (c('EDF', 'signal'), seq (to=nrow(x)))                       # edf file signalnumbering
    if (sum(x$label != names)==0) {
        qT$add ('label/name' , names )
    } else {
        qT$add ('label'       , x$label     )
        qT$add ('name'        , names       )
    }
    qT$add ('transducer'      , x$transducerType, na='n.a.')
    qT$add ('sample rate'     , x$sRate         , na='n.a.',  u='/sec')
    qT$add ('preFilter'       , x$preFilter     , na='n.a.')
    printQTable (qT, nObjects=nrow(x), maxRows=maxRows, ...)
}

# ------------------------------------------------------------------------------
#                            s3 lists of signals
#                            object: ebdfSignals
# ------------------------------------------------------------------------------
#' @export
print.ebdfSignals <- function (x, maxRows=24, ...) {
    if (length(x) == 1 ) print (x[[1]], maxRows=maxRows, ...)
    else  psSignals (signals=x, isSumm=FALSE, maxRows=maxRows, ...)
}

#' @export
summary.ebdfSignals <- function (object, maxRows=24, ...) {
    if (length(object) == 1 ) summary (object[[1]], maxRows=maxRows, ...)
    else psSignals (signals=object, isSumm=TRUE, maxRows=maxRows, ...)
}

psSignals <- function (signals, isSumm, maxRows, ...) {
    # structure:
    # common lines                              cL  psSignals
    # common o signal lines (c or d)            oL  addOsLines     
    # if 1 o   the o signal lines (c or d)      oL  addOsLines
    # else     o signal table                   oT
    # common a signal lines                     aL
    # if 1 a   the a signal lines               aL
    # else     an a singals table               aT
    # start  time
    cL   <- newQLines()
    cL$add ("Start time", signals[[1]]$startTime)
    
    aSignalsL <- sapply (signals, function(X) X$isAnnotation)
    maxOChars <- 0
    if (sum (!aSignalsL)) {                     # ordinary signals
        # ordinary signal common data
        oSignals    <- signals[!aSignalsL]
        oL          <- newQLines()
        addOsLines (oL, oSignals, isSumm=isSumm, ...)
        if (sum (!aSignalsL)==1) {              # only 1 o signal
            oT  <- NULL
        } else {
            oL$add (" Signals", '', itemOnly=TRUE)
            # ordinary signals table
            oT <- newQTable()
            addOsCols (oT, signals, isSumm=isSumm)
        }
        maxOChars   <- itemWidth (oL)
    }
    maxAChars <- 0
    if (sum (aSignalsL)) {
        aRN         <- which (aSignalsL)
        aSignals    <- signals[aSignalsL]
        aL          <- newQLines()
        addAsLines (aL=aL, aSignals=aSignals, aRN=aRN, isSumm=isSumm) 
        if (sum (aSignalsL)==1) {
            aT  <- NULL
        } else {
            aL$add (" Signals", '', itemOnly=TRUE)
            aT      <- newQTable()
            addAsCols  (aT, aSignals, aRN=aRN, isSumm=isSumm, ...)  
        }
        maxAChars   <- itemWidth (aL)
    }
    # get overall itemwidth and print
    itemWidth <- max (nchar("start time"), maxOChars, maxAChars)
    cL$itemWidth <- itemWidth
    printQLines (cL, ...)
    if (sum (!aSignalsL)) {
        oL$itemWidth <- itemWidth
        printQLines (oL, ...)
        if (!is.null(oT)) printQTable (oT, ...)
    }
    if (sum (aSignalsL)) {
        aL$itemWidth <- itemWidth
        printQLines (aL, ...)
        if (!is.null(aT)) printQTable (aT, ...)
    }
}

addOsLines <- function (oL, oSignals, isSumm, ...) {
    one     <- length (oSignals)==1
    caption <- ifelse (one,'Ordinary signal:', 'Ordinary signals:' )
    oL$add (caption, '', itemOnly=TRUE)
    if (one)  {
        oSignal <- oSignals[[1]]
        addSignalIdsL (qL=oL, signal=oSignal, isSumm=isSumm)
    }
    addOSRecordingPeriod (qL=oL, oSignal=oSignals[[1]], ...)
    addOSPeriodRead      (qL=oL, oSignal=oSignals[[1]], ...)
    isFragmented <- class (oSignals[[1]]) == 'ebdfFSignal'
    if (isFragmented) {
        oL$add (" Fragments per signal", length (oSignals[[1]]$fragments))
    }
    if (one) {
        oL$add (' Transducer', oSignal$transducer)
        oL$add (' Sample rate'     , oSignal$sRate, etc='/sec')
        oL$add (' Range'  , oSignal$range)
        oL$add (' PreFilter'  , oSignal$preFilter)
        if (oSignal$isContinuous) {                                     # continuously recorded
            oL$add (' Samples', length (oSignal$signal))
        } else if (!isFragmented) {                                     # read with NAs
            oL$add (' SignalLength',length (oSignal$signal))
            oL$add (' Samples', oSignal$nSamples)
        } else {
            samples <- sum (sapply (oSignal$fragments, function (X) length(X$signal)))
            oL$add (' Samples', samples)
        } 
    }
}

addOsCols <- function (qT, oSignals, isSumm) {
    isOSignal   <- sapply (oSignals, function(X) !X$isAnnotation)
    oSignals    <- oSignals[isOSignal] 
    isCont      <- oSignals[[1]]$isContinuous                      # continuously recorded
    isFragm     <- class (oSignals[[1]]) == 'ebdfFSignal'          # discontinuously recorded and read in fragments
    
    addSignalIdCols (qT=qT, signals=oSignals, isSumm=isSumm)
    qT$add ('transducer'      , sapply (oSignals, function(X) X$transducer))
    qT$add ('sampleRate'      , sapply (oSignals, function(X) X$sRate     ), u='/sec')
    qT$add ('preFilter'       , sapply (oSignals, function(X) X$preFilter ))
    if (isCont) {
        qT$add ('samples'     , sapply (oSignals, function(X) length(X$signal)))
    } else if (! isFragm) {
        qT$add ('signalLength', sapply (oSignals, function(X) length(X$signal)))
        qT$add ('samples'     , sapply (oSignals, function(X) X$nSamples      ))     
    } else {
        n       <- length (oSignals)
        samples <- integer (n)
        for (i in 1:n) {
            os          <- oSignals[[i]]
            samples[i]  <- sum (sapply (os$fragments, function (X) length(X$signal)))
        }
        qT$add ('samples'     , samples)  
    }
}

addAsLines <- function (aL, aSignals, aRN, isSumm, ...) {
    one     <- length (aSignals)==1
    caption <- ifelse (one,'Annotation signal:', 'Annotation signals:' )
    aL$add (caption, '', itemOnly=TRUE)
    if (one)  {
        aSignal <- aSignals[[1]]
        addSignalIdsL (qL=aL,  signal=aSignal, isSumm=isSumm)
        addAsLDetails (qL=aL, aSignal=aSignal, isSumm=isSumm, ...)
    } else {
        addAsPeriodRead (qL=aL, aSignals[[1]], ...)
    }
}

addAsCols <- function (qT, aSignals, aRN, isSumm, ...) {
    firstOnset  <- as.numeric (sapply (aSignals, function(X) X$annotsFrom))
    lastOnset   <- sapply (aSignals, function(X) X$lastOnset)
    lastEnd     <- sapply (aSignals, function(X) X$lastEnd)
    # numbers, names, starts
    addSignalIdCols (qT=qT, signals=aSignals, isSumm=isSumm)
    # other details
    qT$add (c("anno-", "tations"), sapply (aSignals, function(X) nrow (X$annotations)) )
    addDurCol (qT, c("first", "onset"  ), firstOnset, ...)
    addDurCol (qT, c("last" , "onset"  ), lastOnset, ...)
    addDurCol (qT, c("last" , "end"    ), lastEnd, ...)
}

# ------------------------------------------------------------------------------
#                           s3 single signal functions                       
#              objects: ebdfASignal, ebdfCSignal, and ebdfFSignal
# ------------------------------------------------------------------------------

#' @export
print.ebdfCSignal <- function (x, ...) {
    psOSignalCommon (oSignal=x, isSumm=FALSE)
}

#' @export
summary.ebdfCSignal <- function (object, maxRows=24, file='', ...) {
    psOSignalCommon (oSignal=object, isSumm=TRUE)
    cat ("Signal summary\n", file=file)
    qT <- newQTable()
    printNumericSummaries (qT, object$signal, maxRows=maxRows, fil=file, ...)
}

#' @export
print.ebdfFSignal <- function (x, ...) {
    psOSignalCommon (oSignal=x, isSumm=FALSE, ...)
}

#' @export
summary.ebdfFSignal <- function (object, maxRows=24, file='', ...) {
    psOSignalCommon (oSignal=object, isSumm=TRUE)
    cat ("Fragments summaries\n", file=file)
    printFragmentsSummaries (fSignal=object, maxRows=maxRows, file=file, ...)
    cat ("Whole recording\n", file=file)
    printAllFragmentsSummary (fSignal=object, maxRows=maxRows, file=file, ...)
}

psOSignalCommon <- function (oSignal, isSumm, ...) {
    qL <- newQLines() 
    addSignalIdsL        (qL, signal=oSignal, isSumm=isSumm) 
    qL$add (" Start time", oSignal$startTime)
    addOSRecordingPeriod (qL, oSignal=oSignal, ...)
    addOSPeriodRead      (qL, oSignal=oSignal, ...)
    
    qL$add (" Sample rate"         , oSignal$sRate           , etc='/sec'   )

    if (isSumm) {  # a summary
        qL$add (" Transducer"      , oSignal$transducerType  )
        qL$add (" Range"           , oSignal$range           )
        qL$add (" Prefilter"       , oSignal$preFilter       )
        qL$add (" Bits per sample" ,  oSignal$sampleBits     )
    }
    if (class (oSignal) == 'ebdfFSignal') {
        qL$add (" Number of fragments" , length (oSignal$fragments))
    } else {
        if (oSignal$isContinuous) {
            qL$add (" Number of samples"   , length (oSignal$signal))
        } else {
            qL$add (" Signal length"       , length    ( oSignal$signal), etc='incl. NAs')
            qL$add (" Number of samples"   , sum (!is.na(oSignal$signal)))
        }
    }
    printQLines (qL, ...)
}

printFragmentsSummaries <- function (fSignal, maxRows=24, file='', ...) {
    fragments   <- fSignal$fragments  
    nRows       <- length (fragments)
    # x <- sapply (fragments, function (X) X$signal)
    pRows       <- min (maxRows, nRows)
    x           <- vector (mode='list', length=pRows)
    for (i in 1:pRows) x[[i]] <- fragments[[i]]$signal
    qT <- newQTable()
    qT$add ('fragment', seq(1,pRows))
    printNumericSummaries (qT, x, maxRows=pRows, file=file, ...)
    rowsLeft    <- nRows - pRows
    if (rowsLeft > 0) cat ("...", rowsLeft, 'more fragment\n', file=file)
}

printAllFragmentsSummary <- function (fSignal, ...) {
    nFragments  <- length (fSignal$fragments)
    fLength     <- sapply (fSignal$fragments, function (X) length(X$signal))
    totalSignal <- numeric (length = sum(fLength))
    from        <- 1
    for (i in 1:length (fSignal$fragments)) {
        l <- fLength[i]
        totalSignal[from:(from+l-1)] <- fSignal$fragments[[i]]$signal
        from <- from + l
    }
    qT <- newQTable()
    printNumericSummaries (qT, totalSignal, ...)
}

printNumericSummaries <- function (qT, x, maxRows='', ...) {                    # ...may contain a file param
    if (is.numeric (x)) {
        x <- list (x)
    }
    # get summaries
    nRows   <- length (x)
    s1      <- summary (x[[1]])
    nCols   <- length (s1)
    hdrs    <- names (s1)
    m       <- matrix (nrow=nRows, ncol=nCols)
    m[1,]   <- as.numeric (s1)
    if (nRows > 1) for (i in 2:nRows)  m[i,] <- as.numeric (summary (x[[i]]))
    
    # print summaries
    m   <- roundDecimals(m, signif=6)
    for (i in 1:nCols) qT$add (hdrs[i], m[,i])
    printQTable (qT, maxRows=maxRows, ...)
}

#' @export
print.ebdfASignal <- function (x, ...) {
    psASignal (aSignal=x, isSumm=FALSE)
}

#' @export
summary.ebdfASignal <- function (object, ...) {
    psASignal (aSignal=object, isSumm=TRUE)
}

psASignal <- function (aSignal, isSumm, ...) {
    qL <- newQLines()
    aSum        <- getAnnotsSumASignal (aSignal)
    addSignalIdsL (qL=qL,  signal=aSignal, isSumm=isSumm) 
    qL$add (" Start time"            , aSignal$startTime   )
    addAsLDetails (qL=qL, aSignal=aSignal, isSumm=isSumm, ...)
    printQLines (qL, ...)
}

# ------------------------------------------------------------------------------
#                            supporting add functions
# ------------------------------------------------------------------------------
#                                   signal Ids 
# ------------------------------------------------------------------------------
addSignalIdsL <- function (qL, signal, isSumm) {            # used for single signal summary lines 
    # add signal numbers
    rN  <- as.character (signal$RSignalNumber)
    eN  <- paste (signal$signalNumber, collapse = ', ')
    if (!isSumm) {
        qL$add (' R signal', rN)
    } else if (eN == rN) {
        qL$add (" R / EDF signal", rN) 
    } else {
        qL$add (" R signal", rN) 
        qL$add (" EDF signal", eN) 
    }
    # add name / label
    label   <- signal$label
    name    <- signal$name
    if (!isSumm) {
        qL$add (" Name", name)
    } else if (label == name)  {                   
        qL$add (" Name / label", name)
    } else {                  
        qL$add (" Name", name)
        qL$add (" Label", label)   
    }
}

addSignalIdCols <- function (qT, signals, isSumm) {
    eN  <- sapply (signals, function(X) paste(X$signalNumber, collapse = ', '))
    rN  <- as.character (sapply (signals, function(X) X$RSignalNumber))
    if (!isSumm) {
        qT$add (c("R"    , "signal"), rN ) 
    } else if (sum (eN != rN) == 0) {
        qT$add (c("R/EDF", "signal"), rN )
    } else {
        qT$add (c("R"    , "signal"), rN )
        qT$add (c("EDF"  , "signal"), eN) 
    }
    # names / labels
    names   <- as.character (sapply (signals, function(X) X$name))
    labels  <- as.character (sapply (signals, function(X) X$label))
    if (!isSumm) {
        qT$add ("name"      , names ) 
    } else if (sum (names != labels) == 0) {
        qT$add ("name/label", names )
    } else {
        qT$add ("name"      , names )
        qT$add ("label"     , labels)
    }
}

#                                recording period 
# ------------------------------------------------------------------------------

addOSRecordingPeriod <- function (qL, oSignal, ...) {                                # does not make sense for annotations
    totPeriod   <- oSignal$totalPeriod
    isCont      <- oSignal$isContinuous
    
    qL$add (" Continuous recording", isCont )
    qL$add (" Recorded period"     , oSignal$recordedPeriod, etc = secEtc (oSignal$recordedPeriod, ...))
    if (!isCont) {
        qL$add (" Total period"    , oSignal$totalPeriod   , etc = secEtc (oSignal$totalPeriod   , ...)) 
    }
}
#                                   period read
# ------------------------------------------------------------------------------
addOSPeriodRead <- function (qL, oSignal, ...) {                                        
    totPeriod   <- oSignal$totalPeriod
    fromV       <- max (0,           oSignal$from)
    tillV       <- min (totPeriod, oSignal$till)
    fromStart   <- fromV == 0
    tillEnd     <- tillV == totPeriod
    wholeRec    <- fromStart & tillEnd
    if (!wholeRec) {
        if (fromStart) {
            fromV       <- 'start'
            fromEtc     <- ''
            # period = till, so don't print
            periodV     <- ''
            periodEtc   <- ''
            periodIO    <- TRUE
        } else {
            fromEtc     <- secEtc (fromV, ...) 
            # period 
            periodV     <- tillV - fromV
            periodEtc   <- secEtc (tillV - fromV, ...)
            periodIO    <- FALSE
        }
        if (tillEnd) {
            tillV       <- 'end'
            tillEtc     <- ''
        } else {
            tillEtc     <- secEtc (tillV, ...)
        }
    }
    
    if (wholeRec) {
        qL$add (" Period read" , "whole recording")
    } else {
        qL$add (" Period read"   , periodV  , periodEtc, itemOnly=periodIO )
        qL$add ("   from"        , fromV    , fromEtc)  
        qL$add ("   till"        , tillV    , tillEtc)  
    }
}

addAsLDetails <- function (qL, aSignal, isSumm, ...) {
    addAsPeriodRead (qL=qL, aSignal=aSignal, ...)
    if (isSumm) {
        qL$add (" Record start specs", nrow (aSignal$recordStartTimes))
    }
    qL$add (" Annotations", nrow (aSignal$annotations))
    qL$add (" First onset" , aSignal$annotsFrom, etc=secEtc(aSignal$annotsFrom, ...))
    qL$add (" Last onset"  , aSignal$lastOnset , etc=secEtc(aSignal$lastOnset , ...))
    qL$add (" Last end"    , aSignal$lastEnd, etc=secEtc(aSignal$lastEnd, ...))
    
}

addAsPeriodRead <- function (qL, aSignal, ...) {
    fromV       <- max (0, aSignal$from)
    tillV       <- aSignal$till
    wholeRec    <- fromV == 0 & tillV == Inf
    if (!wholeRec) {
        if (fromV == 0) {
            fromV       <- 'start'
            fromEtc     <- ''
        } else {
            fromEtc     <- secEtc (fromV, ...)
        }       
        if (tillV == Inf) {
            tillV       <- 'end'
            tillEtc     <- ''
        } else {
            tillEtc     <- secEtc (tillV, ...) 
        }
    }
    if (wholeRec) {
        qL$add (" Period read"     , "whole recording")
    } else {
        qL$add (" Annotation read if:", ''     , itemOnly=TRUE)
        qL$add ("   end from"         , fromV  , fromEtc)  
        qL$add ("   onset till"       , tillV  , tillEtc)
    }
}

addAsNAnnots <- function (qL, aSignal, isSumm) {
}

#                             other common functions
# ------------------------------------------------------------------------------

getAnnotsSumASignal <- function (aSignal) {
    oFrom   <- aSignal$from                     # observed from:  from parameter in readEdfSignals
    oTill   <- aSignal$till                     # observed till: till parameter in readEdfSignals
    annots  <- aSignal$annotations              # incl record starts
    n       <- nrow (annots)
    rss     <- nrow (aSignal$recordStartTimes)
    aStart  <- annots$onset[1]                  # onset first annotation
    dur     <- annots$duration      
    dur[is.na(dur)] <- 0
    aEnd    <- max (annots$onset+dur)           # end of last annotation (incl duration, if applicable)
    nAnnots <- nrow (aSignal$annotations) - rss
    wholeRec<- oFrom == 0  & oTill == Inf
    list (n=n, rss=rss, wholeRec=wholeRec, oFrom=oFrom, oTill=oTill, aStart=aStart, aEnd=aEnd)
}

secEtc <- function (t, hmsFrom=300, ymdFrom=2592000, ...) { # 5 minutes and 30 days
    l           <- length (t)
    NAs         <- is.na(t)
    if (l==sum(NAs)) return ('')    
    # some t not NA
    etcT        <- character (length=l)
    etcT[NAs]   <- as.character (NA)
    nonNAt      <- t[!NAs]
    if      (max(nonNAt) <= hmsFrom) {
        etc <- 'sec'
    }
    else if (max(nonNAt) <= ymdFrom) {
        etcT[!NAs]  <- numAsHms(nonNAt)
        etc         <- paste ("sec = ", paste (etcT,  collapse=' '), " h:m:s", sep='')
    }
    else {
        etcT[!NAs]  <- numAsYmd(nonNAt)
        etc         <- paste ("sec = ", paste (etcT,  collapse=' '), ' y:m:d', sep='')
    }
    etc
}

addDurCol <- function (qT, i, v, hmsFrom=300, ymdFrom=2592000, ...) {   # 5 minutes and 30 days
    l           <- length (v)
    NAs         <- is.na (v)
    if (l==sum(NAs)) {                                                          # all NA
        qT$add (i, v, u='sec')
    } else {
        # some v not NA
        colV        <- character (length=l)
        colV[NAs]   <- as.character (NA)
        nonNAv      <- v[!NAs]
        if (max (nonNAv) <= hmsFrom) {
            qT$add (i, v, u='sec')                 # no time conversions
        } else if (max (nonNAv) <= ymdFrom) {
            colV[!NAs]  <- numAsHms (nonNAv)
            qT$add (i, colV, u='h:m:s')
        } else {
            colV[!NAs]  <-  numAsYmd (nonNAv)
            qT$add (i, colV, u='y:m:d')
        }
    }
}

numAsYmd <- function (sec) {                    # an EDF sec with no NULL, NA, Inf or neg.
    n <- length (sec)
    if (n == 0 ) return ('')
    dPerY <- 365.2425                           # Gregorian
 
    # isolate NA's  (should be superfluous)  
    isNa        <- is.na (sec)
    aSec        <- sec [!isNa]
    isNeg       <- aSec < 0
    sgn         <- ifelse (isNeg, '-', '')
    aSec[isNeg] <- -aSec[isNeg]                 # make positive
       
    aDay        <- aSec / 86400                  # 60*60*24
    aDay        <- roundDecimals (aDay, signif=6) 
    
    
    year        <- aDay / dPerY
    yy          <- floor (year)
    month       <- (year-yy) * 12
    mm          <- floor (month)
    dd          <- (month - mm) * dPerY / 12
    ymd         <- sprintf ('%s%02d:%02d:%07.4f', sgn, yy, mm, dd) 
    ymd         <- gsub ('[.]?0*$', '', ymd)    # remove trailing '0's
    ymd
}

numAsHms <-function (sec) {
    # x <- as.duration (x)
    # format.duration (x, scaleAs='hhmmss', roundDecimals=NULL)
    n       <- length (sec)
    if (n == 0 ) return ('')
    
    hhmmss  <- character(length=n)
    if (is.character(sec))  sec <- asNumeric(sec)
    if (length(sec) == sum (is.na(sec)))                            return (hhmmss)
    sClass  <- class (sec)
    if (length (intersect (sClass, c('numeric', 'integer'))) == 0 ) return (hhmmss)
    
    # isolate NA's and sign
    isNa        <- is.na (sec)
    aSec        <- sec [!isNa]                                                  # assigned secs
    isNeg       <- aSec < 0
    sgn         <- ifelse (isNeg, '-', '')
    aSec[isNeg] <- -aSec[isNeg]
    if (!is.null(roundDecimals))    aSec    <- roundDecimals (aSec, signif=6)
    
    ss  <- aSec %%  60
    min <- aSec %/% 60
    mm  <- min  %%  60
    hh  <- min  %/% 60
    
    hms <- sprintf ('%02d:%02d:%09.6f', hh, mm, ss)
    hms <- gsub ('[.]?0*$', '', hms)                                            # remove trailing '0's and a '.', if any

    hhmmss[!isNa] <- paste (sgn, hms, sep='')
    hhmmss
}