#
# Purpose   :   Test the reading of complete .edf(+)/.bdf(+) files with edfReader
#
#               The readings are compared with the imported ASCII export
#               produced with EDFBrowser (by Teunis van Beelen)
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

require (testthat)
require (edfReader)

context ("Compare reading whole files with the export from EDFBrowser.")

libDir <- system.file("extdata", package="edfReader")
sampleDir <- paste (libDir, '/', sep='')
expDir    <- paste (libDir, '/', sep='')

sFns <- c('edfPlusC.edf', 'bdfPlusD.bdf')
# edfPlusC.edf is a trancated version of the test_generator_2.edf test file
# bdfPlusD.bdf is a trunacted version of the test_generator_2.edf test file and made discontinuous
# the original files canbe found at: http://www.teuniz.net/edf_bdf_testfiles

sFFns <- character (length = length(sFns))
for (i in 1:length(sFns)) sFFns[i] <- paste (sampleDir, sFns[i], sep='')

sHdrs <- vector (mode='list', length = length(sFns))
for (i in 1:length(sFns)) sHdrs[[i]] <- readEdfHeader(sFFns[i])

#                          importBrowserExport
# ------------------------------------------------------------------------------
importBrowserExport <- function (sFn) {
    fn       <- substring(sFn, 1, nchar(sFn)-4)
    hdrFFn   <- paste (expDir,fn, '_header.txt',      sep='')
    sHdrFFn  <- paste (expDir,fn, '_signals.txt',     sep='')
    annoFFn  <- paste (expDir,fn, '_annotations.txt', sep='')
    dataFFn  <- paste (expDir,fn, '_data.txt',        sep='')
    hdr      <- read.csv(hdrFFn,  stringsAsFactors = FALSE)
    hdr[is.na(hdr)] <-""                                                        # na.strings = "" in read.csv didn't seem to do thejob
    sHdr     <- read.csv(sHdrFFn, stringsAsFactors = FALSE)
    sHdr[is.na(sHdr)] <-""                                                      # na.strings = "" in read.csv didn't seem to do thejob
    anno     <- read.csv(annoFFn, stringsAsFactors = FALSE)
    # none specified, duration will be imported as logical
    anno$Duration <- as.numeric(anno$Duration)
    data     <- read.csv(dataFFn, stringsAsFactors = FALSE)
    list (header = hdr, sHeaders=sHdr, annotations=anno, signals=data)
}

#                          check header and sheader
# ------------------------------------------------------------------------------
checkHeader <- function (edfHdr, expHdr, sFn) {
    edfVersion <- edfHdr$version                                                # version
    oSignalsL <- !edfHdr$sHeaders$isAnnotation
    nOSignals <- sum (!edfHdr$sHeaders$isAnnotation)
    if (edfVersion == "\"255\"BIOSEMI") edfVersion <- ".BIOSEMI"
    that <- paste ("H1: ", sFn, ": Equal file headers (ex signal info)", sep='')
    test_that(that, {
        expect_equal (edfVersion, as.character (expHdr$Version))
        expect_equal (edfHdr$patient, trimws(expHdr$Subject))                   # patient
        expect_equal (edfHdr$recordingId, trimws(expHdr$Recording))             # recording id
        expect_equal (format (edfHdr$startTime, "%d.%m.%y"), expHdr$Startdate)  # start date
        expect_equal (format (edfHdr$startTime, "%H.%M.%S"), expHdr$Startime)   # start time
        expect_equal (edfHdr$headerLength, expHdr$Bytes)                        # header length in octets
        expect_equal (edfHdr$nRecords, expHdr$NumRec)                           # number of data records
        expect_equal (edfHdr$recordDuration, expHdr$Duration)                   # record duration
        if ( edfHdr$fileType == "EDF") {
            expect_equal (nOSignals, expHdr$NumSig)                             # number of signals
        } else {
            expect_equal (edfHdr$nSignals, expHdr$NumSig)                       # number of signals
        }
    })
}

checkSHeaders <- function (edfHdr, edfSHdr, expSHdr, sFn) {
    # EDFBrowser doesn't export sHeader data for annotation signals in EDF files
    # So, delete these rows form edfSHdr, if exist
    if ( edfHdr$fileType == "EDF" & sum (edfSHdr$isAnnotation)) {
        edfSHdr <- edfSHdr [!edfSHdr$isAnnotation, ]
    }

    # "," is the export field separator, therfore EDFBrowser changed it to"'"
    # So, apply the same change as EDFBrowser
    edfTranducer <- gsub(",", "'", edfSHdr$transducerType)

    that <- paste ("SH1: ", sFn, ": Equal signal headers", sep='')
    test_that (that, {
        expect_equal (nrow(edfSHdr), nrow(expSHdr))
        if (nrow(edfSHdr)) {
            expect_equal (edfSHdr$label, trimws(expSHdr$Label))                     # label
            expect_equal (edfTranducer, trimws(expSHdr$Transducer))                 # Transducer type
            expect_equal (edfSHdr$physicalDim, trimws(expSHdr$Units))               # Dimension
            expect_equal (edfSHdr$physicalMin, expSHdr$Min)                         # Physical min
            expect_equal (edfSHdr$physicalMax, expSHdr$Max)                         # Physical max
            expect_equal (edfSHdr$digitalMin, expSHdr$Dmin)                         # Digital min
            expect_equal (edfSHdr$digitalMax, expSHdr$Dmax)                         # Digital max
            expect_equal (edfSHdr$preFilter, trimws(expSHdr$PreFilter))             # pre filters
            expect_equal (edfSHdr$samplesPerRecord, expSHdr$Smp.Rec)                # Samples per record
            expect_equal (edfSHdr$reserved, trimws(expSHdr$Reserved))               # Reserved
        }
    })
}

#                          check signals
# ------------------------------------------------------------------------------
checkCSignals  <- function (edfHdr, edfSignals, expSignals, sFn) {
    cSignalsL <- !edfHdr$sHeaders$isAnnotation
    nCSignals <- sum(cSignalsL)
    cStoS     <- which (cSignalsL)   # ordinary Signal to Signal

    nExpSignals <- length(expSignals)-1
    if ( edfHdr$fileType == "BDF" & sum (edfHdr$sHeaders$isAnnotation)) {
        nExpSignals <- nExpSignals -  sum (edfHdr$sHeaders$isAnnotation)
    }

    if (nCSignals) {
        that <- paste ("C1: ", sFn, ": Equal number of ordinary signals", sep='')
        test_that (that, {
            expect_equal (nCSignals, nExpSignals)
        })

        eSignals <- vector (mode='list', length = nCSignals)
        for (sn in 1:nCSignals) {
            # skip first colum in expSignals (contains the time)
            # remove 'na' from ASCII / data frame colomn due to
            #   -  different sample rates and/or
            #   -  intermediate time only rows (for non-continuous signals)
            s <- expSignals[,(sn+1)]
            s <- s[!is.na(s)]
            eSignals[[sn]] <- s                                                 # exp signals to compare with
        }

#         if (edfHdr$fileType == 'BDF') {
#             tol <- 0.00000000000000000000000000000000001
#         } else {
#             tol <- 0.00000000000000000000000000000000001
#         }
        for (csn in 1:nCSignals) {
            sn      <- cStoS[csn]
            cSigAll <- edfSignals[[sn]]$signal
            # cat ("C2: length(cSigAll)=", length(cSigAll), "sum(is.na(cSigAll)=", sum(is.na(cSigAll)) ,'\n')
            cSig    <- cSigAll[!is.na(cSigAll)]

            that <- paste ("C2: ", sFn, ": Equal length for continuous signal ", sn, sep='')
            test_that (that, {
                expect_equal (length(cSig), length(eSignals[[csn]]))
            })

            that <- paste ("C3: ", sFn, ": (Almost) equal continuous signals for sn ", sn, sep='')
            test_that (that, {
                expect_equal (cSig, eSignals[[csn]])
                # expect_equal (cSig, eSignals[[csn]], tolerance=tol, scale = expectedValue)
            })
        }
    }
}
# checkCSignals  <- function (edfHdr, edfSignals, expSignals, sFn) {
#     cSignalsL <- !edfHdr$sHeaders$isAnnotation
#     nCSignals <- sum(cSignalsL)
#     cStoS     <- which (cSignalsL)   # ordinary Signal to Signal
#
#     nExpSignals <- length(expSignals)-1
#     if ( edfHdr$fileType == "BDF" & sum (edfHdr$sHeaders$isAnnotation)) {
#         nExpSignals <- nExpSignals -  sum (edfHdr$sHeaders$isAnnotation)
#     }
#
#     if (nCSignals) {
#         that <- paste ("C1: ", sFn, ": Equal number of ordinary signals", sep='')
#         test_that (that, {
#             expect_equal (nCSignals, nExpSignals)
#         })
#
#         eSignals <- vector (mode='list', length = nCSignals)
#         for (sn in 1:nCSignals) {
#             # skip first colum in expSignals (contains the time)
#             # remove 'na' from ASCII / data frame colomn due to
#             #   -  different sample rates and/or
#             #   -  intermediate time only rows (for non-continuous signals)
#             s <- expSignals[,(sn+1)]
#             s <- s[!is.na(s)]
#             eSignals[[sn]] <- s                                                 # exp signals to compare with
#         }
#
#         maxDiff <- numeric(length=nCSignals)
#         for (csn in 1:nCSignals) {
#             sn      <- cStoS[csn]
#             maxDiff[csn] <- abs(edfHdr$sHeaders$physicalMin[sn]) + abs(edfHdr$sHeaders$physicalMax[sn])
#         }
#         if (edfHdr$fileType == 'BDF') {
#             maxDiff <- maxDiff / 1000000000
#         } else {
#             maxDiff <- maxDiff / 100000000
#         }
#         for (csn in 1:nCSignals) {
#             sn      <- cStoS[csn]
#             cSigAll <- edfSignals[[sn]]$signal
#             # cat ("C2: length(cSigAll)=", length(cSigAll), "sum(is.na(cSigAll)=", sum(is.na(cSigAll)) ,'\n')
#             cSig    <- cSigAll[!is.na(cSigAll)]
#
#             that <- paste ("C2: ", sFn, ": Equal length for continuous signal ", sn, sep='')
#             test_that (that, {
#                 expect_equal (length(cSig), length(eSignals[[csn]]))
#             })
#             sDiff   <- cSig - eSignals[[csn]]
#             maxSDiff<- max (abs(sDiff))
#             if (maxSDiff >= maxDiff[csn]) {
#                 cat ("csn=", csn, "sn=", sn, "maxSDiff=", maxSDiff, " is >= then maxDiff[csn]=", maxDiff[csn], '\n')
#             }
#             that <- paste ("C3: ", sFn, ": (Almost) equal continuous signals for sn ", sn, sep='')
#             test_that (that, {
#                 expect_lt (maxSDiff, maxDiff[csn])
#             })
#         }
#     }
# }

checkDSignals  <- function (edfHdr, fSignals, cSignals, sFn) {
    fSignalsL       <- sapply (fSignals, class) == "ebdfDSignal"
    nOSignals       <- sum (fSignalsL)
    oStoS           <- which (fSignalsL)

    if (sum (fSignalsL)) {
        cSignalsL <- !edfHdr$sHeaders$isAnnotation
        that <- paste ("D1: ", sFn, ": Same signals being ordinary", sep='')
        test_that (that, {
            expect_equivalent(fSignalsL, cSignalsL)
        })

        for (sn in 1:nOSignals) {
            cSignal     <- cSignals[[oStoS[sn]]]$signal
            fragments   <- fSignals[[oStoS[sn]]]$fragments
            sRate       <- fSignals[[oStoS[sn]]]$sRate
            for (fn in 1:length(fragments)) {
                fStartRT    <- fragments[[fn]]$start
                fStartRS    <- fragments[[fn]]$fromSample
                fSignal     <- fragments[[fn]]$signal
                fTillRS     <- fStartRS + length (fSignal) - 1
                that <- paste ("D2: ", sFn, " sn=", sn, " fn=", fn, " fStartRS=" ,fStartRS,
                               " fStartRT=", fStartRT, ": Equal ordinary signals", sep='')
                test_that (that, {
                    # cat ("sn=", sn, 'fn=', fn, "fStartRS=", fStartRS, "length (fSignal)=", length (fSignal), '\n')
                    # cat ("f[1:6]  =", fSignal[1:6], '\n')
                    # cat ("c[part1]=", cSignal[fStartRS:(fStartRS+5)], '\n')
                    if (length (fSignal) > 200) {
                        # cat ("f[201:206]=", fSignal[1:6], '\n')
                        # cat ("c[part2]  =", cSignal[(fStartRS+200):(fStartRS+205)], '\n')
                    }
                    expect_equal (fStartRS, ceiling (fStartRT*sRate) + 1)
                    expect_equal (fSignal, cSignal[fStartRS:fTillRS])
                })
                cSignal[fStartRS:fTillRS] <- NA
            }
            that <- paste ("D3: ", sFn, ": No values left", sep='')
            test_that (that, {
                expect_equal (sum(!is.na(cSignal)), 0)  # all values should be checked
            })
        }
    }
}

checkAnnotations <- function (edfHdr, edfSignals, expAnnos, sFn) {
    annosL <- edfHdr$sHeaders$isAnnotation
    nAnnos <- sum(annosL)
    if (nAnnos & edfHdr$isContinuous) {
        expect_equal (nAnnos, 1)               # i.e. one annotation signals
        # note: it is unclear how EDFBrower exports more than 1 annotation signal, merged ?
        aToS <- which (annosL)
        for (sn in 1:nAnnos) {                 # be prepared for more signals
            annos <- edfSignals[[aToS[sn]]]
            that <- paste ("A1: ", sFn, " sn=", sn ,": Equal annotation signals", sep='')
            test_that (that, {
                expect_equal (nrow(expAnnos), nrow(annos$annotations))
                expect_equal (expAnnos$Onset, annos$annotations$onset)
                expect_equal (expAnnos$Duration, annos$annotations$duration)
                # EDFBrowser removes '"' characters
                expect_equal (expAnnos$Annotation, gsub('"', "", annos$annotations$annotations))
            })
        }
    }
}

#                              check files
# ------------------------------------------------------------------------------
checkFile <- function (n) {
    cat (sFns[n], ": Checking header of file\n")
    exp     <- importBrowserExport (sFns[n])
    hdr     <- readEdfHeader(sFFns[n])
    checkHeader   (hdr, exp$header, sFn=sFns[n])
    checkSHeaders (hdr, hdr$sHeaders, exp$sHeaders, sFn=sFns[n])

    cat (sFns[n], ": Checking signals of file with framents=FALSE\n")
    # continuous ordinary signals (with NA for ordinary signals in EDF-D files)
    cSignals <- readEdfSignals (hdr, fragments=FALSE, simplify=FALSE)
    # checkCSignals (hdr, cSignals, exp$signals, sFn=sFns[n])
    # checkAnnotations (hdr, cSignals, exp$annotations, sFn=sFns[n])

    cat (sFns[n], ": Checking signals of file with framents=TRUE\n")
    # fragments for ordinary signals in EDF-D files
    fSignals <- readEdfSignals (hdr, fragments=TRUE, simplify=FALSE)
    checkDSignals (hdr, fSignals, cSignals, sFn=sFns[n])
    checkAnnotations (hdr, fSignals, exp$annotations, sFn=sFns[n])               # check on side effects
}

checkAllFiles <- function () {
    checkFile (1)
    checkFile (2)
}

checkAllFiles()

