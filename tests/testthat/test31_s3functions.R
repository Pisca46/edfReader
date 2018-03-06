#                               print/summary support function
#
# Purpose   :   Test s3 functions print and summary
#
#
# Copyright (C) 2017-2018, Vis Consultancy, the Netherlands
#           This program is free software: you can redistribute it and/or modify
#           it under the terms of the GNU General Public License as published by
#           the Free Software Foundation, either version 3 of the License, or
#           (at your option) any later version.
#
#           This program is distributed in the hope that it will be useful,
#           but WITHOUT ANY WARRANTY; without even the implied warranty of
#           MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#           GNU General Public License for more details.
#
#           You should have received a copy of the GNU General Public License
#           along with edf package for R.  If not, see <http://www.gnu.org/licenses/>.
#
# History
#   Nov17 - Created
#   Dec17 - Version 0.1 (Dec 20), tested except for quantities
#             test added for logical columns; 
#   Jan18 - Header and POSIX printing tests added
#   Mar18 - For version 1.2.0, no changes
#
# Notes     Check the directory lines at the top; especially the line with 'tstPsDo'
#           see stdOutCopy() at the end
#           the project test dir is not copied to the package library dir
# ------------------------------------------------------------------------------

pDir        <- "/Users/Jan/Grinds/R/edfReader"

if (file.exists(pDir)) {                            # do not run on build_win, Travis, CRAN etc.
    refOutDir   <- paste (pDir, "tests/testthat/test31_s3functions_RefOut", sep='/')
    thisFile    <- paste (pDir, 'tests/testthat/test31_s3functions.R', sep='/')
    # NOTE the test dir is not copied to the package library dir 
    require (testthat)
    require (edfReader)
    
    edfFileDir  <- paste (pDir, 'inst/extdata' , sep='/') 
    AFile <- paste (edfFileDir, 'edfAnnonC.edf', sep='/') # a file with 2 annotation signals
    BFile <- paste (edfFileDir, 'bdfPlusC.bdf' , sep='/') # a continuously recorded BDF file
    CFile <- paste (edfFileDir, 'edfPlusC.edf' , sep='/') # a continuously recorded EDF file
    DFile <- paste (edfFileDir, 'edfPlusD.edf' , sep='/') # a discontinuously recorded EDF file
    
    AHdr        <- readEdfHeader (AFile)
    BHdr        <- readEdfHeader (BFile)
    CHdr        <- readEdfHeader (CFile)            
    DHdr        <- readEdfHeader (DFile)   
    # read signals
    ASignals    <- readEdfSignals (AHdr)
    BSignals    <- readEdfSignals (BHdr)
    CSignals    <- readEdfSignals (CHdr)
    DSignals    <- readEdfSignals (DHdr)
    DSignalsF   <- readEdfSignals (DHdr, fragments = TRUE)
    CSignals_S8 <- readEdfSignals (CHdr, signals=c(8, "8", "sine 8.5 Hz"))
    
    context ("S3 functions ")
    tstPsDo     <- 'check'  # supported values 'show' on screen and 'create' (superfluous)
    
    tstPrint    <- function (x, maxRows=24, ...) {   
        file <- paste (x, '_P.txt', sep='')
        args <- list (x=get(x), maxRows=maxRows, file=file, ...)
        tstPS ('print', args)
    }
    tstSumm     <- function (x, maxRows=24, ...) { 
        file <- paste (x, '_S.txt', sep='')
        args <- list (object=get(x), maxRows=maxRows, file=file, ...)
        # cat ("tstSumm:", "x=", x , "names(args)=", names(args), '\n')
        tstPS ('summary', args)
    }
    
    tstPS<- function (f, args, do=tstPsDo) {
        file        <- args$file                                # file name only
        ffn         <- paste (refOutDir, file, sep='/')
        args$file   <- ffn                                      # full file name now  
        if (do=='create') {
            if (file.exists(ffn)) file.remove (ffn)
            doPS (f, args, file)
        } else if (do=='check') {
            args$file   <- ''                                   # reset args full file name
            expect_known_output (doPS (f, args, file), ffn, update = TRUE, print=TRUE)
        } else if (do=='show'){
            args$file <- ''
            doPS (f, args, file)        
        } else {
            cat ("tstPS: do error /n")
        }
    }
    
    doPS <- function (f, args, file) {
        cat (' \nfile:', file, '\n', file=args$file)
        do.call (f, args=args)
    }
    
    # ------------------------------------------------------------------------------
    #                               edf (signal) header
    # ------------------------------------------------------------------------------
    tstPrint ('AHdr')
    tstSumm  ('AHdr')
    tstPrint ('DHdr')
    tstSumm  ('DHdr')
    
    # ------------------------------------------------------------------------------
    #                                lists of signals
    # ------------------------------------------------------------------------------
    # 5 / 6: lists with zero or one  ordinary / annotation signal
    ASignals1o1a    <- readEdfSignals (AHdr, signals=c(1,2))
    tstPrint ('ASignals1o1a')
    tstSumm  ('ASignals1o1a')
    # 7  /8
    DSignal1aF      <- readEdfSignals (DHdr, signals=1, fragments=TRUE, simplify=FALSE)
    tstPrint ('DSignal1aF')
    tstSumm  ('DSignal1aF')
    # 9 / 10
    DSignal2aF      <- readEdfSignals (DHdr, signals=c(1,2), fragments=TRUE)
    tstPrint ('DSignal2aF')
    tstSumm  ('DSignal2aF')
    # 11 / 12
    ASignals1aMerged    <- readEdfSignals (AHdr, signals='Annotations', simplify=FALSE)
    tstPrint ('ASignals1aMerged')
    tstSumm  ('ASignals1aMerged')
    # 13 / 14
    ASignalsNotMerged    <- readEdfSignals (AHdr, mergeASignals=FALSE)
    tstPrint ("ASignalsNotMerged")
    tstSumm  ("ASignalsNotMerged")
    
    ASignals1aMergedHms <- ASignals1aMerged
    tstPrint ('ASignals1aMergedHms', hmsFrom=.1)
    ASignalsNotMergedHms <- ASignalsNotMerged
    tstPrint ('ASignalsNotMergedHms', hmsFrom=.1)
    
    # ------------------------------------------------------------------------------
    #                                 a single signal
    # ------------------------------------------------------------------------------
    # ordinary continuous, whole recording
    BSignals_S4 <- BSignals[[4]]
    tstPrint ("BSignals_S4")
    tstSumm  ("BSignals_S4")
    
    # ordinary continuous, partially read
    tstPrint ("CSignals_S8")
    tstSumm  ("CSignals_S8")
    
    # ordinary not-continuous, whole recording
    DSignals_S4 <- DSignals[[4]]
    tstPrint ("DSignals_S4")
    tstSumm  ("DSignals_S4")
    
    # fragmented, whole recording
    DSignalsF_S4 <- DSignalsF[[4]]
    tstPrint ("DSignalsF_S4", maxRows = 4)
    tstSumm  ("DSignalsF_S4", maxRows = 5)
    
    # annotations, merged
    ASignals1aMerged_S1    <- readEdfSignals (AHdr, signals='Annotations')
    # tstPrint ("ASignals1aMerged_S1")
    tstSumm  ("ASignals1aMerged_S1")
    
    # annotations, not merged
    ASignalsNotMerged_S2 <- readEdfSignals (AHdr, mergeASignals=FALSE)[[2]]
    tstPrint ("ASignalsNotMerged_S2")
    tstSumm  ("ASignalsNotMerged_S2")
}

# ------------------------------------------------------------------------------
#                       how to use and and a std out copy
# ------------------------------------------------------------------------------
# the basic functions are tstPrint() and tstSumm() , see about line 50 
# the do parameter controls the operation, possible values are: 
#  'show'   to show the result on screen (or send to a sink), 
#  'create' to create new standard files, 
#  'check'  to check / compare the results against these standard files
stdOutCopy <- function () { # apply with do='show'
    fn      <- 'test30_s3functions.txt'
    path    <- "../edfReader.Private/published versions"
    ffn     <- paste (path, fn, sep='/') 
    sink (file=ffn, type='output')  # apply with do='show'
    cat (format (Sys.time()), '\n')
    source (thisFile)
    sink()
    cat ("copy", ffn, "to the rigth version directory\n")
}
