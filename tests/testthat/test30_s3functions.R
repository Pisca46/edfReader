#
# Purpose   :   Test supporting s3 function
#
#               The readings are compared with the imported ASCII export
#               produced with EDFBrowser (by Teunis van Beelen)
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
#               along with edf package for R.  If not, see <http://www.gnu.org/licenses/>.
#
# History    :
#   Feb18 - Created
#   Mar18 - For version 1.2.0
# ------------------------------------------------------------------------------

require (testthat)
require (edfReader)
context ("supporting s3 function.")

#                          test secEts
# ------------------------------------------------------------------------------
testsecEtc <- function () {
    secEtc      <- edfReader:::secEtc
    test_that ("secEts with all NA's", {
        expect_equal (secEtc (   NA     ), '')
        expect_equal (secEtc (c (NA, NA)), '')
    })
    test_that ("secEtc with some non-NAs", {
        t <- c (NA, 1, 2, 3, NA)
        expect_equal (secEtc (t), 'sec')      # with no translations
        t <- c (NA, 10, 310, 500, NA, 4)
        expect_equal (secEtc (t), "sec = NA 00:00:10 00:05:10 00:08:20 NA 00:00:04 h:m:s")
        t <- c (2*2592000, NA, 2592000, 2592000*1.4, NA, 2592000*20)
        expect_equal (secEtc (t), "sec = 00:01:29.5631 NA 00:00:30 00:01:11.5631 NA 01:07:21.6994 y:m:d")
    })
}

testAddDurCol <- function () {
    addDurCol  <- edfReader:::addDurCol
    qT      <- edfReader:::newQTable()
    test_that ("addDurCol with all NA's", {
        t   <- c (NA, NA, NA,NA, NA, NA)
        addDurCol (qT, 'item1', v=t)  
        expect_equal (qT$table[[1]]$h, 'item1') 
        expect_equal (qT$table[[1]]$v, t)
        expect_equal (qT$table[[1]]$u, 'sec')
    })
    test_that ("addDurCol with some non-NAs", {
        t   <- c (NA, 1, 2, 3, NA)
        v   <- as.character (t)
        addDurCol (qT, 'item2', v=t)  
        expect_equal (qT$table[[2]]$v, t) 
        expect_equal (qT$table[[2]]$u, 'sec')
        
        
        t   <- c (NA, 10, 310, 500, NA, 4)
        v   <- c(NA, "00:00:10", "00:05:10", "00:08:20", NA,  "00:00:04")
        addDurCol (qT, 'item3', v=t)  
        expect_equal (qT$table[[3]]$v, v)  
        expect_equal (qT$table[[3]]$u, 'h:m:s')  
        
        t   <- c (2*2592000, NA, 2592000, 2592000*1.4, NA, 2592000*20)
        v   <- c ("00:01:29.5631", NA, "00:00:30", "00:01:11.5631", NA, "01:07:21.6994")
        addDurCol (qT, 'item4', v=t)
        t <- c (2*2592000, NA, 2592000, 2592000*1.4, NA, 2592000*20)
        expect_equal (qT$table[[4]]$v, v)  
        expect_equal (qT$table[[4]]$u, 'y:m:d')
    })
}



testAll <- function () {
    testsecEtc ()
    testAddDurCol ()
}

testAll()
