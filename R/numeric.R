#                              numeric functions
#
# Purpose   :   Internal nemeric utility functions
#
# Copyright :   (C) 2016-2018, Vis Consultancy, the Netherlands
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
# History   :
#     Feb18 - Imported for version 1.2.0 with superfluous functions removed
# ------------------------------------------------------------------------------
#         numeric details: spacing and distance beween successive values
#                           getSpacing and epsDistance
# -----------------------------------------------------------------------------
getNumericLowerSpacing <- function (x) {
    if (!is.numeric(x)) stop ("x must be numeric")
    lg2         <- log2 (abs(x))
    exp         <- floor (lg2)
    isPow2      <- lg2 == exp
    exp[isPow2] <- exp[isPow2] -1
    signifBits  <- .Machine$double.digits - 1
    2^(exp-signifBits)
}

# -----------------------------------------------------------------------------
#                conversion to numeeric/integer without warnings
# -----------------------------------------------------------------------------

asNumeric <- function (x) {
    n   <- as.numeric (NA)
    if (is.numeric(x)) n <- as.numeric (x)
    else if (is.character(x) | is.logical(x)) {
        WarnOptionSave <- getOption("warn")
        options (warn = -1)
        n <- as.numeric (x)
        options (warn = WarnOptionSave)
    }
    n
}

asInteger <- function (x) { 
    n           <- asNumeric(x)
    i           <- round (n) 
    i[i !=  n]  <- NA              # NA for non-integer values
    maxInt      <- .Machine$integer.max
    huge        <- n < -maxInt | n > maxInt
    i[huge]     <- NA
    as.integer(i)
}

# -----------------------------------------------------------------------------
#                               rounding decimals
# -----------------------------------------------------------------------------

roundDecimals <- function (x, signif=6) {
    n <- 10^signif - 1
    ifelse (abs(x) > n, round(x), signif (x, digits=signif))
}
