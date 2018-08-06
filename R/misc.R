#                               miscwllaneous function
#
# Purpose   :   a place for miscellaneus (noon-numice) basic utility function
#
# Copyright :   (C) 2017-2018, Vis Consultancy, the Netherlands
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
#     Feb18 - Imported for version 1.2.0
# ------------------------------------------------------------------------------
#                          parameter value processing
# ------------------------------------------------------------------------------
procesEnumertedParameter <- function (x, type) {
    p <- as.character (NA)
    if (!is.null(x) && length(x)==1 && is.character(x) && is.character(type)) {
        i <- grep (paste ('^', x, sep=''), type)
        if (length (i) == 1)    p <- type [i]
    }
    p
}