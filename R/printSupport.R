#                        S3 print / summary functions support
#
# Purpose   :   rendering for print and summarize functions
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
#
# History   :
#     Feb18 - Imported for version 1.2.0 with all functions made internal
#
# Legend:
# qL    - a number of quntity lines
# qT    - a quantity table
# q     - a list with a single line / column
# v     - q$v; a vector with quantities / values
# txt   - a character vector with the text for v

# @keywords internal

# ------------------------------------------------------------------------------
#                 create a new lines / table printing environment
# ------------------------------------------------------------------------------

newQLines <- function () {
    q           <- new.env()
    q$type      <- 'lines'
    q$lines     <- list()
    add         <- function (i, v, etc='', null='NULL', skipAllNull=TRUE, na='NA', zero=0, pInf='Inf', mInf='-Inf', itemOnly=FALSE) {
        line <- list (i=i, v=v, etc=etc, null=null, skipAllNull=skipAllNull, na=na, zero=zero, pInf=pInf, mInf=mInf, itemOnly=itemOnly)
        q$lines[[ length(q$lines)+1]] <<- line
    }
    q$add       <- add
    q
}

newQTable <- function () {
    q           <- new.env()
    q$type      <- 'table'
    q$table     <- list()
    q$add       <- function (h, v, u=NULL, null='NULL', skipAllNull=TRUE, na='NA', zero='0', pInf='Inf', mInf='-Inf', true='x', false='',allign='default') {
        col     <- list (h=h, v=v, u=u, null=null, skipAllNull=skipAllNull, na=na, zero=zero, pInf=pInf, mInf=mInf, true=true, false=false, allign=allign)
        q$table[[length(q$table)+1]] <<- col
    }
    q
}

# ------------------------------------------------------------------------------
#                                 print QLines
# ------------------------------------------------------------------------------

printQLines <- function (qL, file='', ...) {
    if (length (qL$lines) > 0)  {
        toBeSkipped <- allNullsToBeSkipped (qL)
        if (sum(toBeSkipped)) qL$lines <- qL$lines [!toBeSkipped]
    }
    if (length (qL$lines) == 0) return (invisible(NULL))
    # determine items width
    maxChars    <- qL$itemWidth                                 # a given value
    if (is.null (maxChars)) maxChars <- itemWidth (qL)
    ifs         <- paste ("%-", maxChars, "s :", sep='')        # item format string
    # process lines
    for (line in qL$lines) {
        if (line$itemOnly) {
            cat (line$i, '\n')
            next
        }
        if ('POSIXlt' %in% class(line$v)) line$v <- as.POSIXct (line$v)
        else if (class(line$v)[1] =='list') line$v <- convertPOSIXltListValues (line$v)
        lV          <- length(line$v)
        if (lV == 0) {
            txt     <- line$null
        } else {
            txt         <- character (length(line$v))           # v may be a vector
            line        <- processNullVals   (line)
            if (is.null(line)) next
            nn          <- ! line$isNull                        # not Null
            txt[!nn]    <- line$null
            # non NULL non std values
            if (sum(nn)) {
                line           <- processNonStdVals (line)
                txt[nn][line$nnIsNa]       <- line$na
                txt[nn][line$nnIsZero]     <- line$zero         # non '0' replacements only
                txt[nn][line$nnIsPInf]     <- line$pInf
                txt[nn][line$nnIsMInf]     <- line$mInf
                txt[nn][line$nnIsNaN]      <- "NaN"

                # std Values
                if (sum(line$nnIsStd)) {
                    stdV                    <- line$nnV[line$nnIsStd]
                    stdTxt                  <- processStdValues (stdV)$stdTxt
                    txt[nn][line$nnIsStd]   <- stdTxt
                }
            }
        }
        cat (sprintf (ifs, line$i), txt, line$etc, '\n', file=file, append=TRUE)
    }
    invisible (NULL)
}

itemWidth <- function (qL) {
    lines       <- qL$lines
    items       <- as.character (sapply (lines, function(X) X$i))
    iOnlyLines  <- as.logical (sapply (lines, function (X) X$itemOnly==TRUE))
    max (nchar (items [!iOnlyLines]))
}

convertPOSIXltListValues <- function (v) {
    # if (length(class (v)) > 1) cat ("convertPOSIXltListValues:", "class (v)=", class (v), '\n')
    if ('list' %in% class (v)) for (i in 1:length (v)) {
        if ('POSIXlt' %in% class(v[[i]])) {
            v[[i]] <- as.POSIXct(v[[i]])
        }
    }
    v
}

processLineUnit <- function (line) {
    u   <- line$u
    if (is.null (u))    u <- 'unit'
    if (u=='unit' || u=='symbol') {                             # try to find a quantity unit
        u <- ''
    } else {
        # a unit value to be used
        if (!is.character(u)) stop ("u must be of type character")
    }
    u       <- trimws(u)
    line$u  <- u
    line
}

# ------------------------------------------------------------------------------
#                                  print QTable
# ------------------------------------------------------------------------------

printQTable <- function (qT, maxRows=24, file='', ...) {
    nCols   <- length (qT$table)
    if (nCols) {
        toBeSkipped <- allNullsToBeSkipped (qT)
        if (sum(toBeSkipped)) qT$table <- qT$table [!toBeSkipped]
    }
    table   <- qT$table
    if (length(table) == 0) return (NULL)

    table   <- processTableUnits (table)

    # someUnits   <- as.logical (sum (sapply (table, function(X) X$u != '')))
    hRows   <- max (sapply (table, function (X) length(X$h)))
    table   <- correctCollapsedNullColums (table)
    # check value vector length and max number of rows
    vRows   <- sapply (table, function(X) length (X$v))
    if (sum(vRows != vRows[1])) stop ("All columns (value vectors) must have the same length.")
    # convert POSIXlt (a list) to POSIXct
    table <- convertPOSIXltTableValues (table)
    vRows   <- vRows[1]
    pRows   <- min (vRows, maxRows)
    oRows   <- vRows - pRows
    if (oRows) {
        vRows <- maxRows
        for (c in 1:length(table)) {
            table[[c]]$v  <- table[[c]]$v[1:vRows]       # truncate value vector
            # remove now: values not printed should not have any impact on colomn width.
        }
    }

    cols    <- list()
    for (col in table) {
        colTxt <- processTableColumn (col, hRows=hRows)
        if (!is.null (colTxt)) cols[[length(cols)+1]] <- colTxt
    }
    if (length(cols)) for (r in 1:(hRows+pRows)) {              # plus header lines
        rowParts <- sapply (cols,function (X) X[r])
        cat (' ', rowParts, '\n', sep='', file=file, append=TRUE)
    }

    if (oRows) cat ("... and", oRows, 'more\n', file=file, append=TRUE)
}

processTableUnits <- function (table) {
    for (col in 1:length (table)) {
        u   <- table[[col]]$u
        if (is.null(u)) u  <- 'symbol'
        if (!is.character(u)) stop ("u must be of type character")
        if (u=='symbol' | u=='unit' ) {
            u <- ''
        }
        u               <- trimws(u)
        table[[col]]$u  <- u
        if (u!='' )  table[[col]]$h <- c (table[[col]]$h, u)
    }
    table
}

correctCollapsedNullColums <- function (table) {
    nullCols    <- as.logical (sapply (table, function(X) is.null (X$v)))
    if (sum (nullCols)) {   # a null colum which is assumed to be a collapsed list of NULLs
       maxVRows <- max (sapply (table, function(X) length (X$v)))
       if (maxVRows) for (cl in 1:length(nullCols)) {
          if (nullCols[cl]) table[[cl]]$v <- vector(mode='list', length=maxVRows)
       }
    }
    table
}

convertPOSIXltTableValues <- function (table) {
    nRows <- length (table[[1]]$v)
    for (cl in 1:length(table)) {
        v <- table[[cl]]$v
        #if (length(class (v)) > 1) cat ("convertPOSIXltTableValues:", "class (v)=", class (v), '\n')
        if ('list' %in% class (v)) {
            for (rw in 1:nRows) {
                if ('POSIXlt' %in% class(v[[rw]])) {
                    table[[cl]]$v[[rw]] <- as.POSIXct(v[[rw]])
                }
            }
        }
    }
    table
}

processTableColumn <- function (col, hRows) {           # col, a table column
    allign <- col$allign
    if (allign=='cente' | allign=='center') allign <- 'centre'
    allign <- procesEnumertedParameter (allign, c('default', 'left', 'right', 'centre', 'dot'))
    txt    <- character(length(col$v)) # value rows
    # process NULL values
    col         <- processNullVals   (col)
    if (is.null(col))  return (NULL)
    nn          <- ! col$isNull                         # not Null
    txt[!nn]    <- col$null

    if (sum(nn) == 0) {
        vClass <- class (col$null)                      # col is now a character column
    } else { # process other non-std values
        vClass      <- class (col$nnV)
        col         <- processNonStdVals (col)
        txt[nn][col$nnIsNa]     <- col$na
        txt[nn][col$nnIsZero]   <- col$zero             # non '0' replacements only
        txt[nn][col$nnIsPInf]   <- col$pInf
        txt[nn][col$nnIsMInf]   <- col$mInf
        txt[nn][col$nnIsNaN]    <- "NaN"
        txt[nn][col$nnIsTrue]   <- col$true
        txt[nn][col$nnIsFalse]  <- col$false
    }
    # std Values
    someStd                     <- sum (col$nnIsStd)
    if (someStd) {
        stdV                    <- col$nnV[col$nnIsStd]
        psv                     <- processStdValues (stdV)
        stdTxt                  <- psv$stdTxt
        vClass                  <- psv$vClass           # may be changed to integer
        txt[nn][col$nnIsStd]    <- stdTxt
        # correct for is.na
        stdIsNa                 <- is.na (stdTxt)       # some converted values may be NA
        if (sum(stdIsNa)) {
            stdTxt[stdIsNa]         <- col$na
            col$nnIsNa[stdIsNa]     <- TRUE
            col$nnIsStd[stdIsNa]    <- FALSE
            someStd                 <- sum (col$nnIsStd)
        }
    }

    # format the column
    if (someStd) {
        if (allign == 'default') {
            allign <- 'left'
            if ('numeric' %in% vClass)        allign <- 'dot'
            else if (vClass[1] == 'logical')  allign <- 'centre'
            else if (vClass[1] == 'integer')  allign <- 'right'
        }
    } else {                            # all non std values
        vClass <- 'character'
        if (allign == 'default') allign <- ifelse (vClass=='logical', 'centre', 'left')
    }

    if (allign=='dot') {
        isStd                   <- logical(length(txt))
        isStd[nn][col$nnIsStd]  <-TRUE
        fCol <- formatDotColumn (h=col$h, txt=txt, nonNumL=!isStd, hRows=hRows)
    } else {    # nonDot: 'POSIXt', 'Date', 'character', 'integer', 'logical'
        fCol<- formatNonDotColumn (h=col$h, txt=txt, allign=allign, hRows=hRows)
    }
    fCol
}

formatDotColumn <- function (h, txt, nonNumL, hRows) {
    # a dot column may cantain non-numeric character strings as well, e.g. NULL, an or zero  replacements.formatDotColumnNonNums
    vWidth  <- getDotColumnWidthParams (txt, nonNumL)
    colW    <- max (nchar (h), vWidth$valueW, vWidth$nonNumW)   # net column width, ex ' ' as column separator

    # create column and add header
    tRows       <- length(txt)
    col         <- character (hRows + tRows)
    n           <- length (h)
    col[1:n]    <- h
    col[1:hRows]<- centreText (col[1:hRows], colW)
    # list (valueW=valueW, preW=preW, postW=postW, nonNumW=nonNumW, pre=pre, post=post)
    fColV           <- character (tRows)
    fColV[!nonNumL] <- formatDotColumnNums    (nVals=txt[!nonNumL], vWidth=vWidth, colW)
    fColV[ nonNumL] <- formatDotColumnNonNums (cVals=txt[ nonNumL], vWidth=vWidth, colW)
    col[hRows + 1:tRows] <- fColV
    col
}

formatNonDotColumn <- function (h, txt, allign='left', hRows) {
    tRows   <- length(txt)
    col     <- character (hRows + tRows)
    n       <- length (h)
    col[1:n]                        <- h
    col[(hRows+1):(hRows+tRows)]    <- txt
    colW    <- max (nchar(col))
    formatNonDotColValues (col, colW=colW, allign=allign)
}

formatNonDotColValues <- function (values, colW, allign) {
    tRows <- length(values)
    fColV   <- character (length=tRows)
    if (allign=='left') {
        lfs     <- paste ("%-", colW, 's ', sep='')
        fColV   <- sprintf (lfs, values)
    } else if (allign=='right') {                               # right justify, trailinfg space for column seapration
        rfs     <- paste ("%", colW, 's ', sep='')
        fColV   <- sprintf (rfs, values)
    } else {                                                    # centre, trailinfg space for column seapration
        fColV <- centreText (txt=values, colW=colW)
    }
    fColV
}

centreText <- function (txt, colW) {
    n       <- length (txt)
    cTxt    <- character (n)
    for (i in 1:n) {
        s   <- txt[i]
        lW      <- (colW - nchar(s)) %/% 2
        cfs     <- paste ("%", lW, "s%-", colW-lW, 's ', sep='')
        cTxt[i] <- sprintf (cfs, '', s)
    }
    cTxt
}

getDotColumnWidthParams <- function (values, nonNumL) {
    nonNumW <- valueW <- preW <- 0
    postW   <- 1
    if (sum(nonNumL)) nonNumW <- max (nchar (values[nonNumL]))
    if (sum(!nonNumL)) {
        pp      <- strsplit (values[!nonNumL], '[.]')
        pre     <- sapply (pp, function (X) X[1])
        post    <- sapply (pp, function(X) ifelse (length(X)>1, paste('.', X[2], sep=''), ''))
        preW    <- max (nchar (pre))
        postW   <- max (nchar (post))                           # incl dot, if present
        valueW  <- preW + postW
    }
    list (valueW=valueW, preW=preW, postW=postW, nonNumW=nonNumW, pre=pre, post=post)
}

formatDotColumnNums <- function (nVals, vWidth, colW) {
    # determine left marging
    lmW     <- colW - vWidth$valueW
    if (lmW) {
        lmFs    <- paste ("%", lmW, 's', sep='')
        lm      <- sprintf(lmFs, '')
    } else lm <- ''
    fs      <- paste ("%s%", vWidth$preW, 's%-', vWidth$postW , 's ', sep='')   # with trailinfg space for column seapration
    sprintf (fs, lm, vWidth$pre, vWidth$post)
}

formatDotColumnNonNums <- function (cVals, vWidth, colW) {
    valueW  <- vWidth$valueW
    nonNumW <- vWidth$nonNumW
    lmW     <- colW - valueW                                                    # left margin for numbers                                                   # left margin num values
    preDotW <- vWidth$preW + lmW                                                # pre dot space
    postDotW<- max (0, vWidth$postW-1)                                          # excl dot if present, i.e. post dot space
    if (nonNumW <= max (preDotW, postDotW)) {
        if (postDotW >= preDotW) {                                              # add under decimal parts
            fs          <- paste ("%", preDotW, 's %-', postDotW , 's ', sep='')
            nonNums     <- sprintf (fs, '', cVals)
        } else {                                                                # rigth allign under lm + integer parts
            if (postDotW == 0) {
                fs          <- paste ("%", preDotW, 's ', sep='')
                nonNums     <- sprintf (fs, cVals)
            } else {
                fs          <- paste ("%", preDotW, 's %', postDotW , 's ', sep='')
                nonNums     <- sprintf (fs, cVals, '')
            }

        }
    } else {                                                           # centre largest under numeric values
        margingsW   <- ifelse (nonNumW <= valueW, valueW - nonNumW, colW - nonNumW)
        nnLmW       <- floor (margingsW / 2)
        nnVfs       <- paste ("%", nnLmW,"s%-", colW-nnLmW , 's ', sep='')
        nonNums     <- sprintf (nnVfs, '', cVals)
    }
    nonNums
}

# ------------------------------------------------------------------------------
#                     Common Lines / Table support functions
# ------------------------------------------------------------------------------
allNullsToBeSkipped <- function (q) {
    if (q$type == 'lines') cls  <- q$lines
    else                   cls  <- q$table
    l           <- length (cls)
    toBeSkipped <- logical (length = l)
    for (i in 1:l) if (cls[[i]]$skipAllNull) {
        v   <- cls[[i]]$v
        if ('list' %in% class(v)) {
            allNulls    <- min (as.logical (sapply (v, is.null)))
            if (allNulls) toBeSkipped [i] <- TRUE
        }
    }
    toBeSkipped
}

processNullVals <- function (lc) {                      # line or column
    # NOTE v may be a list with NULLs and all other elements, if any, of the same class
    v  <- lc$v
    # process NULL, if any, and unlist
    isNull      <- as.logical (sapply (v, is.null))
    lc$isNull <- isNull
    if (sum(isNull)) {
        if (sum (isNull) == length(v) && lc$skipAllNull)   return (NULL)        # superfluous
        if (!is.character(lc$null))                     stop ("nulll must be of type character")
    }
    if (sum (isNull) < length(v)) {
        aNonNull            <- v [[which.min (isNull)]]
        vAttributes         <- attributes (aNonNull)        # save attributes
        nnV                 <- unlist (v[!isNull])
        attributes(nnV)     <- vAttributes                  # restore attributes stripped by unlist()
        lc$nnV              <- nnV
    }
    lc
}

processNonStdVals <- function (lc) {                    # line or column
    nnV     <- lc$nnV
    # na
    isNa    <- is.na (nnV)                              # R considers NaN as NA
    if (sum(isNa)) {
        if (!is.character(lc$na))                       stop ("na must be of type character")
    }

    isZero <- isPInf <- isMInf <- isNaN <- isTrue <- isFalse <- logical (length(nnV))
    # if numeric: zero, Inf, NaN
    if (is.numeric(nnV)) {
        zero        <- lc$zero
        if (zero != 0) {
            if (!is.character(zero))                    stop ("zero must be 0 or of type character")
            isZero  <- nnV == 0                         # zero replacements only
        }
        if (lc$pInf !=  Inf && !is.character(lc$pInf))  stop ("pInf must be Inf or of type character")
        if (lc$mInf != -Inf && !is.character(lc$mInf))  stop ("mInf must be -Inf or of type character")
        isPInf[!isNa]   <- nnV[!isNa] ==  Inf
        isMInf[!isNa]   <- nnV[!isNa] == -Inf
        isNaN           <- is.nan (nnV)
    }

    nnIsStd    <- ! (isNa | isZero | isPInf | isMInf | isNaN)
    if (sum(nnIsStd) & is.logical (nnV)) {
        # in case of a line nvv contains std values and lc$true and lc$false are both NULL
        if (!is.null(lc$true)) {
            if (!is.character(lc$true ))                stop ("true must be of type character")
            isTrue  <- !isNa & nnV
            nnIsStd <- logical (length(nnV))            # nnIsStd <- FALSE
        }
        if (!is.null(lc$false)) {
            if (!is.character(lc$false))                stop ("false must be of type character")
            isFalse <- !isNa & !nnV
            nnIsStd <- logical (length(nnV))            # nnIsStd <- FALSE
        }
    }
    lc$nnIsNa   <- isNa   & !isNaN                      # R considers NaN as NA
    lc$nnIsZero <- isZero
    lc$nnIsPInf <- isPInf
    lc$nnIsMInf <- isMInf
    lc$nnIsNaN  <- isNaN
    lc$nnIsTrue <- isTrue
    lc$nnIsFalse<- isFalse
    lc$nnIsStd  <- nnIsStd
    lc
}

processStdValues <- function (stdV) {                   # line or column
    vClass      <- class (stdV)
    # change numeric to integer if possible (i.e. without rounding)
    if (vClass[1] == 'numeric') {
        stdValIsInt <- !is.na (sapply(stdV, asInteger))
        if (sum(stdValIsInt) == length(stdValIsInt))  vClass <- 'integer'
    }
    # convert to a character vector
    if      ('POSIXt'   %in% vClass    ) stdTxt <- posixToStr (stdV)
    # length vClass sohould be 1 for the following
    else if (vClass      == 'Date'     ) stdTxt <- as.character (stdV)
    else if (vClass      == 'numeric'  ) stdTxt <- as.character (stdV)
    else if (vClass      == 'integer'  ) stdTxt <- as.character (stdV)
    else if (vClass      == 'logical'  ) stdTxt <- as.character (stdV)
    else if (vClass      == 'character') stdTxt <- stdV
    else stop ("values of class '", vClass, "' are yet supported")
    list (stdTxt=stdTxt, vClass=vClass)
}

posixToStr <- function (t) {        # with subsecond accuracy and without trailing zero'
    tc  <- format(t, "%Y-%m-%d %H:%M:%OS9", usetz=FALSE)
    tc <- gsub ('[.]?0*$', '', tc)              # remove trailing '0's and a '.'
    # append tz, if appropriate
    tzone <- attr(t, 'tzone')[1]
    if (is.null(tzone)) tzone <- ''
    paste (tc, tzone, sep=' ')
}
