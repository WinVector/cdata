
#' @importFrom dplyr %>%
#' @importFrom stats complete.cases
#' @importFrom tibble frame_data
#' @importFrom tidyr gather spread
#' @importFrom wrapr let
NULL



#' Check that a set of columns form unique keys
#'
#' @param data data.frame to work with.
#' @param keyColNames character array of column names to check.
#' @param allowNAKeys logical if FALSE throw if there are NAs in the key columns.
#' @return logical TRUE if the rows of data are unique addressable by the columns named in keyColNames.
#'
#'
#' @examples
#'
#' d <- data.frame(key= c('a','a', 'b'))
#' checkColsFormUniqueKeys(d, 'key')
#'
#' @export
#'
checkColsFormUniqueKeys <- function(data, keyColNames,
                                    allowNAKeys = TRUE) {
  data <- dplyr::ungroup(data)
  # check for NA keys
  if((!allowNAKeys) && (length(keyColNames)>0)) {
    allGood <- data %>%
      dplyr::select(dplyr::one_of(keyColNames)) %>%
      complete.cases() %>%
      all()
    if(!allGood) {
      stop("saw NA in keys")
    }
  }
  cn <- colnames(data)
  if(length(keyColNames)!=length(unique(keyColNames, allowNAKeys=TRUE))) {
    stop("cdata:checkColsFormUniqueKeys keyColNames must not have duplicates")
  }
  if(length(setdiff(keyColNames,cn))>0) {
    stop("cdata:checkColsFormUniqueKeys all keyColNames must be columns of data")
  }
  # count the number of rows (threat 0-column frames as 0-row frames)
  ndata <- 0
  if(ncol(data)>0) {
    ndata <- nrow(data)
  }
  if(ndata<=1) {
    return(TRUE)
  }
  # count the number of rows identifiable by keys
  nunique <- min(1, ndata)
  if(length(keyColNames)>0) {
    nunique <-
      data %>%
        dplyr::select(dplyr::one_of(keyColNames)) %>%
        dplyr::distinct() %>%
        nrow()
  }
  # compare
  return(nunique==ndata)
}

#' Move values from columns to rows (wrapper for \code{tidyr::gather}, or anti-pivot).
#'
#' For a tutorial please try \code{vignette('RowsAndColumns', package='cdata')}.
#'
#' @seealso \code{\link{moveValuesToColumns}}
#'
#' @param data data.frame to work with.
#' @param nameForNewKeyColumn character name of column to write new keys in.
#' @param nameForNewValueColumn character name of column to write new values in.
#' @param columnsToTakeFrom character array names of columns to take values from.
#' @param na.rm passed to \code{tidyr::gather}
#' @param convert passed to \code{tidyr::gather}
#' @param factor_key passed to \code{tidyr::gather}
#' @return new data.frame with values moved to rows.
#'
#' @examples
#'
#' d <- data.frame(AUC= 0.6, R2= 0.2)
#' moveValuesToRows(d,
#'                  nameForNewKeyColumn= 'meas',
#'                  nameForNewValueColumn= 'val',
#'                  columnsToTakeFrom= c('AUC', 'R2'))
#'
#' @export
#'
#'
moveValuesToRows <- function(data,
                             nameForNewKeyColumn,
                             nameForNewValueColumn,
                             columnsToTakeFrom,
                             na.rm = FALSE,
                             convert = FALSE,
                             factor_key = FALSE) {
  data <- dplyr::ungroup(data)
  cn <- colnames(data)
  if(length(nameForNewKeyColumn)!=1) {
    stop("cdata:moveValuesToRows nameForNewKeyColumn must be length 1")
  }
  if(length(nameForNewValueColumn)!=1) {
    stop("cdata:moveValuesToRows nameForNewValueColumn must be length 1")
  }
  if(!is.character(nameForNewKeyColumn)) {
    stop("cdata:moveValuesToRows nameForNewKeyColumn must be character")
  }
  if(!is.character(nameForNewValueColumn)) {
    stop("cdata:moveValuesToRows nameForNewValueColumn must be character")
  }
  if(length(columnsToTakeFrom)>0) {
    if(!is.character(columnsToTakeFrom)) {
      stop("cdata:moveValuesToRows columnsToTakeFrom must be character")
    }
    if(any(is.na(columnsToTakeFrom))) {
      stop("cdata:moveValuesToRows columnsToTakeFrom must not contain NA")
    }
    if(any(nchar(columnsToTakeFrom)<=0)) {
      stop("cdata:moveValuesToRows columnsToTakeFrom must not contain ''")
    }
    if(length(unique(columnsToTakeFrom))!=length(columnsToTakeFrom)) {
      stop("cdata:moveValuesToRows columnsToTakeFrom must be unique values")
    }
  }
  if(nameForNewKeyColumn %in% cn) {
    stop("cdata:moveValuesToRows nameForNewKeyColumn must not be a column name")
  }
  if(nameForNewValueColumn %in% cn) {
    stop("cdata:moveValuesToRows nameForNewValueColumn must not be a column name")
  }
  if(nameForNewKeyColumn==nameForNewValueColumn) {
    stop("cdata:moveValuesToRows nameForNewKeyColumn must not equal nameForNewValueColumn")
  }
  if(length(setdiff(columnsToTakeFrom,cn))>0) {
    stop("cdata:moveValuesToRows columnsToTakeFrom must all be column names")
  }
  dcols <- setdiff(cn, columnsToTakeFrom)
  if(!checkColsFormUniqueKeys(dplyr::select(data,
                                            dplyr::one_of(dcols)),
                              dcols,
                              allowNAKeys = TRUE)) {
    stop("cdata:moveValuesToRows rows were not uniquely keyed")
  }
  NAMEFORNEWKEYCOLUMM <- NULL # signal not an unbound variable
  NAMEFORNEWVALUECOLUMN <- NULL # signal not an unbound variable
  wrapr::let(c(NAMEFORNEWKEYCOLUMM= nameForNewKeyColumn,
               NAMEFORNEWVALUECOLUMN= nameForNewValueColumn),
             tidyr::gather(data,
                           key= NAMEFORNEWKEYCOLUMM,
                           value= NAMEFORNEWVALUECOLUMN,
                           dplyr::one_of(columnsToTakeFrom),
                           na.rm = na.rm,
                           convert = convert,
                           factor_key = factor_key)
  )
}

#' Move values from rows to columns (wrapper for \code{tidyr::spread} or pivot).
#'
#' For a tutorial please try \code{vignette('RowsAndColumns', package='cdata')}.
#'
#' @seealso \code{\link{moveValuesToRows}}
#'
#' @param data data.frame to work with.
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param rowKeyColumns character array names columns that should be table keys.
#' @param fill passed to \code{tidyr::spread}
#' @param convert passed to \code{tidyr::spread}
#' @param drop passed to \code{tidyr::spread}
#' @param sep passed to \code{tidyr::spread}
#' @return new data.frame with values moved to columns.
#'
#' @examples
#'
#' d <- data.frame(meas= c('AUC', 'R2'), val= c(0.6, 0.2))
#' moveValuesToColumns(d,
#'                     columnToTakeKeysFrom= 'meas',
#'                     columnToTakeValuesFrom= 'val',
#'                     rowKeyColumns= c())
#'
#' @export
#'
moveValuesToColumns <- function(data,
                                columnToTakeKeysFrom,
                                columnToTakeValuesFrom,
                                rowKeyColumns,
                                fill = NA,
                                convert = FALSE,
                                drop = TRUE,
                                sep = NULL) {
  data <- dplyr::ungroup(data)
  cn <- colnames(data)
  if(length(columnToTakeKeysFrom)!=1) {
    stop("cdata:moveValuesToColumns columnToTakeKeysFrom must be length 1")
  }
  if(length(columnToTakeValuesFrom)!=1) {
    stop("cdata:moveValuesToColumns columnToTakeValuesFrom must be length 1")
  }
  if(!is.character(columnToTakeKeysFrom)) {
    stop("cdata:moveValuesToColumns columnToTakeKeysFrom must be character")
  }
  if(!is.character(columnToTakeValuesFrom)) {
    stop("cdata:moveValuesToColumns columnToTakeValuesFrom must be character")
  }
  if(length(rowKeyColumns)>0) {
    if(!is.character(rowKeyColumns)) {
      stop("cdata:moveValuesToColumns rowKeyColumns must be character")
    }
  }
  if(!(columnToTakeKeysFrom %in% cn)) {
    stop("cdata:moveValuesToColumns columnToTakeKeysFrom must be a column name")
  }
  if(!(columnToTakeValuesFrom %in% cn)) {
    stop("cdata:moveValuesToColumns columnToTakeValuesFrom must be a column name")
  }
  # if(columnToTakeKeysFrom==columnToTakeValuesFrom) {
  #   stop("cdata:moveValuesToColumns columnToTakeKeysFrom must not equal columnToTakeValuesFrom")
  # }
  if(length(setdiff(rowKeyColumns,cn))>0) {
    stop("cdata:moveValuesToColumns rowKeyColumns must all be column names")
  }
  if(columnToTakeKeysFrom %in% rowKeyColumns) {
    stop("cdata:moveValuesToColumns columnToTakeKeysFrom not be in rowKeyColumns")
  }
  if(columnToTakeValuesFrom %in% rowKeyColumns) {
    stop("cdata:moveValuesToColumns columnToTakeValuesFrom not be in rowKeyColumns")
  }
  # we insist that the rowKeyColumns plus
  # columnToTakeKeysFrom are unique keys
  if(!checkColsFormUniqueKeys(data,
                              c(rowKeyColumns,
                                columnToTakeKeysFrom),
                              allowNAKeys = TRUE)) {
    stop(paste0("\n moveValeusToColumns: specified",
                "\n rowKeyColumns plus columnToTakeKeysFrom",
                "\n isn't unique across rows"))
  }
  # we are also going to insist that rowKeyColumns are the unique keys for
  # the distinct rows data frame without columnToTakeKeysFrom and columnToTakeValuesFrom
  dcols <- setdiff(colnames(data),
                   c(columnToTakeKeysFrom, columnToTakeValuesFrom))
  dsub <- data %>%
    dplyr::select(dplyr::one_of(dcols)) %>%
    dplyr::distinct()
  if(!checkColsFormUniqueKeys(dsub,
                              rowKeyColumns,
                              allowNAKeys = TRUE)) {
    stop(paste0("\n some columns not in",
                "\n c(rowKeyColumns, columnToTakeKeysFrom, columnToTakeValuesFrom)",
                "\n are splitting up row groups"))
  }
  COLUMNTOTAKEKEYSFROM <- NULL # signal not an unbound variable
  COLUMNTOTAKEVALUESFROM <- NULL # signal not an unbound variable
  wrapr::let(c(COLUMNTOTAKEKEYSFROM= columnToTakeKeysFrom,
               COLUMNTOTAKEVALUESFROM= columnToTakeValuesFrom),
             tidyr::spread(data,
                           key= COLUMNTOTAKEKEYSFROM,
                           value= COLUMNTOTAKEVALUESFROM,
                           fill = fill,
                           convert = convert,
                           drop = drop,
                           sep = sep)
  )
}

