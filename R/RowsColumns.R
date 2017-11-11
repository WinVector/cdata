
#' @importFrom wrapr %.>%
#' @importFrom stats complete.cases
#' @importFrom wrapr let
NULL



#' Check that a set of columns form unique keys.
#'
#' For local data.frame only.
#'
#' @param data data.frame to work with.
#' @param keyColNames character array of column names to check.
#' @return logical TRUE if the rows of data are unique addressable by the columns named in keyColNames.
#'
#'
#' @examples
#'
#' d <- data.frame(key = c('a','a', 'b'))
#' checkColsFormUniqueKeys(d, 'key')
#' d <- data.frame(key = c('a','a', 'b'), k2 = c(1 ,2, 2))
#' checkColsFormUniqueKeys(d, c('key', 'k2'))
#' @export
#'
checkColsFormUniqueKeys <- function(data, keyColNames) {
  cn <- colnames(data)
  if(length(keyColNames)!=length(unique(keyColNames, allowNAKeys=TRUE))) {
    stop("cdata::checkColsFormUniqueKeys keyColNames must not have duplicates")
  }
  if(length(setdiff(keyColNames,cn))>0) {
    stop("cdata::checkColsFormUniqueKeys all keyColNames must be columns of data")
  }
  # count the number of rows (treat 0-column frames as 0-row frames)
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  if(length(keyColNames) <= 0) {
    return(FALSE)
  }
  # count the number of rows identifiable by keys
  data <- data[, keyColNames, drop=FALSE]
  return(anyDuplicated(data)<=0)
}

#' Move values from columns to rows (anti-pivot).
#'
#' For a tutorial please try \code{vignette('RowsAndColumns', package='cdata')}.
#'
#' @seealso \code{\link{pivotValuesToColumns}}
#'
#' @param data data.frame to work with (must be local, for remote please try \code{\link[replyr]{moveValuesToRowsQ}}).
#' @param nameForNewKeyColumn character name of column to write new keys in.
#' @param nameForNewValueColumn character name of column to write new values in.
#' @param columnsToTakeFrom character array names of columns to take values from.
#' @param ... force later argumets to bind by name.
#' @param nameForNewClassColumn optional name to land original cell classes to.
#' @return new data.frame with values moved to rows.
#'
#' @examples
#'
#' d <- data.frame(AUC= 0.6, R2= 0.2)
#' unpivotValuesToRows(d,
#'                  nameForNewKeyColumn= 'meas',
#'                  nameForNewValueColumn= 'val',
#'                  columnsToTakeFrom= c('AUC', 'R2'))
#'
#' @export
#'
#'
unpivotValuesToRows <- function(data,
                             nameForNewKeyColumn,
                             nameForNewValueColumn,
                             columnsToTakeFrom,
                             ...,
                             nameForNewClassColumn = NULL) {
  if(!is.data.frame(data)) {
    stop("cdata::unpivotValuesToRows data must be a local data.frame")
  }
  cn <- colnames(data)
  if(length(list(...))>0) {
    stop("cdata::unpivotValuesToRows unexpected arguments")
  }
  if(length(nameForNewKeyColumn)!=1) {
    stop("cdata::unpivotValuesToRows nameForNewKeyColumn must be length 1")
  }
  if(length(nameForNewValueColumn)!=1) {
    stop("cdata::unpivotValuesToRows nameForNewValueColumn must be length 1")
  }
  if(!is.character(nameForNewKeyColumn)) {
    stop("cdata::unpivotValuesToRows nameForNewKeyColumn must be character")
  }
  if(!is.character(nameForNewValueColumn)) {
    stop("cdata::unpivotValuesToRows nameForNewValueColumn must be character")
  }
  if(length(columnsToTakeFrom)>0) {
    if(!is.character(columnsToTakeFrom)) {
      stop("cdata::unpivotValuesToRows columnsToTakeFrom must be character")
    }
    if(any(is.na(columnsToTakeFrom))) {
      stop("cdata::unpivotValuesToRows columnsToTakeFrom must not contain NA")
    }
    if(any(nchar(columnsToTakeFrom)<=0)) {
      stop("cdata::unpivotValuesToRows columnsToTakeFrom must not contain ''")
    }
    if(length(unique(columnsToTakeFrom))!=length(columnsToTakeFrom)) {
      stop("cdata::unpivotValuesToRows columnsToTakeFrom must be unique values")
    }
  }
  if(nameForNewKeyColumn %in% cn) {
    stop("cdata::unpivotValuesToRows nameForNewKeyColumn must not be a column name")
  }
  if(nameForNewValueColumn %in% cn) {
    stop("cdata::unpivotValuesToRows nameForNewValueColumn must not be a column name")
  }
  if(nameForNewKeyColumn==nameForNewValueColumn) {
    stop("cdata::unpivotValuesToRows nameForNewKeyColumn must not equal nameForNewValueColumn")
  }
  if(length(setdiff(columnsToTakeFrom,cn))>0) {
    stop("cdata::unpivotValuesToRows columnsToTakeFrom must all be column names")
  }
  if(length(nameForNewClassColumn)!=0) {
    if((length(nameForNewClassColumn)!=1) || (!is.character(nameForNewClassColumn))) {
      stop("cdata::unpivotValuesToRows nameForNewClassColumn must be length 1 character")
    }
  }
  dcols <- setdiff(cn, columnsToTakeFrom)
  if(!checkColsFormUniqueKeys(data, dcols)) {
    stop("cdata::unpivotValuesToRows rows were not uniquely keyed")
  }
  cT <- buildUnPivotControlTable(nameForNewKeyColumn = nameForNewKeyColumn,
                                 nameForNewValueColumn = nameForNewValueColumn,
                                 columnsToTakeFrom = columnsToTakeFrom)
  res <- moveValuesToRowsD(data,
                           controlTable = cT)
  if(!is.null(nameForNewClassColumn)) {
    classMap <- vapply(data, class, character(1))
    names(classMap) <- colnames(data)
    res[[nameForNewClassColumn]] <- classMap[res[[nameForNewKeyColumn]]]
  }
  res
}

#' Move values from rows to columns (pivot).
#'
#' For a tutorial please try \code{vignette('RowsAndColumns', package='cdata')}.
#'
#' @seealso \code{\link{unpivotValuesToRows}}
#'
#' @param data data.frame to work with (must be local, for remote please try \code{\link[replyr]{moveValuesToColumnsQ}}).
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param rowKeyColumns character array names columns that should be table keys.
#' @param ... force later arguments to bind by name.
#' @param sep character if not null build more detailed column names.
#' @return new data.frame with values moved to columns.
#'
#' @examples
#'
#' d <- data.frame(meas= c('AUC', 'R2'), val= c(0.6, 0.2))
#' pivotValuesToColumns(d,
#'                     columnToTakeKeysFrom= 'meas',
#'                     columnToTakeValuesFrom= 'val',
#'                     rowKeyColumns= c())
#'
#' @export
#'
pivotValuesToColumns <- function(data,
                                columnToTakeKeysFrom,
                                columnToTakeValuesFrom,
                                rowKeyColumns,
                                ...,
                                sep = NULL) {
  if(!is.data.frame(data)) {
    stop("cdata::pivotValuesToColumns data must be a local data.frame")
  }
  cn <- colnames(data)
  if(length(list(...))>0) {
    stop("cdata::pivotValuesToColumns unexpected arguments")
  }
  if(length(columnToTakeKeysFrom)!=1) {
    stop("cdata::pivotValuesToColumns columnToTakeKeysFrom must be length 1")
  }
  if(length(columnToTakeValuesFrom)!=1) {
    stop("cdata::pivotValuesToColumns columnToTakeValuesFrom must be length 1")
  }
  if(!is.character(columnToTakeKeysFrom)) {
    stop("cdata::pivotValuesToColumns columnToTakeKeysFrom must be character")
  }
  if(!is.character(columnToTakeValuesFrom)) {
    stop("cdata::pivotValuesToColumns columnToTakeValuesFrom must be character")
  }
  if(length(rowKeyColumns)>0) {
    if(!is.character(rowKeyColumns)) {
      stop("cdata::pivotValuesToColumns rowKeyColumns must be character")
    }
  }
  if(!(columnToTakeKeysFrom %in% cn)) {
    stop("cdata::pivotValuesToColumns columnToTakeKeysFrom must be a column name")
  }
  if(!(columnToTakeValuesFrom %in% cn)) {
    stop("cdata::pivotValuesToColumns columnToTakeValuesFrom must be a column name")
  }
  # if(columnToTakeKeysFrom==columnToTakeValuesFrom) {
  #   stop("cdata::pivotValuesToColumns columnToTakeKeysFrom must not equal columnToTakeValuesFrom")
  # }
  if(length(setdiff(rowKeyColumns,cn))>0) {
    stop("cdata::pivotValuesToColumns rowKeyColumns must all be column names")
  }
  if(columnToTakeKeysFrom %in% rowKeyColumns) {
    stop("cdata::pivotValuesToColumns columnToTakeKeysFrom not be in rowKeyColumns")
  }
  if(columnToTakeValuesFrom %in% rowKeyColumns) {
    stop("cdata::pivotValuesToColumns columnToTakeValuesFrom not be in rowKeyColumns")
  }
  # we insist that the rowKeyColumns plus
  # columnToTakeKeysFrom are unique keys
  if(!checkColsFormUniqueKeys(data,
                              c(rowKeyColumns,
                                columnToTakeKeysFrom))) {
    stop(paste0("\n moveValeusToColumns: specified",
                "\n rowKeyColumns plus columnToTakeKeysFrom",
                "\n isn't unique across rows"))
  }
  # we are also going to insist that rowKeyColumns are the unique keys for
  # the distinct rows data frame without columnToTakeKeysFrom and columnToTakeValuesFrom
  dcols <- setdiff(colnames(data),
                   c(columnToTakeKeysFrom, columnToTakeValuesFrom))
  if(length(dcols)>0) {
    dsub <- data[, dcols, drop=FALSE]
    dups <- duplicated(dsub)
    dsub <- dsub[!dups, , drop=FALSE]
    if(!checkColsFormUniqueKeys(dsub,
                                rowKeyColumns)) {
      stop(paste0("\n some columns not in",
                  "\n c(rowKeyColumns, columnToTakeKeysFrom, columnToTakeValuesFrom)",
                  "\n are splitting up row groups"))
    }
  }
  cT <- buildPivotControlTableD(data,
                                columnToTakeKeysFrom = columnToTakeKeysFrom,
                                columnToTakeValuesFrom = columnToTakeValuesFrom,
                                sep = sep)
  moveValuesToColumnsD(data,
                       keyColumns = keyColumns,
                       controlTable = cT)
}

