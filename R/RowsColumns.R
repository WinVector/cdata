

# adapters for more direct pivot/un-pivot notation
# (hides details of control table)

#' @importFrom wrapr %.>%
#' @importFrom stats complete.cases
#' @importFrom wrapr let
NULL


#' Check for duplicate rows.
#'
#' @param data data.frame
#' @return TRUE if there are no duplicate rows, else FALSE.
#'
#' @noRd
has_no_dup_rows <- function(data) {
  if(!is.data.frame(data)) {
    stop("cdata:::has_no_dup_rows(data) data must be a data.frame")
  }
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  keyColNames <- colnames(data)
  nkey <- length(keyColNames)
  if(nkey<=0) {
    return(FALSE)
  }
  idxs <- do.call(order, c(as.list(data), list(method = "radix")))
  data <- data[idxs, , drop = FALSE]
  rownames(data) <- NULL
  new_value <- c(TRUE, rep(FALSE, ndata-1))
  for(ki in keyColNames) {
    cA <- data[[ki]][-1]
    cB <- data[[ki]][-ndata]
    cAn <- is.na(cA)
    cBn <- is.na(cB)
    check <- ifelse(cAn | cBn, cAn != cBn, cA != cB)
    di <- c(TRUE, check)
    new_value <- new_value | di
  }
  return(isTRUE(all(new_value)))
}
# n <- 1000
# set.seed(2352)
#
# d1 <- data.frame(x1 = sample(letters, n, replace = TRUE),
#                  x2 = sample(letters, n, replace = TRUE),
#                  x3 = sample(letters, n, replace = TRUE),
#                  x4 = sample(letters, n, replace = TRUE))
# d1_decision <- anyDuplicated(d1)<=0
# d2 <- d1
# while((anyDuplicated(d2)<=0)==d1_decision) {
#   d2 <- data.frame(x1 = sample(letters, n, replace = TRUE),
#                    x2 = sample(letters, n, replace = TRUE),
#                    x3 = sample(letters, n, replace = TRUE),
#                    x4 = sample(letters, n, replace = TRUE))
# }
#
# my_check <- function(values) {
#   all(sapply(values[-1], function(x) identical(values[[1]], x)))
# }
#
# print(anyDuplicated(d1)<=0)
# microbenchmark::microbenchmark(
#   any_dup = { anyDuplicated(d1)<=0 },
#   has_no_dup = { cdata:::has_no_dup_rows(d1) },
#   check = my_check
# )
# # Unit: microseconds
# # expr       min         lq      mean   median        uq       max neval cld
# # any_dup 16528.260 21851.0965 31134.210 30628.08 39116.609 96021.977   100   b
# # has_no_dup   728.621   900.9505  1031.029   964.23  1068.575  4182.615   100  a
#
#
# print(anyDuplicated(d2)<=0)
# microbenchmark::microbenchmark(
#   any_dup = { anyDuplicated(d2)<=0 },
#   has_no_dup = { cdata:::has_no_dup_rows(d2) },
#   check = my_check
# )
# # Unit: microseconds
# # expr       min       lq       mean     median       uq       max neval cld
# # any_dup 16679.607 17874.35 20899.9830 19369.3610 21756.95 50894.236   100   b
# # has_no_dup   735.003   859.79   973.4994   927.8125  1053.39  2099.416   100  a


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
#' d <- data.frame(key = c('a','a', 'b'), k2 = c(1 ,2, 2))
#' checkColsFormUniqueKeys(d, 'key') # should be FALSE
#' checkColsFormUniqueKeys(d, c('key', 'k2')) # should be TRUE
#'
#' @export
#'
checkColsFormUniqueKeys <- function(data, keyColNames) {
  if(!is.data.frame(data)) {
    stop("cdata:::checkColsFormUniqueKeys data should be a data.frame")
  }
  if(length(keyColNames)!=length(unique(keyColNames, allowNAKeys=TRUE))) {
    stop("cdata::checkColsFormUniqueKeys keyColNames must not have duplicates/NAs")
  }
  cn <- colnames(data)
  if(length(setdiff(keyColNames, cn))>0) {
    stop("cdata::checkColsFormUniqueKeys all keyColNames must be columns of data")
  }
  # count the number of rows
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  if(length(keyColNames) <= 0) {
    return(FALSE)
  }
  data <- data[, keyColNames, drop = FALSE]
  rownames(data) <- NULL
  # identify duplicate rows, no duplicated is the obvious way, the
  # code below is an attempt at a speedup (at the cost of space).
  # return(anyDuplicated(data)<=0)
  return(has_no_dup_rows(data))
}




#' @export
#' @rdname unpivot_to_blocks
unpivot_to_blocks.default <- function(data,
                                      nameForNewKeyColumn,
                                      nameForNewValueColumn,
                                      columnsToTakeFrom,
                                      ...,
                                      nameForNewClassColumn = NULL,
                                      checkNames = TRUE,
                                      checkKeys = FALSE,
                                      strict = FALSE) {
  if(!is.data.frame(data)) {
    stop("cdata::unpivot_to_blocks.default data must be a local data.frame")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::unpivot_to_blocks")
  cn <- colnames(data)
  if(length(nameForNewKeyColumn)!=1) {
    stop("cdata::unpivot_to_blocks nameForNewKeyColumn must be length 1")
  }
  if(length(nameForNewValueColumn)!=1) {
    stop("cdata::unpivot_to_blocks nameForNewValueColumn must be length 1")
  }
  if(!is.character(nameForNewKeyColumn)) {
    stop("cdata::unpivot_to_blocks nameForNewKeyColumn must be character")
  }
  if(!is.character(nameForNewValueColumn)) {
    stop("cdata::unpivot_to_blocks nameForNewValueColumn must be character")
  }
  if(length(columnsToTakeFrom)>0) {
    if(!is.character(columnsToTakeFrom)) {
      stop("cdata::unpivot_to_blocks columnsToTakeFrom must be character")
    }
    if(any(is.na(columnsToTakeFrom))) {
      stop("cdata::unpivot_to_blocks columnsToTakeFrom must not contain NA")
    }
    if(any(nchar(columnsToTakeFrom)<=0)) {
      stop("cdata::unpivot_to_blocks columnsToTakeFrom must not contain ''")
    }
    if(length(unique(columnsToTakeFrom))!=length(columnsToTakeFrom)) {
      stop("cdata::unpivot_to_blocks columnsToTakeFrom must be unique values")
    }
  }
  if(nameForNewKeyColumn %in% cn) {
    stop("cdata::unpivot_to_blocks nameForNewKeyColumn must not be an existing column name")
  }
  if(nameForNewValueColumn %in% cn) {
    stop("cdata::unpivot_to_blocks nameForNewValueColumn must not be an existing column name")
  }
  if(nameForNewKeyColumn==nameForNewValueColumn) {
    stop("cdata::unpivot_to_blocks nameForNewKeyColumn must not equal nameForNewValueColumn")
  }
  if(length(setdiff(columnsToTakeFrom,cn))>0) {
    stop("cdata::unpivot_to_blocks columnsToTakeFrom must all be column names")
  }
  if(length(nameForNewClassColumn)!=0) {
    if((length(nameForNewClassColumn)!=1) || (!is.character(nameForNewClassColumn))) {
      stop("cdata::unpivot_to_blocks nameForNewClassColumn must be length 1 character")
    }
  }
  dcols <- setdiff(cn, columnsToTakeFrom)
  cT <- build_unpivot_control(nameForNewKeyColumn = nameForNewKeyColumn,
                              nameForNewValueColumn = nameForNewValueColumn,
                              columnsToTakeFrom = columnsToTakeFrom)
  colsToCopy <- setdiff(colnames(data), columnsToTakeFrom)
  res <- rowrecs_to_blocks(data,
                           controlTable = cT,
                           columnsToCopy = colsToCopy,
                           checkNames = checkNames,
                           checkKeys = checkKeys,
                           strict = strict)
  if(!is.null(nameForNewClassColumn)) {
    classMap <- vapply(data, class, character(1))
    names(classMap) <- colnames(data)
    res[[nameForNewClassColumn]] <- classMap[res[[nameForNewKeyColumn]]]
  }
  res
}

#' Move values from rows to columns (pivot).
#'
#' This is a convenience notation for \code{blocks_to_rowrecs}.
#' For a tutorial please try \code{vignette('RowsAndColumns', package='cdata')}.
#'
#'
#' @param data data.frame to work with (must be local, for remote please try \code{moveValuesToColumns*}).
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param rowKeyColumns character array names columns that should be table keys.
#' @param ... force later arguments to bind by name.
#' @param sep character if not null build more detailed column names.
#' @param checkNames logical, if TRUE check names.
#' @param checkKeys logical, if TRUE check keyColumns uniquely identify blocks (required).
#' @param strict logical, if TRUE check control table name forms
#' @return new data.frame with values moved to columns.
#'
#' @seealso \code{\link{unpivot_to_blocks}}, \code{\link{blocks_to_rowrecs}}
#'
#' @examples
#'
#'   d <- data.frame(meas= c('AUC', 'R2'), val= c(0.6, 0.2))
#'   pivot_to_rowrecs(d,
#'                    columnToTakeKeysFrom= 'meas',
#'                    columnToTakeValuesFrom= 'val',
#'                    rowKeyColumns= c()) %.>%
#'      print(.)
#'
#' @export
#'
pivot_to_rowrecs <- function(data,
                             columnToTakeKeysFrom,
                             columnToTakeValuesFrom,
                             rowKeyColumns,
                             ...,
                             sep = NULL,
                             checkNames = TRUE,
                             checkKeys = TRUE,
                             strict = FALSE) {
  if(!is.data.frame(data)) {
    stop("cdata::pivot_to_rowrecs data must be a local data.frame")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::pivot_to_rowrecs")
  cn <- colnames(data)
  if(length(columnToTakeKeysFrom)!=1) {
    stop("cdata::pivot_to_rowrecs columnToTakeKeysFrom must be length 1")
  }
  if(length(columnToTakeValuesFrom)!=1) {
    stop("cdata::pivot_to_rowrecs columnToTakeValuesFrom must be length 1")
  }
  if(!is.character(columnToTakeKeysFrom)) {
    stop("cdata::pivot_to_rowrecs columnToTakeKeysFrom must be character")
  }
  if(!is.character(columnToTakeValuesFrom)) {
    stop("cdata::pivot_to_rowrecs columnToTakeValuesFrom must be character")
  }
  if(length(rowKeyColumns)>0) {
    if(!is.character(rowKeyColumns)) {
      stop("cdata::pivot_to_rowrecs rowKeyColumns must be character")
    }
  }
  if(!(columnToTakeKeysFrom %in% cn)) {
    stop("cdata::pivot_to_rowrecs columnToTakeKeysFrom must be an existing column name")
  }
  if(!(columnToTakeValuesFrom %in% cn)) {
    stop("cdata::pivot_to_rowrecs columnToTakeValuesFrom must be an existing column name")
  }
  # if(columnToTakeKeysFrom==columnToTakeValuesFrom) {
  #   stop("cdata::pivot_to_rowrecs columnToTakeKeysFrom must not equal columnToTakeValuesFrom")
  # }
  if(length(setdiff(rowKeyColumns,cn))>0) {
    stop("cdata::pivot_to_rowrecs rowKeyColumns must all be column names")
  }
  if(columnToTakeKeysFrom %in% rowKeyColumns) {
    stop("cdata::pivot_to_rowrecs columnToTakeKeysFrom not be in rowKeyColumns")
  }
  if(columnToTakeValuesFrom %in% rowKeyColumns) {
    stop("cdata::pivot_to_rowrecs columnToTakeValuesFrom not be in rowKeyColumns")
  }
  cT <- build_pivot_control(data,
                            columnToTakeKeysFrom = columnToTakeKeysFrom,
                            columnToTakeValuesFrom = columnToTakeValuesFrom,
                            sep = sep)
  colsToCopy <- setdiff(colnames(data),
                        c(columnToTakeKeysFrom, columnToTakeValuesFrom, rowKeyColumns))
  blocks_to_rowrecs(data,
                    keyColumns = rowKeyColumns,
                    controlTable = cT,
                    columnsToCopy = colsToCopy,
                    checkNames = checkNames,
                    checkKeys = checkKeys,
                    strict = strict)
}


