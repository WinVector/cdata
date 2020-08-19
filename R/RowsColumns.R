

# adapters for more direct pivot/un-pivot notation
# (hides details of control table)



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
                                      strict = FALSE,
                                      allow_rqdatatable = FALSE) {
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

#' Map data records from block records that have one row per measurement value to row records.
#'
#' Map data records from block records (where each record may be more than one row) to
#' row records (where each record is a single row).  Values specified in rowKeyColumns
#' determine which sets of rows build up records and are copied into the result.
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
#' @param allow_rqdatatable logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @return new data.frame with values moved to columns.
#'
#' @seealso \code{\link{unpivot_to_blocks}}, \code{\link{blocks_to_rowrecs}}
#'
#' @examples
#'
#'   d <- data.frame(model_id = c("m1", "m1"), meas = c('AUC', 'R2'), val= c(0.6, 0.2))
#'   pivot_to_rowrecs(d,
#'                    columnToTakeKeysFrom= 'meas',
#'                    columnToTakeValuesFrom= 'val',
#'                    rowKeyColumns= "model_id") %.>%
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
                             strict = FALSE,
                             allow_rqdatatable = FALSE) {
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
                    strict = strict,
                    allow_rqdatatable = allow_rqdatatable)
}

#' @rdname pivot_to_rowrecs
#' @export
layout_to_rowrecs <- pivot_to_rowrecs

