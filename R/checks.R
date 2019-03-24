

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




# confirm control table structure
checkControlTable <- function(controlTable, controlTableKeys, strict) {
  if(!is.data.frame(controlTable)) {
    return("control table must be a data.frame")
  }
  if(nrow(controlTable)<1) {
    return("control table must have at least 1 row")
  }
  if(ncol(controlTable)<1) {
    return("control table must have at least 1 column")
  }
  if(length(colnames(controlTable)) != length(unique(colnames(controlTable)))) {
    return("control table column names must be unique")
  }
  if(any(is.na(colnames(controlTable)))) {
    return("control table column names must not be NA")
  }
  if( (length(controlTableKeys)<1) || (!is.character(controlTableKeys)) ) {
    return("controlTableKeys must be non-empty character")
  }
  if(ncol(controlTable)<=length(controlTableKeys)) {
    return("control table must have more columns than controlTableKeys")
  }
  if(length(setdiff(controlTableKeys, colnames(controlTable)))>0) {
    return("all controlTableKeys must be controlTable column names")
  }
  classes <- vapply(controlTable, class, character(1))
  if(!all(classes=='character')) {
    return("all control table columns must be character")
  }
  if(any(is.na(controlTable[, controlTableKeys, drop = FALSE]))) {
    return("control table key values must not be NA")
  }
  if(!checkColsFormUniqueKeys(controlTable, controlTableKeys)) {
    return("controlTable rows must be uniquely keyed by controlTableKeys key columns")
  }
  toCheck <- list(
    "column names" = colnames(controlTable),
    "keys" = unlist(controlTable[, controlTableKeys], use.names = FALSE),
    "values" = unlist(controlTable, use.names = FALSE) # overlaps, but keys will catch first
  )
  for(ci in names(toCheck)) {
    vals <- toCheck[[ci]]
    if(length(vals)<=0) {
      return(paste("control table", ci, "must not be empty"))
    }
    if(!is.character(vals)) {
      return(paste("all control table", ci, "must be character"))
    }
    if(any(nchar(vals)<=0)) {
      return(paste("all control table", ci, "must not be empty strings"))
    }
    if(strict) {
      if(length(grep(".", vals, fixed=TRUE))>0) {
        return(paste("all control table", ci ,"must '.'-free"))
      }
      if(!all(vals==make.names(vals))) {
        return(paste("all control table", ci ,"must be valid R variable names"))
      }
    }
  }
  return(NULL) # good
}


check_rowrecs_to_blocks_args <- function(...,
                                         wideTable_columns,
                                         controlTable,
                                         checkNames,
                                         strict,
                                         controlTableKeys,
                                         columnsToCopy) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::check_rowrecs_to_blocks_args")
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::check_rowrecs_to_blocks_args", cCheck))
  }
  bad_copy_cols <- setdiff(columnsToCopy, wideTable_columns)
  if(length(bad_copy_cols)>0) {
    stop(paste0("cdata::check_rowrecs_to_blocks_args bad columnsToCopy: ",
                paste(bad_copy_cols, collapse = ", ")))
  }
  # check keying is present
  # TODO: impl
  # check for production collisions
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  producing_columns <- controlTableKeys
  surviving_columns <- columnsToCopy
  collisions <- intersect(producing_columns, surviving_columns)
  if(length(collisions)>0) {
    stop(paste("cdata::check_rowrecs_to_blocks_args columns being produced collide with copied columns:",
               paste(collisions, collapse = ", ")))
  }
  # check more
  if(checkNames) {
    interiorCells <- unlist(controlTable[, controlTableValueColumns], use.names = FALSE)
    interiorCells <- interiorCells[!is.na(interiorCells)]
    wideTableColnames <- wideTable_columns
    badCells <- setdiff(interiorCells, wideTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::check_rowrecs_to_blocks_args: control table entries that are not wideTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }
  invisible(NULL) # all good
}

check_blocks_to_rowrecs_args <- function(...,
                                         tallTable_columns,
                                         keyColumns,
                                         controlTable,
                                         columnsToCopy,
                                         checkNames,
                                         strict,
                                         controlTableKeys) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::check_blocks_to_rowrecs_args controlTable")
  if(length(keyColumns)<=0) {
    # avoid no-keys case
    keyColumns <- "cdata_key_column"
    tallTable_columns <- c(tallTable_columns, keyColumns)
  }
  bad_key_cols <- setdiff(keyColumns, tallTable_columns)
  if(length(bad_key_cols)>0) {
    stop(paste0("cdata::check_blocks_to_rowrecs_args bad keyColumns: ",
                paste(bad_key_cols, collapse = ", ")))
  }
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::check_blocks_to_rowrecs_args", cCheck))
  }
  # look for column production collisions
  double_copied <- as.character(intersect(keyColumns, columnsToCopy))
  if(length(double_copied)>0) {
    stop(paste("cdata::check_blocks_to_rowrecs_args common columns in keyColumns and columnsToCopy:",
               paste(double_copied, collapse = ", ")))
  }
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  producing_columns <- as.character(unique(unlist(controlTable[, controlTableValueColumns, drop=FALSE])))
  surviving_columns <- as.character(unique(c(keyColumns, columnsToCopy)))
  collisions <- intersect(producing_columns, surviving_columns)
  if(length(collisions)>0) {
    stop(paste("cdata::check_blocks_to_rowrecs_args columns being produced collide with copied columns:",
               paste(collisions, collapse = ", ")))
  }
  # check more
  if(checkNames) {
    tallTableColnames <- tallTable_columns
    badCells <- setdiff(colnames(controlTable), tallTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::check_blocks_to_rowrecs_args: control table column names that are not tallTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }
}

