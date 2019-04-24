

#' @importFrom wrapr %.>%
#' @importFrom stats complete.cases
#' @importFrom wrapr let
NULL







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
  if(!check_cols_form_unique_keys(controlTable, controlTableKeys)) {
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

