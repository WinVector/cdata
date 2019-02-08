


# in-memory direct functionality


#' @importFrom stats as.formula
NULL




#' @export
#' @rdname build_pivot_control
build_pivot_control.default <- function(table,
                                        columnToTakeKeysFrom,
                                        columnToTakeValuesFrom,
                                        ...,
                                        prefix = columnToTakeKeysFrom,
                                        sep = NULL,
                                        tmp_name_source = wrapr::mk_tmp_name_source("bpcd"),
                                        temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::build_pivot_control")
  if(!is.data.frame(table)) {
    stop("build_pivot_control.default table should be a data.frame")
  }
  controlTable <- data.frame(vals = unique(table[[columnToTakeKeysFrom]]),
                             stringsAsFactors = FALSE)
  colnames(controlTable) <- columnToTakeKeysFrom
  controlTable[[columnToTakeKeysFrom]] <- as.character(controlTable[[columnToTakeKeysFrom]])
  controlTable[[columnToTakeValuesFrom]] <- controlTable[[columnToTakeKeysFrom]]
  if(!is.null(sep)) {
    controlTable[[columnToTakeValuesFrom]] <- paste(prefix,
                                                    controlTable[[columnToTakeValuesFrom]],
                                                    sep=sep)
  }
  controlTable
}


#' Build a rowrecs_to_blocks() control table that specifies a un-pivot (or "shred").
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param nameForNewKeyColumn character name of column to write new keys in.
#' @param nameForNewValueColumn character name of column to write new values in.
#' @param columnsToTakeFrom character array names of columns to take values from.
#' @param ... not used, force later args to be by name
#' @return control table
#'
#' @seealso \code{\link{rowrecs_to_blocks}}
#'
#' @examples
#'
#' build_unpivot_control("measurmentType", "measurmentValue", c("c1", "c2"))
#'
#' @export
build_unpivot_control <- function(nameForNewKeyColumn,
                                  nameForNewValueColumn,
                                  columnsToTakeFrom,
                                  ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::build_unpivot_control")
  controlTable <- data.frame(x = as.character(columnsToTakeFrom),
                             y = as.character(columnsToTakeFrom),
                             stringsAsFactors = FALSE)
  colnames(controlTable) <- c(nameForNewKeyColumn, nameForNewValueColumn)
  controlTable
}




#' @export
#' @rdname rowrecs_to_blocks
rowrecs_to_blocks.default <- function(wideTable,
                                      controlTable,
                                      ...,
                                      checkNames = TRUE,
                                      checkKeys = FALSE,
                                      strict = FALSE,
                                      controlTableKeys = colnames(controlTable)[[1]],
                                      columnsToCopy = NULL,
                                      tmp_name_source = wrapr::mk_tmp_name_source("rrtobd"),
                                      temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!is.data.frame(wideTable)) {
    stop("cdata::rowrecs_to_blocks.default wideTable should be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::rowrecs_to_blocks controlTable should be a data.frame")
  }
  rownames(wideTable) <- NULL
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::rowrecs_to_blocks", cCheck))
  }
  bad_copy_cols <- setdiff(columnsToCopy, colnames(wideTable))
  if(length(bad_copy_cols)>0) {
    stop(paste0("cdata::rowrecs_to_blocks bad columnsToCopy: ",
                paste(bad_copy_cols, collapse = ", ")))
  }
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  if(checkNames || checkKeys) {
    interiorCells <- unlist(controlTable[, controlTableValueColumns], use.names = FALSE)
    interiorCells <- interiorCells[!is.na(interiorCells)]
    wideTableColnames <- colnames(wideTable)
    badCells <- setdiff(interiorCells, wideTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::rowrecs_to_blocks: control table entries that are not wideTable column names:",
                 paste(badCells, collapse = ', ')))
    }
    if(checkKeys) {
      if(!checkColsFormUniqueKeys(wideTable, columnsToCopy)) {
        stop("cdata::rowrecs_to_blocks columnsToCopy do not uniquely key the rows")
      }
    }
  }

  n_row_in <- nrow(wideTable)
  n_rep <- nrow(controlTable)
  n_row_res <- n_rep*n_row_in
  # build and start filling in result
  res <- data.frame(x = seq_len(n_row_in))
  res[['x']] <- NULL
  for(cn in columnsToCopy) {
    res[[cn]] <- wideTable[[cn]]
  }
  for(cn in controlTableKeys) {
    res[[cn]] <- NA_character_
  }
  for(cn in controlTableValueColumns) {
    res[[cn]] <- wideTable[[controlTable[2, cn, drop = TRUE]]]
    # TODO: check this keeps class and works with dates
    res[[cn]][seq_len(n_row_in)] <- NA
  }
  # cross product with control table
  res <- res[sort(rep(seq_len(n_row_in), n_rep)), , drop = FALSE]
  rownames(res) <- NULL
  for(cn in controlTableKeys) {
    res[[cn]] <- rep(controlTable[[cn]], n_row_in)
  }
  # fill in values
  for(cn in controlTableValueColumns) {
    for(i in seq_len(n_rep)) {
      indxs <- i + n_rep*(0:(n_row_in-1))
      col <- controlTable[i, cn, drop = TRUE]
      res[[cn]][indxs] <- wideTable[[col]]
    }
  }
  rownames(res) <- NULL
  res
}


#' @export
#' @rdname blocks_to_rowrecs
blocks_to_rowrecs.default <- function(tallTable,
                                      keyColumns,
                                      controlTable,
                                      ...,
                                      columnsToCopy = NULL,
                                      checkNames = TRUE,
                                      checkKeys = TRUE,
                                      strict = FALSE,
                                      controlTableKeys = colnames(controlTable)[[1]],
                                      tmp_name_source = wrapr::mk_tmp_name_source("btrd"),
                                      temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs")
  if(!is.data.frame(tallTable)) {
    stop("cdata::blocks_to_rowrecs.default tallTable should be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::blocks_to_rowrecs controlTable should be a data.frame")
  }
  rownames(tallTable) <- NULL
  clear_key_column <- FALSE
  if(length(keyColumns)<=0) {
    # avoid no-keys case
    tallTable$cdata_key_column <- 1
    keyColumns <- "cdata_key_column"
    clear_key_column <- TRUE
  }
  bad_key_cols <- setdiff(keyColumns, colnames(tallTable))
  if(length(bad_key_cols)>0) {
    stop(paste0("cdata::blocks_to_rowrecs bad keyColumns: ",
                paste(bad_key_cols, collapse = ", ")))
  }
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::blocks_to_rowrecs", cCheck))
  }
  if( (length(controlTableKeys)!=1) || (!isTRUE(controlTableKeys==colnames(controlTable)[[1]])) ) {
    # TODO: extend code and remove this check
    stop("for now (soon to change) controlTableKeys must be exactly the first column name of controlTable")
  }
  if(checkNames || checkKeys) {
    tallTableColnames <- colnames(tallTable)
    badCells <- setdiff(colnames(controlTable), tallTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::blocks_to_rowrecs: control table column names that are not tallTable column names:",
                 paste(badCells, collapse = ', ')))
    }
    if(checkKeys) {
      # keys plot colnames(controlTable)[[1]] should uniquely identify rows
      if(!checkColsFormUniqueKeys(tallTable, c(keyColumns, colnames(controlTable)[[1]]))) {
        stop("cdata::blocks_to_rowrecs: keyColumns plus first column of control table do not uniquely key rows")
      }
      # only values expected in controlTable[[1]] should be in tallTable[[colnames(controlTable)[[1]]]]
      bkeys <- controlTable[[1]]
      bseen <- unique(tallTable[[colnames(controlTable)[[1]]]])
      bnovel <- setdiff(bseen, bkeys)
      if(length(bnovel)>0) {
        stop(paste("cdata::blocks_to_rowrecs: table values that are not block keys:",
                   paste(bnovel, collapse = ', ')))
      }
    }
  }

  # make simple grouping keys
  tallTable$cdata_group_key_col <- 1
  if(length(keyColumns)>=1) {
    cols <- as.list(tallTable[ , keyColumns, drop=FALSE])
    names(cols) <- NULL
    keys <- do.call("paste", c(cols, sep = " CDATA_SEP "))
    tallTable$cdata_group_key_col <- match(keys, keys)
    tallTable <- tallTable[order(tallTable$cdata_group_key_col), , drop = FALSE]
  }
  first_idxs <- match(unique(tallTable$cdata_group_key_col), tallTable$cdata_group_key_col)
  res <- tallTable[first_idxs,
                   c("cdata_group_key_col", keyColumns, columnsToCopy),
                   drop = FALSE]
  rownames(res) <- NULL
  n_res <- nrow(res)
  # fill in values
  meas_col <- colnames(controlTable)[[1]]
  n_rep <- nrow(controlTable)
  for(ci in 2:ncol(controlTable)) {
    cn <- colnames(controlTable)[[ci]]
    for(i in seq_len(n_rep)) {
      srccol <- controlTable[i, 1, drop = TRUE]
      destcol <- controlTable[[cn]][i]
      indxs <- which(tallTable[[meas_col]] == srccol)
      vals <- tallTable[[cn]][indxs]
      res[[destcol]] <- vals[[1]]
      res[[destcol]][seq_len(n_res)] <- NA
      posns <- match(res$cdata_group_key_col,
                     tallTable$cdata_group_key_col[indxs])
      lhs <- seq_len(n_res)
      lhs <- lhs[!is.na(posns)]
      posns <- posns[!is.na(posns)]
      res[[destcol]][lhs] <- vals[posns]
    }
  }
  res$cdata_group_key_col <- NULL
  if(clear_key_column) {
    res$cdata_key_column <- NULL
  }
  rownames(res) <- NULL
  res
}





