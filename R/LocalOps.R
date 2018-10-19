


# in-memory direct functionality


#' @importFrom stats as.formula
NULL


#' Build a blocks_to_rowrecs()/rowrecs_to_blocks() control table that specifies a pivot from a \code{data.frame}.
#'
#' Some discussion and examples can be found here: \url{https://winvector.github.io/FluidData/FluidData.html}.
#'
#' @param table data.frame to scan for new column names (in-memory data.frame).
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param ... not used, force later args to be by name
#' @param prefix column name prefix (only used when sep is not NULL)
#' @param sep separator to build complex column names.
#' @return control table
#'
#' @seealso \code{\link{blocks_to_rowrecs}}, \code{\link{build_pivot_control_q}}
#'
#' @examples
#'
#'   d <- data.frame(measType = c("wt", "ht"),
#'                   measValue = c(150, 6),
#'                   stringsAsFactors = FALSE)
#'   build_pivot_control(d,
#'                       'measType', 'measValue',
#'                       sep = '_')
#'
#' @export
build_pivot_control <- function(table,
                                columnToTakeKeysFrom,
                                columnToTakeValuesFrom,
                                ...,
                                prefix = columnToTakeKeysFrom,
                                sep = NULL) {
  UseMethod("build_pivot_control")
}

#' @export
#' @rdname build_pivot_control
build_pivot_control.default <- function(table,
                                        columnToTakeKeysFrom,
                                        columnToTakeValuesFrom,
                                        ...,
                                        prefix = columnToTakeKeysFrom,
                                        sep = NULL) {
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
#' @seealso \code{\link{rowrecs_to_blocks_q}}, \code{\link{rowrecs_to_blocks}}
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


# unpack control table into maps
build_transform_maps <- function(controlTable) {
  cCheck <- checkControlTable(controlTable, FALSE)
  if(!is.null(cCheck)) {
    stop(paste("cdata:::build_transform_maps", cCheck))
  }
  # use control table to get into a triple-form (only one data column, all others keys).
  cells <- as.character(unlist(unlist(controlTable[, -1])))
  cells_to_row_labels <- controlTable
  for(i in 1:nrow(controlTable)) {
    cells_to_row_labels[i, ] <- cells_to_row_labels[i, 1]
  }
  cells_to_row_labels <- as.character(unlist(cells_to_row_labels[, -1]))
  names(cells_to_row_labels) <- cells
  cells_to_col_labels <- controlTable
  for(j in 2:ncol(controlTable)) {
    cells_to_col_labels[, j] <- colnames(controlTable)[[j]]
  }
  cells_to_col_labels <- as.character(unlist(cells_to_col_labels[, -1]))
  names(cells_to_col_labels) <- cells
  rows_cols_to_cells <- cells
  names(rows_cols_to_cells) <- paste(cells_to_row_labels, ",", cells_to_col_labels)
  list(
    cells = cells,
    cells_to_row_labels = cells_to_row_labels,
    cells_to_col_labels = cells_to_col_labels,
    rows_cols_to_cells = rows_cols_to_cells
  )
}


#' Map a set of columns to rows (takes a \code{data.frame}).
#'
#' Transform data facts from columns into additional rows controlTable.
#'
#'
#' This is using the theory of "fluid data"
#' (\url{https://github.com/WinVector/cdata}), which includes the
#' principle that each data cell has coordinates independent of the
#' storage details and storage detail dependent coordinates (usually
#' row-id, column-id, and group-id) can be re-derived at will (the
#' other principle is that there may not be "one true preferred data
#' shape" and many re-shapings of data may be needed to match data to
#' different algorithms and methods).
#'
#' The controlTable defines the names of each data element in the two notations:
#' the notation of the tall table (which is row oriented)
#' and the notation of the wide table (which is column oriented).
#' controlTable[ , 1] (the group label) cross colnames(controlTable)
#' (the column labels) are names of data cells in the long form.
#' controlTable[ , 2:ncol(controlTable)] (column labels)
#' are names of data cells in the wide form.
#' To get behavior similar to tidyr::gather/spread one builds the control table
#' by running an appropriate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param wideTable data.frame containing data to be mapped (in-memory data.frame).
#' @param controlTable table specifying mapping (local data frame).
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character array of column names to copy.
#' @param checkNames logical, if TRUE check names.
#' @param checkKeys logical, if TRUE check columnsToCopy form row keys (not a requirement, unless you want to be able to invert the operation).
#' @param strict logical, if TRUE check control table name forms.
#' @param use_data_table logical if TRUE try to use data.table for the un-pivots.
#' @return long table built by mapping wideTable to one row per group
#'
#' @seealso \code{\link{build_unpivot_control}}, \code{\link{blocks_to_rowrecs_q}}
#'
#' @examples
#'
#'   # un-pivot example
#'   d <- data.frame(AUC = 0.6, R2 = 0.2)
#'   cT <- build_unpivot_control(nameForNewKeyColumn= 'meas',
#'                               nameForNewValueColumn= 'val',
#'                               columnsToTakeFrom= c('AUC', 'R2'))
#'   rowrecs_to_blocks(d, cT)
#'
#'
#' @export
#'
rowrecs_to_blocks <- function(wideTable,
                              controlTable,
                              ...,
                              checkNames = TRUE,
                              checkKeys = FALSE,
                              strict = FALSE,
                              columnsToCopy = NULL,
                              use_data_table = FALSE) {
  UseMethod("rowrecs_to_blocks")
}

#' @export
#' @rdname rowrecs_to_blocks
rowrecs_to_blocks.default <- function(wideTable,
                                      controlTable,
                                      ...,
                                      checkNames = TRUE,
                                      checkKeys = FALSE,
                                      strict = FALSE,
                                      columnsToCopy = NULL,
                                      use_data_table = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!is.data.frame(wideTable)) {
    stop("cdata::rowrecs_to_blocks.default wideTable should be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::rowrecs_to_blocks controlTable should be a data.frame")
  }
  rownames(wideTable) <- NULL
  cCheck <- checkControlTable(controlTable, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::rowrecs_to_blocks", cCheck))
  }
  bad_copy_cols <- setdiff(columnsToCopy, colnames(wideTable))
  if(length(bad_copy_cols)>0) {
    stop(paste0("cdata::rowrecs_to_blocks bad columnsToCopy: ",
                paste(bad_copy_cols, collapse = ", ")))
  }
  if(checkNames || checkKeys) {
    interiorCells <- as.vector(as.matrix(controlTable[,2:ncol(controlTable)]))
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

  if( use_data_table &&
      requireNamespace("data.table", quietly = TRUE) ) {
    maps <- build_transform_maps(controlTable)

    # from rowrec to one value per row form (triple-like)
    d_thin_r <- data.table::melt.data.table(data.table::as.data.table(wideTable),
                                            variable.name = "cdata_cell_label",
                                            value.name = "cdata_cell_value",
                                            id.vars = columnsToCopy,
                                            measure.vars = maps$cells)
    d_thin_r$cdata_row_label <- maps$cells_to_row_labels[d_thin_r$cdata_cell_label]
    d_thin_r$cdata_col_label <- maps$cells_to_col_labels[d_thin_r$cdata_cell_label]

    # cast to block form, note: if cdata_col_label isn't varying then don't need this step.
    f <- paste0(paste(c(columnsToCopy, "cdata_row_label"), collapse = " + "), " ~ ", "cdata_col_label")
    r <- data.table::dcast.data.table(d_thin_r, as.formula(f), value.var = "cdata_cell_value")
    colnames(r)[which(colnames(r)=="cdata_row_label")] <- colnames(controlTable)[[1]]
    rownames(r) <- NULL
    return(as.data.frame(r))
  }

  if( use_data_table ) {
    warning("cdata::rowrecs_to_blocks use_data_table==TRUE requires data.table package")
  }

  # fall back to local impl

  n_row_in <- nrow(wideTable)
  n_rep <- nrow(controlTable)
  n_row_res <- n_rep*n_row_in
  # build and start filling in result
  res <- data.frame(x = seq_len(n_row_in))
  res[['x']] <- NULL
  for(ci in columnsToCopy) {
    res[[ci]] <- wideTable[[ci]]
  }
  res[[colnames(controlTable)[[1]]]] <- NA_character_
  for(ci in 2:ncol(controlTable)) {
    cn <- colnames(controlTable)[[ci]]
    res[[cn]] <- wideTable[[controlTable[2, ci, drop = TRUE]]]
    # TODO: check this keeps class and works with dates
    res[[cn]][seq_len(n_row_in)] <- NA
  }
  # cross product with control table
  res <- res[sort(rep(seq_len(n_row_in), n_rep)), , drop = FALSE]
  rownames(res) <- NULL
  res[[colnames(controlTable)[[1]]]] <- rep(controlTable[[1]], n_row_in)
  # fill in values
  for(ci in 2:ncol(controlTable)) {
    cn <- colnames(controlTable)[[ci]]
    for(i in seq_len(n_rep)) {
      indxs <- i + n_rep*(0:(n_row_in-1))
      col <- controlTable[i, ci, drop = TRUE]
      res[[cn]][indxs] <- wideTable[[col]]
    }
  }
  rownames(res) <- NULL
  res
}


#' Map sets rows to columns (takes a \code{data.frame}).
#'
#' Transform data facts from rows into additional columns using controlTable.
#'
#'
#' This is using the theory of "fluid data"n
#' (\url{https://github.com/WinVector/cdata}), which includes the
#' principle that each data cell has coordinates independent of the
#' storage details and storage detail dependent coordinates (usually
#' row-id, column-id, and group-id) can be re-derived at will (the
#' other principle is that there may not be "one true preferred data
#' shape" and many re-shapings of data may be needed to match data to
#' different algorithms and methods).
#'
#' The controlTable defines the names of each data element in the two notations:
#' the notation of the tall table (which is row oriented)
#' and the notation of the wide table (which is column oriented).
#' controlTable[ , 1] (the group label) cross colnames(controlTable)
#' (the column labels) are names of data cells in the long form.
#' controlTable[ , 2:ncol(controlTable)] (column labels)
#' are names of data cells in the wide form.
#' To get behavior similar to tidyr::gather/spread one builds the control table
#' by running an appropriate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param tallTable data.frame containing data to be mapped (in-memory data.frame).
#' @param keyColumns character vector of column defining row groups
#' @param controlTable table specifying mapping (local data frame)
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character, extra columns to copy.
#' @param checkNames logical, if TRUE check names.
#' @param checkKeys logical, if TRUE check keyColumns uniquely identify blocks (required).
#' @param strict logical, if TRUE check control table name forms
#' @param use_data_table logical if TRUE try to use data.table for the pivots.
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{\link{build_pivot_control}}, \code{\link{rowrecs_to_blocks_q}}
#'
#' @examples
#'
#'   # pivot example
#'   d <- data.frame(meas = c('AUC', 'R2'),
#'                   val = c(0.6, 0.2))
#'
#'   cT <- build_pivot_control(d,
#'                             columnToTakeKeysFrom= 'meas',
#'                             columnToTakeValuesFrom= 'val')
#'   blocks_to_rowrecs(d,
#'                     keyColumns = NULL,
#'                     controlTable = cT)
#'
#' @export
#'
blocks_to_rowrecs <- function(tallTable,
                              keyColumns,
                              controlTable,
                              ...,
                              columnsToCopy = NULL,
                              checkNames = TRUE,
                              checkKeys = TRUE,
                              strict = FALSE,
                              use_data_table = FALSE) {
  UseMethod("blocks_to_rowrecs")
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
                                      use_data_table = TRUE) {
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
  cCheck <- checkControlTable(controlTable, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::blocks_to_rowrecs", cCheck))
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

  if( use_data_table &&
      requireNamespace("data.table", quietly = TRUE) ) {
    maps <- build_transform_maps(controlTable)

    # from block form to one value per row form (triple-like)
    d_thin_b <- data.table::melt.data.table(data.table::as.data.table(tallTable),
                                            variable.name = "cdata_col_label",
                                            value.name = "cdata_cell_value",
                                            id.vars = c(keyColumns, colnames(controlTable)[[1]]),
                                            measure.vars = colnames(controlTable)[-1])
    d_thin_b$cdata_row_label <- d_thin_b[[colnames(controlTable)[[1]]]]
    d_thin_b[[colnames(controlTable)[[1]]]] <- NULL
    d_thin_b$cdata_cell_label <- maps$rows_cols_to_cells[paste(d_thin_b$cdata_row_label, ",", d_thin_b$cdata_col_label)]

    # cast to rowrec form
    f <- paste0(paste(keyColumns, collapse = " + "), " ~ ", "cdata_cell_label")
    r <- data.table::dcast.data.table(d_thin_b, as.formula(f), value.var = "cdata_cell_value")
    if(clear_key_column) {
      r$cdata_key_column <- NULL
    }
    rownames(r) <- NULL
    return(as.data.frame(r))
  }

  if( use_data_table ) {
    warning("cdata::blocks_to_rowrecs use_data_table==TRUE requires data.table package")
  }

  # fall back to local impl

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





