


# in-memory direct functionality





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
#' @seealso \code{\link{blocks_to_rowrecs}}
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
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::build_pivot_control")
  if(!is.data.frame(table)) {
    stop("build_pivot_control table should be a data.frame")
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



#' Map a set of columns to rows (takes a \code{data.frame}).
#'
#' Transform data facts from columns into additional rows controlTable.
#'
#' TODO: test and and checks.
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
#' by running an appropiate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param wideTable data.frame containing data to be mapped (in-memory data.frame).
#' @param controlTable table specifying mapping (local data frame).
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character array of column names to copy
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
                              columnsToCopy = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!is.data.frame(wideTable)) {
    stop("cdata::rowrecs_to_blocks wideTable shoud be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::rowrecs_to_blocks controlTable shoud be a data.frame")
  }
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
  res
}


#' Map sets rows to columns (takes a \code{data.frame}).
#'
#' Transform data facts from rows into additional columns using controlTable.
#'
#' TODO: test and and checks.
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
#' by running an appropiate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param tallTable data.frame containing data to be mapped (in-memory data.frame).
#' @param keyColumns character list of column defining row groups
#' @param controlTable table specifying mapping (local data frame)
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character, extra columns to copy (aribrary which row per group).
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{\link{rowrecs_to_blocks_q}}, \code{\link{build_pivot_control}}
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
                              columnsToCopy = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs")
  if(!is.data.frame(tallTable)) {
    stop("cdata::blocks_to_rowrecs tallTable shoud be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::blocks_to_rowrecs controlTable shoud be a data.frame")
  }
  # make simple grouping keys
  tallTable$cdata_group_key_col <- ""
  if(length(keyColumns)>=1) {
    cols <- as.list(tallTable[ , keyColumns, drop=FALSE])
    names(cols) <- NULL
    tallTable$cdata_group_key_col <- do.call("paste",
                                             c(cols, sep = " CDATA_SEP "))
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
      vals <- tallTable[[cn]][[indxs]]
      res[[destcol]] <- vals[[1]]
      res[[destcol]][seq_len(n_res)] <- NA
      posns <- match(res$cdata_group_key_col,
                     tallTable$cdata_group_key_col[indxs])
      posns <- posns[!is.na(posns)]
      res[[destcol]][posns] <- vals
    }
  }
  res$cdata_group_key_col <- NULL
  res
}





