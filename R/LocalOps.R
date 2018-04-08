



# confirm control table has uniqueness
checkControlTable <- function(controlTable, strict) {
  if(!is.data.frame(controlTable)) {
    return("control table must be a data.frame")
  }
  if(nrow(controlTable)<1) {
    return("control table must have at least 1 row")
  }
  if(ncol(controlTable)<1) {
    return("control table must have at least 1 column")
  }
  classes <- vapply(controlTable, class, character(1))
  if(!all(classes=='character')) {
    return("all control table columns must be character")
  }
  toCheck <- list(
    "column names" = colnames(controlTable),
    "group ids" = controlTable[, 1, drop=TRUE]
  )
  for(ci in names(toCheck)) {
    vals <- toCheck[[ci]]
    if(any(is.na(vals))) {
      return(paste("all control table", ci, "must not be NA"))
    }
    if(length(unique(vals))!=length(vals)) {
      return(paste("all control table", ci, "must be distinct"))
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



get_db_handle <- function(env) {
  need_close <- FALSE
  my_db <- NULL
  db_handle <- base::mget("winvector_temp_db_handle",
                          envir = env,
                          ifnotfound = list(NULL),
                          inherits = TRUE)[[1]]
  if(is.null(db_handle)) {
    if (requireNamespace("RSQLite", quietly = TRUE)) {
      my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      need_close = TRUE
    } else {
      stop("cdata needs a database connection to work, to supply one please either set 'winvector_temp_db_handle' or install the package 'RSQLite'.")
    }
  } else {
    my_db <- db_handle$db
  }
  list(my_db = my_db, need_close = need_close)
}



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
#' @param columnsToCopy character list of column names to copy
#' @param strict logical, if TRUE check control table contents for uniqueness
#' @param checkNames logical, if TRUE check names
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param env environment to look for "winvector_temp_db_handle" in.
#' @return long table built by mapping wideTable to one row per group
#'
#' @seealso \code{\link{build_unpivot_control}}, \code{\link{blocks_to_rowrecs_q}}
#'
#' @examples
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   # un-pivot example
#'   d <- data.frame(AUC = 0.6, R2 = 0.2)
#'   cT <- build_unpivot_control(nameForNewKeyColumn= 'meas',
#'                               nameForNewValueColumn= 'val',
#'                               columnsToTakeFrom= c('AUC', 'R2'))
#'   rowrecs_to_blocks(d, cT)
#' }
#'
#'
#' @export
#'
rowrecs_to_blocks <- function(wideTable,
                              controlTable,
                              ...,
                              columnsToCopy = NULL,
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL,
                              env = parent.frame()) {
  # TODO: non-database impl
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!is.data.frame(wideTable)) {
    stop("cdata::rowrecs_to_blocks wideTable shoud be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::rowrecs_to_blocks controlTable shoud be a data.frame")
  }
  wtname <- "cata_wide_tmp"
  dblist <- get_db_handle(env)
  need_close <- dblist$need_close
  my_db <- dblist$my_db
  rownames(wideTable) <- NULL # just in case
  DBI::dbWriteTable(my_db,
                    wtname,
                    wideTable,
                    temporary = TRUE)
  resName <- rowrecs_to_blocks_q(wideTable = wtname,
                                 controlTable = controlTable,
                                 my_db = my_db,
                                 columnsToCopy = columnsToCopy,
                                 tempNameGenerator = mk_tmp_name_source('mvtrq'),
                                 strict = strict,
                                 checkNames = checkNames,
                                 showQuery = showQuery,
                                 defaultValue = defaultValue)
  resData <- DBI::dbGetQuery(my_db, paste("SELECT * FROM", resName))
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", wtname))
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", resName))
  if(need_close) {
    DBI::dbDisconnect(my_db)
  }
  resData
}


#' Map sets rows to columns (takes a \code{data.frame}).
#'
#' Transform data facts from rows into additional columns using controlTable.
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
#' @param columnsToCopy character list of column names to copy
#' @param strict logical, if TRUE check control table contents for uniqueness
#' @param checkNames logical, if TRUE check names
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param dropDups logical if TRUE supress duplicate columns (duplicate determined by name, not content).
#' @param env environment to look for "winvector_temp_db_handle" in.
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{\link{rowrecs_to_blocks_q}}, \code{\link{build_pivot_control}}
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   # pivot example
#'   d <- data.frame(meas = c('AUC', 'R2'), val = c(0.6, 0.2))
#'
#'   cT <- build_pivot_control(d,
#'                             columnToTakeKeysFrom= 'meas',
#'                             columnToTakeValuesFrom= 'val')
#'   blocks_to_rowrecs(d,
#'                     keyColumns = NULL,
#'                     controlTable = cT)
#' }
#'
#' @export
#'
blocks_to_rowrecs <- function(tallTable,
                              keyColumns,
                              controlTable,
                              ...,
                              columnsToCopy = NULL,
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL,
                              dropDups = FALSE,
                              env = parent.frame()) {
  # TODO: non-database impl
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs")
  if(!is.data.frame(tallTable)) {
    stop("cdata::blocks_to_rowrecs tallTable shoud be a data.frame")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::blocks_to_rowrecs controlTable shoud be a data.frame")
  }
  dblist <- get_db_handle(env)
  need_close <- dblist$need_close
  my_db <- dblist$my_db
  talltbltmpnam <- "cdata_tall_tmp"
  rownames(tallTable) <- NULL # just in case
  DBI::dbWriteTable(my_db,
                    talltbltmpnam,
                    tallTable,
                    temporary = TRUE)
  resName <- blocks_to_rowrecs_q(tallTable = talltbltmpnam,
                                 keyColumns = keyColumns,
                                 controlTable = controlTable,
                                 my_db = my_db,
                                 columnsToCopy = columnsToCopy,
                                 tempNameGenerator = mk_tmp_name_source('mvtcq'),
                                 strict = strict,
                                 checkNames = checkNames,
                                 showQuery = showQuery,
                                 defaultValue = defaultValue,
                                 dropDups = dropDups)
  resData <- DBI::dbGetQuery(my_db, paste("SELECT * FROM", resName))
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", talltbltmpnam))
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", resName))
  if(need_close) {
    DBI::dbDisconnect(my_db)
  }
  resData
}





