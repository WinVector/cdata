
# Contributed by John Mount jmount@win-vector.com , ownership assigned to Win-Vector LLC.
# Win-Vector LLC currently distributes this code without intellectual property indemnification, warranty, claim of fitness of purpose, or any other guarantee under a GPL3 license.

#' @importFrom wrapr %.>% let mapsyms
NULL



listFields <- function(my_db, tableName) {
  # fails intermitnently, and sometimes gives wrong results
  # filed as: https://github.com/tidyverse/dplyr/issues/3204
  # tryCatch(
  #   return(DBI::dbListFields(my_db, tableName)),
  #   error = function(e) { NULL })
  # below is going to have issues to to R-column name conversion!
  q <- paste0("SELECT * FROM ",
              DBI::dbQuoteIdentifier(my_db, tableName),
              " LIMIT 1")
  v <- DBI::dbGetQuery(my_db, q)
  colnames(v)
}



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



#' Build a moveValuesToColumns*() control table that specifies a un-pivot.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/replyr/articles/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param nameForNewKeyColumn character name of column to write new keys in.
#' @param nameForNewValueColumn character name of column to write new values in.
#' @param columnsToTakeFrom character array names of columns to take values from.
#' @param ... not used, force later args to be by name
#' @return control table
#'
#' @seealso \code{{moveValuesToRows}}, \code{\link{moveValuesToRowsN}}
#'
#' @examples
#'
#' buildUnPivotControlTable("measurmentType", "measurmentValue", c("c1", "c2"))
#'
#' @export
buildUnPivotControlTable <- function(nameForNewKeyColumn,
                                     nameForNewValueColumn,
                                     columnsToTakeFrom,
                                     ...) {
  if(length(list(...))>0) {
    stop("cdata::buildUnPivotControlTable unexpected arguments.")
  }
  controlTable <- data.frame(x = as.character(columnsToTakeFrom),
                             y = as.character(columnsToTakeFrom),
                             stringsAsFactors = FALSE)
  colnames(controlTable) <- c(nameForNewKeyColumn, nameForNewValueColumn)
  controlTable
}




#' Map a set of columns to rows (query based).
#'
#' Transform data facts from columns into additional rows using SQL
#' and controlTable.
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
#' \url{https://winvector.github.io/replyr/articles/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param wideTable name of table containing data to be mapped (db/Spark data)
#' @param controlTable table specifying mapping (local data frame)
#' @param my_db db handle
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character list of column names to copy
#' @param tempNameGenerator a tempNameGenerator from cdata::makeTempNameGenerator()
#' @param strict logical, if TRUE check control table contents for uniqueness
#' @param checkNames logical, if TRUE check names
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @return long table built by mapping wideTable to one row per group
#'
#' @seealso \code{{moveValuesToRows}}, \code{\link{buildUnPivotControlTable}}, \code{\link{moveValuesToColumnsN}}
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # un-pivot example
#' d <- data.frame(AUC = 0.6, R2 = 0.2)
#' DBI::dbWriteTable(my_db,
#'                   'd',
#'                   d,
#'                   overwrite = TRUE,
#'                   temporary = TRUE)
#' cT <- buildUnPivotControlTable(nameForNewKeyColumn= 'meas',
#'                                nameForNewValueColumn= 'val',
#'                                columnsToTakeFrom= c('AUC', 'R2'))
#' tab <- moveValuesToRowsN('d', cT, my_db = my_db)
#' DBI::dbGetQuery(my_db, paste("SELECT * FROM", tab))
#'
#'
#' @export
#'
moveValuesToRowsN <- function(wideTable,
                              controlTable,
                              my_db,
                              ...,
                              columnsToCopy = NULL,
                              tempNameGenerator = makeTempNameGenerator('mvtrq'),
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL) {
  if(length(list(...))>0) {
    stop("cdata::moveValuesToRowsN unexpected arguments.")
  }
  if(length(columnsToCopy)>0) {
    if(!is.character(columnsToCopy)) {
      stop("moveValuesToRowsN: columnsToCopy must be character")
    }
  }
  if((!is.character(wideTable))||(length(wideTable)!=1)) {
    stop("moveValuesToRowsN: wideTable must be the name of a remote table")
  }
  controlTable <- as.data.frame(controlTable)
  cCheck <- checkControlTable(controlTable, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::moveValuesToRowsN", cCheck))
  }
  if(checkNames) {
    interiorCells <- as.vector(as.matrix(controlTable[,2:ncol(controlTable)]))
    interiorCells <- interiorCells[!is.na(interiorCells)]
    wideTableColnames <- listFields(my_db, wideTable)
    badCells <- setdiff(interiorCells, wideTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::moveValuesToRowsN: control table entries that are not wideTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }
  ctabName <- tempNameGenerator()
  DBI::dbWriteTable(my_db,
                    ctabName,
                    controlTable,
                    temporary = TRUE)
  resName <- tempNameGenerator()
  missingCaseTerm = "NULL"
  if(!is.null(defaultValue)) {
    if(is.numeric(defaultValue)) {
      missingCaseTerm <- as.character(defaultValue)
    } else {
      missingCaseTerm <- DBI::dbQuoteString(paste(as.character(defaultValue),
                                                  collapse = ' '))
    }
  }
  casestmts <- lapply(2:ncol(controlTable),
                      function(j) {
                        whens <- lapply(seq_len(nrow(controlTable)),
                                        function(i) {
                                          cij <- controlTable[i,j,drop=TRUE]
                                          if(is.null(cij) || is.na(cij)) {
                                            return(NULL)
                                          }
                                          paste0(' WHEN b.',
                                                 DBI::dbQuoteIdentifier(my_db, colnames(controlTable)[1]),
                                                 ' = ',
                                                 DBI::dbQuoteString(my_db, controlTable[i,1,drop=TRUE]),
                                                 ' THEN a.',
                                                 DBI::dbQuoteIdentifier(my_db, cij))
                                        })
                        whens <- as.character(Filter(function(x) { !is.null(x) },
                                                     whens))
                        if(length(whens)<=0) {
                          return(NULL)
                        }
                        casestmt <- paste0('CASE ',
                                           paste(whens, collapse = ' '),
                                           ' ELSE ',
                                           missingCaseTerm,
                                           ' END AS ',
                                           DBI::dbQuoteIdentifier(my_db, colnames(controlTable)[j]))
                      })
  casestmts <- as.character(Filter(function(x) { !is.null(x) },
                                   casestmts))
  copystmts <- NULL
  if(length(columnsToCopy)>0) {
    copystmts <- paste0('a.', DBI::dbQuoteIdentifier(my_db, columnsToCopy))
  }
  groupstmt <- paste0('b.', DBI::dbQuoteIdentifier(my_db, colnames(controlTable)[1]))
  # deliberate cross join
  qs <-  paste0(" SELECT ",
                paste(c(copystmts, groupstmt, casestmts), collapse = ', '),
                ' FROM ',
                DBI::dbQuoteIdentifier(my_db, wideTable),
                ' a CROSS JOIN ',
                DBI::dbQuoteIdentifier(my_db, ctabName),
                ' b ')
  q <-  paste0("CREATE TABLE ",
               DBI::dbQuoteIdentifier(my_db, resName),
               " AS ",
               qs)
  if(showQuery) {
    print(q)
  }
  tryCatch(
    # sparklyr didn't implement dbExecute(), so using dbGetQuery()
    DBI::dbGetQuery(my_db, q),
    warning = function(w) { NULL })
  resName
}


#' Map a set of columns to rows (query based).
#'
#' Transform data facts from columns into additional rows using SQL
#' and controlTable.
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
#' \url{https://winvector.github.io/replyr/articles/FluidData.html} and
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
#' @return long table built by mapping wideTable to one row per group
#'
#' @seealso \code{{moveValuesToRows}}, \code{\link{buildUnPivotControlTable}}, \code{\link{moveValuesToColumnsN}}
#'
#' @examples
#'
#' # un-pivot example
#' d <- data.frame(AUC = 0.6, R2 = 0.2)
#' cT <- buildUnPivotControlTable(nameForNewKeyColumn= 'meas',
#'                                nameForNewValueColumn= 'val',
#'                                columnsToTakeFrom= c('AUC', 'R2'))
#' tab <- moveValuesToRowsD(d, cT)
#'
#'
#' @export
#'
moveValuesToRowsD <- function(wideTable,
                              controlTable,
                              ...,
                              columnsToCopy = NULL,
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL) {
  if(length(list(...))>0) {
    stop("cdata::moveValuesToRowsD unexpected arguments.")
  }
  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(my_db,
                    'wideTable',
                    wideTable,
                    overwrite = TRUE,
                    temporary = TRUE)
  resName <- moveValuesToRowsN(wideTable = 'wideTable',
                               controlTable = controlTable,
                               my_db = my_db,
                               columnsToCopy = columnsToCopy,
                               tempNameGenerator = makeTempNameGenerator('mvtrq'),
                               strict = strict,
                               checkNames = checkNames,
                               showQuery = showQuery,
                               defaultValue = defaultValue)
  resData <- DBI::dbGetQuery(my_db, paste("SELECT * FROM", resName))
  DBI::dbDisconnect(my_db)
  resData
}



#' Build a moveValuesToColumns*() control table that specifies a pivot.
#'
#' Some discussion and examples can be found here: \url{https://winvector.github.io/replyr/articles/FluidData.html}.
#'
#' @param tableName Name of table to scan for new column names.
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param my_db db handle
#' @param ... not used, force later args to be by name
#' @param prefix column name prefix (only used when sep is not NULL)
#' @param sep separator to build complex column names.
#' @return control table
#'
#' @seealso \url{https://github.com/WinVector/cdata}, \code{{moveValuesToRows}}, \code{{moveValuesToColumns}}, \code{\link{moveValuesToRowsN}}, \code{\link{moveValuesToColumnsN}}
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- data.frame(measType = c("wt", "ht"),
#'                 measValue = c(150, 6),
#'                 stringsAsFactors = FALSE)
#' DBI::dbWriteTable(my_db,
#'                   'd',
#'                   d,
#'                   overwrite = TRUE,
#'                   temporary = TRUE)
#' buildPivotControlTableN('d', 'measType', 'measValue',
#'                                  my_db = my_db,
#'                                  sep = '_')
#'
#' @export
buildPivotControlTableN <- function(tableName,
                                    columnToTakeKeysFrom,
                                    columnToTakeValuesFrom,
                                    my_db,
                                    ...,
                                    prefix = columnToTakeKeysFrom,
                                    sep = NULL) {
  if(length(list(...))>0) {
    stop("cdata::buildPivotControlTableN unexpected arguments.")
  }
  q <- paste0("SELECT ",
              DBI::dbQuoteIdentifier(my_db, columnToTakeKeysFrom),
              " FROM ",
              DBI::dbQuoteIdentifier(my_db, tableName),
              " GROUP BY ",
              DBI::dbQuoteIdentifier(my_db, columnToTakeKeysFrom))
  controlTable <- DBI::dbGetQuery(my_db, q)
  controlTable[[columnToTakeKeysFrom]] <- as.character(controlTable[[columnToTakeKeysFrom]])
  controlTable[[columnToTakeValuesFrom]] <- controlTable[[columnToTakeKeysFrom]]
  if(!is.null(sep)) {
    controlTable[[columnToTakeValuesFrom]] <- paste(prefix,
                                                 controlTable[[columnToTakeValuesFrom]],
                                                 sep=sep)
  }
  controlTable
}



#' Build a moveValuesToColumns*() control table that specifies a pivot.
#'
#' Some discussion and examples can be found here: \url{https://winvector.github.io/replyr/articles/FluidData.html}.
#'
#' @param table data.frame to scan for new column names (in-memory data.frame).
#' @param columnToTakeKeysFrom character name of column build new column names from.
#' @param columnToTakeValuesFrom character name of column to get values from.
#' @param ... not used, force later args to be by name
#' @param prefix column name prefix (only used when sep is not NULL)
#' @param sep separator to build complex column names.
#' @return control table
#'
#' @seealso \url{https://github.com/WinVector/cdata}, \code{{moveValuesToRows}}, \code{{moveValuesToColumns}}, \code{\link{moveValuesToRowsN}}, \code{\link{moveValuesToColumnsN}}
#'
#' @examples
#'
#' d <- data.frame(measType = c("wt", "ht"),
#'                 measValue = c(150, 6),
#'                 stringsAsFactors = FALSE)
#' buildPivotControlTableD(d,
#'                         'measType', 'measValue',
#'                         sep = '_')
#'
#' @export
buildPivotControlTableD <- function(table,
                                    columnToTakeKeysFrom,
                                    columnToTakeValuesFrom,
                                    ...,
                                    prefix = columnToTakeKeysFrom,
                                    sep = NULL) {
  if(length(list(...))>0) {
    stop("cdata::buildPivotControlTableD unexpected arguments.")
  }
  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(my_db,
                    'table',
                    table,
                    overwrite = TRUE,
                    temporary = TRUE)
  res <- buildPivotControlTableN(tableName = 'table',
                                 columnToTakeKeysFrom = columnToTakeKeysFrom,
                                 columnToTakeValuesFrom = columnToTakeValuesFrom,
                                 my_db = my_db,
                                 prefix = prefix,
                                 sep = sep)
  DBI::dbDisconnect(my_db)
  res
}



#' Map sets rows to columns (query based).
#'
#' Transform data facts from rows into additional columns using SQL
#' and controlTable.
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
#' \url{https://winvector.github.io/replyr/articles/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param tallTable name of table containing data to be mapped (db/Spark data)
#' @param keyColumns character list of column defining row groups
#' @param controlTable table specifying mapping (local data frame)
#' @param my_db db handle
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character list of column names to copy
#' @param tempNameGenerator a tempNameGenerator from cdata::makeTempNameGenerator()
#' @param strict logical, if TRUE check control table contents for uniqueness
#' @param checkNames logical, if TRUE check names
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param dropDups logical if TRUE supress duplicate columns (duplicate determined by name, not content).
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{{moveValuesToColumns}}, \code{\link{moveValuesToRowsN}}, \code{\link{buildPivotControlTableN}}
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' # pivot example
#' d <- data.frame(meas = c('AUC', 'R2'), val = c(0.6, 0.2))
#' DBI::dbWriteTable(my_db,
#'                   'd',
#'                   d,
#'                   temporary = TRUE)
#' cT <- buildPivotControlTableN('d',
#'                                        columnToTakeKeysFrom= 'meas',
#'                                        columnToTakeValuesFrom= 'val',
#'                                        my_db = my_db)
#' tab <- moveValuesToColumnsN('d',
#'                                      keyColumns = NULL,
#'                                      controlTable = cT,
#'                                      my_db = my_db)
#' DBI::dbGetQuery(my_db, paste("SELECT * FROM", tab))
#'
#'
#' @export
#'
moveValuesToColumnsN <- function(tallTable,
                                 keyColumns,
                                 controlTable,
                                 my_db,
                                 ...,
                                 columnsToCopy = NULL,
                                 tempNameGenerator = makeTempNameGenerator('mvtcq'),
                                 strict = FALSE,
                                 checkNames = TRUE,
                                 showQuery = FALSE,
                                 defaultValue = NULL,
                                 dropDups = FALSE) {
  if(length(list(...))>0) {
    stop("cdata::moveValuesToColumnsN unexpected arguments.")
  }
  if(length(keyColumns)>0) {
    if(!is.character(keyColumns)) {
      stop("moveValuesToColumnsN: keyColumns must be character")
    }
  }
  if(length(columnsToCopy)>0) {
    if(!is.character(columnsToCopy)) {
      stop("moveValuesToColumnsN: columnsToCopy must be character")
    }
  }
  if((!is.character(tallTable))||(length(tallTable)!=1)) {
    stop("moveValuesToColumnsN: tallTable must be the name of a remote table")
  }
  controlTable <- as.data.frame(controlTable)
  cCheck <- checkControlTable(controlTable, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::moveValuesToColumnsN", cCheck))
  }
  if(checkNames) {
    tallTableColnames <- listFields(my_db, tallTable)
    badCells <- setdiff(colnames(controlTable), tallTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::moveValuesToColumnsN: control table column names that are not tallTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }
  ctabName <- tempNameGenerator()
  DBI::dbWriteTable(my_db,
                    ctabName,
                    controlTable,
                    temporary = TRUE)
  resName <- tempNameGenerator()
  missingCaseTerm = "NULL"
  if(!is.null(defaultValue)) {
    if(is.numeric(defaultValue)) {
      missingCaseTerm <- as.character(defaultValue)
    } else {
      missingCaseTerm <- DBI::dbQuoteString(paste(as.character(defaultValue),
                                                  collapse = ' '))
    }
  }
  collectstmts <- vector(mode = 'list',
                         length = nrow(controlTable) * (ncol(controlTable)-1))
  collectN <- 1
  saw <- list()
  for(i in seq_len(nrow(controlTable))) {
    for(j in 2:ncol(controlTable)) {
      cij <- controlTable[i,j,drop=TRUE]
      if((!is.null(cij))&&(!is.na(cij))) {
        if(dropDups && (cij %in% names(saw))) {
          cij <- NA
        }
      }
      if((!is.null(cij))&&(!is.na(cij))) {
        collectstmts[[collectN]] <- paste0("MAX( CASE WHEN ", # pseudo aggregator
                                           "a.",
                                           DBI::dbQuoteIdentifier(my_db, colnames(controlTable)[[1]]),
                                           " = ",
                                           DBI::dbQuoteString(my_db, controlTable[i,1,drop=TRUE]),
                                           " THEN a.",
                                           DBI::dbQuoteIdentifier(my_db, colnames(controlTable)[[j]]),
                                           " ELSE ",
                                           missingCaseTerm,
                                           " END ) ",
                                           DBI::dbQuoteIdentifier(my_db, cij))
        saw[[cij]] <- TRUE
      }
      collectN <- collectN + 1
    }
  }
  # turn non-nulls into an array
  collectstmts <- as.character(Filter(function(x) { !is.null(x) },
                                      collectstmts))
  # pseudo-aggregators for columns we are copying
  # paste works on vectors in alligned fashion (not as a cross-product)
  copystmts <- NULL
  if(length(columnsToCopy)>0) {
    copystmts <- paste0('MAX(a.',
                        DBI::dbQuoteIdentifier(my_db, columnsToCopy),
                        ') ',
                        DBI::dbQuoteIdentifier(my_db, columnsToCopy))
  }
  groupterms <- NULL
  groupstmts <- NULL
  if(length(keyColumns)>0) {
    groupterms <- paste0('a.', DBI::dbQuoteIdentifier(my_db, keyColumns))
    groupstmts <- paste0('a.',
                         DBI::dbQuoteIdentifier(my_db, keyColumns),
                         ' ',
                         DBI::dbQuoteIdentifier(my_db, keyColumns))
  }
  # deliberate cross join
  qs <-  paste0(" SELECT ",
                paste(c(groupstmts, copystmts, collectstmts), collapse = ', '),
                ' FROM ',
                DBI::dbQuoteIdentifier(my_db, tallTable),
                ' a ')
  if(length(groupstmts)>0) {
    qs <- paste0(qs,
                 'GROUP BY ',
                 paste(groupterms, collapse = ', '))
  }
  q <-  paste0("CREATE TABLE ",
               DBI::dbQuoteIdentifier(my_db, resName),
               " AS ",
               qs)
  if(showQuery) {
    print(q)
  }
  tryCatch(
    # sparklyr didn't implement dbExecute(), so using dbGetQuery()
    DBI::dbGetQuery(my_db, q),
    warning = function(w) { NULL })
  resName
}


#' Map sets rows to columns (query based).
#'
#' Transform data facts from rows into additional columns using SQL
#' and controlTable.
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
#' \url{https://winvector.github.io/replyr/articles/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param tallTable data,frame containing data to be mapped (in-memory data.frame).
#' @param keyColumns character list of column defining row groups
#' @param controlTable table specifying mapping (local data frame)
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character list of column names to copy
#' @param strict logical, if TRUE check control table contents for uniqueness
#' @param checkNames logical, if TRUE check names
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param dropDups logical if TRUE supress duplicate columns (duplicate determined by name, not content).
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{{moveValuesToColumns}}, \code{\link{moveValuesToRowsN}}, \code{\link{buildPivotControlTableD}}
#'
#' @examples
#'
#' # pivot example
#' d <- data.frame(meas = c('AUC', 'R2'), val = c(0.6, 0.2))
#'
#' cT <- buildPivotControlTableD(d,
#'                               columnToTakeKeysFrom= 'meas',
#'                               columnToTakeValuesFrom= 'val')
#' moveValuesToColumnsD(d,
#'                      keyColumns = NULL,
#'                      controlTable = cT)
#'
#' @export
#'
moveValuesToColumnsD <- function(tallTable,
                                 keyColumns,
                                 controlTable,
                                 ...,
                                 columnsToCopy = NULL,
                                 strict = FALSE,
                                 checkNames = TRUE,
                                 showQuery = FALSE,
                                 defaultValue = NULL,
                                 dropDups = FALSE) {
  if(length(list(...))>0) {
    stop("cdata::moveValuesToColumnsD unexpected arguments.")
  }
  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(my_db,
                    'tallTable',
                    tallTable,
                    temporary = TRUE,
                    overwrite = TRUE)
  resName <- moveValuesToColumnsN(tallTable = 'tallTable',
                                  keyColumns = keyColumns,
                                  controlTable = controlTable,
                                  my_db = my_db,
                                  columnsToCopy = columnsToCopy,
                                  tempNameGenerator = makeTempNameGenerator('mvtcq'),
                                  strict = strict,
                                  checkNames = checkNames,
                                  showQuery = showQuery,
                                  defaultValue = defaultValue,
                                  dropDups = dropDups)
  resData <- DBI::dbGetQuery(my_db, paste("SELECT * FROM", resName))
  DBI::dbDisconnect(my_db)
  resData
}



