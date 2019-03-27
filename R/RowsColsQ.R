
# Contributed by John Mount jmount@win-vector.com , ownership assigned to Win-Vector LLC.
# Win-Vector LLC currently distributes this code without intellectual property indemnification, warranty, claim of fitness of purpose, or any other guarantee under a GPL3 license.


# Core functionality on databases.








#' List columns of a table
#'
#' @param my_db database connection
#' @param tableName character name of table
#' @return list of column names
#'
#' @examples
#'
#' if (  requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   rquery::rq_copy_to(my_db,
#'                     'd',
#'                     data.frame(AUC = 0.6, R2 = 0.2, nope = -5),
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   cols(my_db, 'd')
#'   cT <- build_unpivot_control(
#'     nameForNewKeyColumn= 'meas',
#'     nameForNewValueColumn= 'val',
#'     columnsToTakeFrom= setdiff(cols(my_db, 'd'), "nope"))
#'   print(cT)
#'   tab <- rowrecs_to_blocks_q('d', cT, my_db = my_db)
#'   qlook(my_db, tab)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @noRd
#' @keywords internal
#'
cols <- function(my_db, tableName) {
  rquery::rq_colnames(my_db, tableName)
}

#' Quick look at remote data
#'
#' @param my_db database handle
#' @param tableName name of table to look at
#' @param displayRows number of rows to sample
#' @param countRows logical, if TRUE return row count.
#' @return str view of data
#'
#' @examples
#'
#' if ( requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   rquery::rq_copy_to(my_db,
#'                     'd',
#'                     data.frame(AUC = 0.6, R2 = 0.2),
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   qlook(my_db, 'd')
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#' @keywords internal
#'
qlook <- function(my_db, tableName,
                  displayRows = 10,
                  countRows = TRUE) {
  h <- rquery::rq_get_query(my_db,
                       paste0("SELECT * FROM ",
                              rquery::quote_identifier(my_db, tableName),
                              " LIMIT ", displayRows))
  cat(paste('table',
            rquery::quote_identifier(my_db, tableName),
            paste(class(my_db), collapse = ' '),
            '\n'))
  if(countRows) {
    nrow <- rquery::rq_get_query(my_db,
                            paste0("SELECT COUNT(1) FROM ",
                                   rquery::quote_identifier(my_db, tableName)))[1,1, drop=TRUE]
    nrow <- as.numeric(nrow) # defend against Rpostgres integer64
    cat(paste(" nrow:", nrow, '\n'))
    if(nrow>displayRows) {
      cat(" NOTE: \"obs\" below is count of sample, not number of rows of data.\n")
    }
  } else {
    cat(" NOTE: \"obs\" below is count of sample, not number of rows of data.\n")
  }
  utils::str(h, list.len = length(h))
  invisible(NULL)
}




#' Map a set of columns to rows (query based, take name of table).
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
#' by running an appropriate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param wideTable name of table containing data to be mapped (db/Spark data)
#' @param controlTable table specifying mapping (local data frame)
#' @param my_db db handle
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character array of column names to copy
#' @param tempNameGenerator a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param strict logical, if TRUE check control table name forms
#' @param controlTableKeys character, which column names of the control table are considered to be keys.
#' @param checkNames logical, if TRUE check names
#' @param checkKeys logical, if TRUE check wideTable keys
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param temporary logical, if TRUE make result temporary.
#' @param resultName character, name for result table.
#' @return long table built by mapping wideTable to one row per group
#'
#' @seealso \code{\link{build_unpivot_control}},  \code{\link{rowrecs_to_blocks}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   # un-pivot example
#'   d <- data.frame(AUC = 0.6, R2 = 0.2)
#'   rquery::rq_copy_to(my_db,
#'                     'd',
#'                     d,
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   cT <- build_unpivot_control(nameForNewKeyColumn= 'meas',
#'                               nameForNewValueColumn= 'val',
#'                               columnsToTakeFrom= c('AUC', 'R2'))
#'   tab <- rowrecs_to_blocks_q('d', cT, my_db = my_db)
#'   qlook(my_db, tab)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#' @keywords internal
#'
rowrecs_to_blocks_q <- function(wideTable,
                                controlTable,
                                my_db,
                                ...,
                                columnsToCopy = NULL,
                                tempNameGenerator = mk_tmp_name_source('mvtrq'),
                                strict = FALSE,
                                controlTableKeys = colnames(controlTable)[[1]],
                                checkNames = TRUE,
                                checkKeys = FALSE,
                                showQuery = FALSE,
                                defaultValue = NULL,
                                temporary = FALSE,
                                resultName = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks_q")
  if(length(columnsToCopy)>0) {
    if(!is.character(columnsToCopy)) {
      stop("rowrecs_to_blocks_q: columnsToCopy must be character")
    }
  }
  if((!is.character(wideTable))||(length(wideTable)!=1)) {
    stop("rowrecs_to_blocks_q: wideTable must be the name of a remote table")
  }
  controlTable <- as.data.frame(controlTable)
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::rowrecs_to_blocks_q", cCheck))
  }
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  if(checkNames) {
    interiorCells <- unlist(controlTable[, controlTableValueColumns], use.names = FALSE)
    interiorCells <- interiorCells[!is.na(interiorCells)]
    wideTableColnames <- cols(my_db, wideTable)
    badCells <- setdiff(interiorCells, wideTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::rowrecs_to_blocks_q: control table entries that are not wideTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }

  # check more
  if(checkKeys) {
    if(!rows_are_uniquely_keyed(wideTable, columnsToCopy, my_db)) {
      stop("cdata::rowrecs_to_blocks_q columnsToCopy do not uniquely key the rows")
    }
  }

  ctabName <- tempNameGenerator()
  rownames(controlTable) <- NULL # just in case
  rquery::rq_copy_to(my_db,
                      ctabName,
                      controlTable)
  if(is.null(resultName)) {
    resName <- tempNameGenerator()
  } else {
    resName = resultName
  }
  missingCaseTerm = "NULL"
  if(!is.null(defaultValue)) {
    if(is.numeric(defaultValue)) {
      missingCaseTerm <- as.character(defaultValue)
    } else {
      missingCaseTerm <- rquery::quote_string(paste(as.character(defaultValue),
                                                  collapse = ' '))
    }
  }
  control_key_indices <- match(controlTableKeys, colnames(controlTable))
  casestmts <- lapply(controlTableValueColumns,
                      function(cj) {
                        whens <- lapply(seq_len(nrow(controlTable)),
                                        function(i) {
                                          cij <- controlTable[i,cj,drop=TRUE]
                                          if(is.null(cij) || is.na(cij)) {
                                            return(NULL)
                                          }
                                          match_stmts <- vapply(
                                            control_key_indices,
                                            function(j2) {
                                              paste0('b.',
                                                     rquery::quote_identifier(my_db, colnames(controlTable)[j2]),
                                                     ' = ',
                                                     rquery::quote_string(my_db, controlTable[i,j2,drop=TRUE]))
                                            }, character(1))
                                          paste0(' WHEN ',
                                                 paste(match_stmts, collapse = " AND "),
                                                 ' THEN a.',
                                                 rquery::quote_identifier(my_db, cij))
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
                                           rquery::quote_identifier(my_db, cj))
                      })
  casestmts <- as.character(Filter(function(x) { !is.null(x) },
                                   casestmts))
  copystmts <- NULL
  if(length(columnsToCopy)>0) {
    copystmts <- paste0('a.', rquery::quote_identifier(my_db, columnsToCopy))
  }
  groupstmts <- paste0('b.', rquery::quote_identifier(my_db, controlTableKeys))
  # deliberate cross join
  qs <-  paste0(" SELECT ",
                paste(c(copystmts, groupstmts, casestmts), collapse = ', '),
                ' FROM ',
                rquery::quote_identifier(my_db, wideTable),
                ' a CROSS JOIN ',
                rquery::quote_identifier(my_db, ctabName),
                ' b ')
  q <-  paste0("CREATE ",
               ifelse(temporary, "TEMPORARY", ""),
               " TABLE ",
               rquery::quote_identifier(my_db, resName),
               " AS ",
               qs)
  if(showQuery) {
    print(q)
  }
  rquery::rq_execute(my_db, q)
  rquery::rq_remove_table(my_db, ctabName)
  resName
}





#' Build a blocks_to_rowrecs_q() control table that specifies a pivot (query based, takes name of table).
#'
#' Some discussion and examples can be found here: \url{https://winvector.github.io/FluidData/FluidData.html}.
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
#' @seealso \code{\link{blocks_to_rowrecs_q}}, \code{\link{build_pivot_control}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- data.frame(measType = c("wt", "ht"),
#'                   measValue = c(150, 6),
#'                   stringsAsFactors = FALSE)
#'   rquery::rq_copy_to(my_db,
#'                     'd',
#'                     d,
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   build_pivot_control_q('d', 'measType', 'measValue',
#'                         my_db = my_db,
#'                         sep = '_') %.>%
#'      print(.)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#' @keywords internal
#'
build_pivot_control_q <- function(tableName,
                                  columnToTakeKeysFrom,
                                  columnToTakeValuesFrom,
                                  my_db,
                                  ...,
                                  prefix = columnToTakeKeysFrom,
                                  sep = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::build_pivot_control_q")
  q <- paste0("SELECT ",
              rquery::quote_identifier(my_db, columnToTakeKeysFrom),
              " FROM ",
              rquery::quote_identifier(my_db, tableName),
              " GROUP BY ",
              rquery::quote_identifier(my_db, columnToTakeKeysFrom))
  controlTable <- rquery::rq_get_query(my_db, q)
  controlTable[[columnToTakeKeysFrom]] <- as.character(controlTable[[columnToTakeKeysFrom]])
  controlTable[[columnToTakeValuesFrom]] <- controlTable[[columnToTakeKeysFrom]]
  if(!is.null(sep)) {
    controlTable[[columnToTakeValuesFrom]] <- paste(prefix,
                                                    controlTable[[columnToTakeValuesFrom]],
                                                    sep=sep)
  }
  controlTable
}





#' Map sets rows to columns (query based, take name of table).
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
#' by running an appropriate query over the data.
#'
#' Some discussion and examples can be found here:
#' \url{https://winvector.github.io/FluidData/FluidData.html} and
#' here \url{https://github.com/WinVector/cdata}.
#'
#' @param tallTable name of table containing data to be mapped (db/Spark data)
#' @param keyColumns character list of column defining row groups
#' @param controlTable table specifying mapping (local data frame)
#' @param my_db db handle
#' @param ... force later arguments to be by name.
#' @param columnsToCopy character list of column names to copy
#' @param tempNameGenerator a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param strict logical, if TRUE check control table name forms
#' @param controlTableKeys character, which column names of the control table are considered to be keys.
#' @param checkNames logical, if TRUE check names
#' @param checkKeys logical, if TRUE check keying of tallTable
#' @param showQuery if TRUE print query
#' @param defaultValue if not NULL literal to use for non-match values.
#' @param dropDups logical if TRUE suppress duplicate columns (duplicate determined by name, not content).
#' @param temporary logical, if TRUE make result temporary.
#' @param resultName character, name for result table.
#' @return wide table built by mapping key-grouped tallTable rows to one row per group
#'
#' @seealso \code{\link{build_pivot_control_q}}, \code{\link{blocks_to_rowrecs}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   # pivot example
#'   d <- data.frame(meas = c('AUC', 'R2'), val = c(0.6, 0.2))
#'   rquery::rq_copy_to(my_db,
#'                     'd',
#'                     d,
#'                     temporary = TRUE)
#'   cT <- build_pivot_control_q('d',
#'                               columnToTakeKeysFrom= 'meas',
#'                               columnToTakeValuesFrom= 'val',
#'                               my_db = my_db)
#'   tab <- blocks_to_rowrecs_q('d',
#'                              keyColumns = NULL,
#'                              controlTable = cT,
#'                              my_db = my_db)
#'   qlook(my_db, tab)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#' @keywords internal
#'
blocks_to_rowrecs_q <- function(tallTable,
                                keyColumns,
                                controlTable,
                                my_db,
                                ...,
                                columnsToCopy = NULL,
                                tempNameGenerator = mk_tmp_name_source('mvtcq'),
                                strict = FALSE,
                                controlTableKeys = colnames(controlTable)[[1]],
                                checkNames = TRUE,
                                checkKeys = FALSE,
                                showQuery = FALSE,
                                defaultValue = NULL,
                                dropDups = FALSE,
                                temporary = FALSE,
                                resultName = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs_q")
  if(length(keyColumns)>0) {
    if(!is.character(keyColumns)) {
      stop("blocks_to_rowrecs_q: keyColumns must be character")
    }
  }
  if(length(columnsToCopy)>0) {
    if(!is.character(columnsToCopy)) {
      stop("blocks_to_rowrecs_q: columnsToCopy must be character")
    }
  }
  if((!is.character(tallTable))||(length(tallTable)!=1)) {
    stop("blocks_to_rowrecs_q: tallTable must be the name of a remote table")
  }
  controlTable <- as.data.frame(controlTable)
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::blocks_to_rowrecs_q", cCheck))
  }
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  if(checkNames) {
    tallTableColnames <- cols(my_db, tallTable)
    badCells <- setdiff(colnames(controlTable), tallTableColnames)
    if(length(badCells)>0) {
      stop(paste("cdata::blocks_to_rowrecs_q: control table column names that are not tallTable column names:",
                 paste(badCells, collapse = ', ')))
    }
  }

  # check more
  if(checkKeys) {
    # TODO: consider these checks
    # # only values expected as controlTable keys should be in tallTable[[controlTableKeys]]
    # bkeys <- unique(unlist(controlTable[, controlTableKeys], use.names = FALSE))
    # bseen <- unique(unlist(tallTable[, controlTableKeys], use.names = FALSE))
    # bnovel <- setdiff(bseen, bkeys)
    # if(length(bnovel)>0) {
    #   stop(paste("cdata::blocks_to_rowrecs_q: table values that are not block keys:",
    #              paste(bnovel, collapse = ', ')))
    # }
    # check keyColumns plus controltable keys key data
    if(!rows_are_uniquely_keyed(tallTable, c(controlTableKeys, keyColumns), my_db)) {
      stop(paste("cdata::blocks_to_rowrecs_q: controlTableKeys plus keyColumns do not unique index data"))
    }
  }

  ctabName <- tempNameGenerator()
  rownames(controlTable) <- NULL # just in case
  rquery::rq_copy_to(my_db,
                      ctabName,
                      controlTable)
  if(is.null(resultName)) {
    resName <- tempNameGenerator()
  } else {
    resName = resultName
  }
  missingCaseTerm = "NULL"
  if(!is.null(defaultValue)) {
    if(is.numeric(defaultValue)) {
      missingCaseTerm <- as.character(defaultValue)
    } else {
      missingCaseTerm <- rquery::quote_string(paste(as.character(defaultValue),
                                                  collapse = ' '))
    }
  }
  control_key_indices <- match(controlTableKeys, colnames(controlTable))
  collectstmts <- vector(mode = 'list',
                         length = nrow(controlTable) * (ncol(controlTable)-1))
  collectN <- 1
  saw <- list()
  for(i in seq_len(nrow(controlTable))) {
    for(cj in controlTableValueColumns) {
      cij <- controlTable[i,cj,drop=TRUE]
      if((!is.null(cij))&&(!is.na(cij))) {
        if(dropDups && (cij %in% names(saw))) {
          cij <- NA
        }
      }
      if((!is.null(cij))&&(!is.na(cij))) {
        match_stmts <- vapply(
          control_key_indices,
          function(j2) {
            paste0('a.',
                   rquery::quote_identifier(my_db, colnames(controlTable)[j2]),
                   ' = ',
                   rquery::quote_string(my_db, controlTable[i,j2,drop=TRUE]))
          }, character(1))
        collectstmts[[collectN]] <- paste0("MAX( CASE WHEN ", # pseudo aggregator
                                           paste(match_stmts, collapse = " AND "),
                                           " THEN a.",
                                           rquery::quote_identifier(my_db, cj),
                                           " ELSE ",
                                           missingCaseTerm,
                                           " END ) ",
                                           rquery::quote_identifier(my_db, cij))
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
                        rquery::quote_identifier(my_db, columnsToCopy),
                        ') ',
                        rquery::quote_identifier(my_db, columnsToCopy))
  }
  groupterms <- NULL
  groupstmts <- NULL
  if(length(keyColumns)>0) {
    groupterms <- paste0('a.', rquery::quote_identifier(my_db, keyColumns))
    groupstmts <- paste0('a.',
                         rquery::quote_identifier(my_db, keyColumns),
                         ' ',
                         rquery::quote_identifier(my_db, keyColumns))
  }
  # deliberate cross join
  qs <-  paste0(" SELECT ",
                paste(c(groupstmts, copystmts, collectstmts), collapse = ', '),
                ' FROM ',
                rquery::quote_identifier(my_db, tallTable),
                ' a ')
  if(length(groupstmts)>0) {
    qs <- paste0(qs,
                 'GROUP BY ',
                 paste(groupterms, collapse = ', '))
  }
  q <-  paste0("CREATE ",
               ifelse(temporary, "TEMPORARY", ""),
               " TABLE ",
               rquery::quote_identifier(my_db, resName),
               " AS ",
               qs)
  if(showQuery) {
    print(q)
  }
  rquery::rq_execute(my_db, q)
  rquery::rq_remove_table(my_db, ctabName)
  resName
}


