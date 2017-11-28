

#' Bind rows by a query. Assumes all tables structured identically.
#'
#' @param tableNames names of tables to concatinate (not empty)
#' @param colNames names of columns
#' @param my_db connection to where tables live
#' @param resultTableName name of result table
#' @param ... force later arguments to bind by name
#' @param origTableColumn character, column to put original table name in.
#' @return resultTableName
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(my_db,
#'                   'd1',
#'                    data.frame(x=1:2, y= 10:11),
#'                    overwrite = TRUE,
#'                    temporary = TRUE)
#' DBI::dbWriteTable(my_db,
#'                   'd2',
#'                    data.frame(x=3:4, y= 13:14),
#'                    overwrite = TRUE,
#'                    temporary = TRUE)
#' bind_rowsQ(tableNames = c('d1', 'd2'),
#'            colNames = c('x', 'y'),
#'            my_db = my_db,
#'            resultTableName = 'res1',
#'            origTableColumn = 'orig_table')
#' cdata::qlook(my_db, 'res1')
#'
#' @export
#'
bind_rowsQ <- function(tableNames, colNames, my_db,
                       resultTableName,
                       ...,
                       origTableColumn = NULL) {
  if(length(list(...))>0) {
    stop("replyr::bind_rowsQ unexpected arguments")
  }
  qSel <- vapply(colNames,
                 function(ci) {
                   DBI::dbQuoteIdentifier(my_db, ci)
                 }, character(1))
  selTerms <- paste(qSel, collapse = ", ")
  if(!is.null(origTableColumn)) {
    selTerms <- paste0(selTerms,
                       ", ",
                       DBI::dbQuoteString(my_db, tableNames[[1]]),
                       " ",
                       DBI::dbQuoteIdentifier(my_db, origTableColumn))
  }
  qd <- paste0("DROP TABLE IF EXISTS ",
               DBI::dbQuoteIdentifier(my_db, resultTableName))
  tryCatch(
    r <- DBI::dbGetQuery(my_db, qd),
    warning = function(w) { NULL })
  qc <- paste0("CREATE TABLE ",
               DBI::dbQuoteIdentifier(my_db, resultTableName),
               " AS SELECT ",
               selTerms,
               " FROM ",
               tableNames[[1]])
  tryCatch(
    r <- DBI::dbGetQuery(my_db, qc),
    warning = function(w) { NULL })
  if(length(tableNames)>1) {
    for(i in 2:length(tableNames)) {
      selTerms <- paste(qSel, collapse = ", ")
      if(!is.null(origTableColumn)) {
        selTerms <- paste0(selTerms,
                           ", ",
                           DBI::dbQuoteString(my_db, tableNames[[i]]),
                           " ",
                           DBI::dbQuoteIdentifier(my_db, origTableColumn))
      }
      qi <- paste0("INSERT INTO ",
                   DBI::dbQuoteIdentifier(my_db,  resultTableName),
                   " SELECT ",
                   selTerms,
                   " FROM ",
                   tableNames[[i]])
      tryCatch(
        r <- DBI::dbGetQuery(my_db, qi),
        warning = function(w) { NULL })
    }
  }
  resultTableName
}

