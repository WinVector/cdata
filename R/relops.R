
#' @param tmp_name_source a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param temporary logical, if TRUE use temporary tables
#' @examples
#'
#' d <- data.frame(measType = c("wt", "ht"),
#'                 measValue = c(150, 6),
#'                 stringsAsFactors = FALSE)
#'
#' ops <- rquery::local_td(d) %.>%
#'   build_pivot_control(.,
#'                       'measType', 'measValue',
#'                       sep = '_')
#' cat(format(ops))
#'
#' if(requireNamespace("rqdatatable", quietly = TRUE)) {
#'   library("rqdatatable")
#'   d %.>%
#'     ops %.>%
#'     print(.)
#' }
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(db,
#'                     'd',
#'                     d,
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   db %.>%
#'     ops %.>%
#'     print(.)
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#' @rdname build_pivot_control
build_pivot_control.relop <- function(table,
                                      columnToTakeKeysFrom,
                                      columnToTakeValuesFrom,
                                      ...,
                                      prefix = columnToTakeKeysFrom,
                                      sep = NULL,
                                      tmp_name_source = wrapr::mk_tmp_name_source("bpc"),
                                      temporary = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::build_pivot_control")
  if(!("relop" %in% class(table))) {
    stop("cdata::build_pivot_control.relop must be of class relop")
  }
  force(table)
  force(columnToTakeKeysFrom)
  force(columnToTakeValuesFrom)
  force(prefix)
  force(sep)
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  columns_produced <- c(columnToTakeKeysFrom, columnToTakeValuesFrom)
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    pct <- build_pivot_control_q(tableName = incoming_table_name,
                          columnToTakeKeysFrom = columnToTakeKeysFrom,
                          columnToTakeValuesFrom = columnToTakeValuesFrom,
                          my_db = db,
                          prefix = prefix,
                          sep = sep)
    rquery::rq_copy_to(db,
                       table_name = outgoing_table_name,
                       d = pct,
                       overwrite = TRUE,
                       temporary = temporary)
  }
  f_df <- function(d) {
    build_pivot_control.default(table = d,
                                columnToTakeKeysFrom = columnToTakeKeysFrom,
                                columnToTakeValuesFrom = columnToTakeValuesFrom,
                                prefix = prefix,
                                sep = sep)
  }
  nd <- rquery::non_sql_node(table,
                             f_db = f_db,
                             f_df = f_df,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = paste0("build_pivot_control(., columnToTakeKeysFrom=\"",
                                                   columnToTakeKeysFrom,
                                                   "\", columnToTakeValuesFrom=\"",
                                                   columnToTakeValuesFrom,
                                                   "\")"),
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}



#' @param tmp_name_source a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param temporary logical, if TRUE make result temporary.
#'
#' @examples
#'
#' d <- data.frame(AUC= 0.6, R2= 0.2)
#' ops <- rquery::local_td(d) %.>%
#'   unpivot_to_blocks(
#'     .,
#'     nameForNewKeyColumn= 'meas',
#'     nameForNewValueColumn= 'val',
#'     columnsToTakeFrom= c('AUC', 'R2'))
#' cat(format(ops))
#'
#' if(requireNamespace("rqdatatable", quietly = TRUE)) {
#'   library("rqdatatable")
#'   d %.>%
#'     ops %.>%
#'     print(.)
#' }
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(db,
#'                     'd',
#'                     d,
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   db %.>%
#'     ops %.>%
#'     print(.)
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#' @rdname unpivot_to_blocks
unpivot_to_blocks.relop <- function(data,
                                    nameForNewKeyColumn,
                                    nameForNewValueColumn,
                                    columnsToTakeFrom,
                                    ...,
                                    nameForNewClassColumn = NULL,
                                    tmp_name_source = wrapr::mk_tmp_name_source("upb"),
                                    temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::unpivot_to_blocks.relop")
  if(!("relop" %in% class(data))) {
    stop("cdata::unpivot_to_blocks.relop must be of class relop")
  }
  force(data)
  force(nameForNewKeyColumn)
  force(nameForNewValueColumn)
  force(columnsToTakeFrom)
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  cT <- build_unpivot_control(nameForNewKeyColumn = nameForNewKeyColumn,
                              nameForNewValueColumn = nameForNewValueColumn,
                              columnsToTakeFrom = columnsToTakeFrom)
  colsToCopy <- setdiff(colnames(data), columnsToTakeFrom)
  columns_produced <- c(colsToCopy, nameForNewKeyColumn, nameForNewValueColumn, nameForNewClassColumn)
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    rowrecs_to_blocks_q(wideTable = incoming_table_name,
                        controlTable = cT,
                        my_db = db,
                        ...,
                        columnsToCopy = colsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = FALSE,
                        checkNames = FALSE,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d) {
    unpivot_to_blocks.default(data = d,
                              nameForNewKeyColumn = nameForNewKeyColumn,
                              nameForNewValueColumn = nameForNewValueColumn,
                              columnsToTakeFrom = columnsToTakeFrom,
                              nameForNewClassColumn = nameForNewClassColumn)
  }
  nd <- rquery::non_sql_node(data,
                             f_db = f_db,
                             f_df = f_df,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = paste0("unpivot_to_blocks(., nameForNewKeyColumn=\"",
                                                   nameForNewKeyColumn,
                                                   " , nameForNewValueColumn=\"",
                                                   nameForNewValueColumn,
                                                   "\")"),
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}


