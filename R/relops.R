
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


#' @param tmp_name_source a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param temporary logical, if TRUE make result temporary.
#'
#' @export
#' @rdname blocks_to_rowrecs
blocks_to_rowrecs.relop <- function(tallTable,
                                    keyColumns,
                                    controlTable,
                                    ...,
                                    columnsToCopy = NULL,
                                    checkNames = TRUE,
                                    strict = FALSE,
                                    use_data_table = TRUE,
                                    tmp_name_source = wrapr::mk_tmp_name_source("bltrr"),
                                    temporary = TRUE) {
  stop("Not implmented yet.") # TODO: remove this stop().
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs")
  if(!("relop" %in% class(tallTable))) {
    stop("cdata::blocks_to_rowrecs.relop tallTable must be of class relop")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::blocks_to_rowrecs controlTable should be a data.frame")
  }
  force(tallTable)
  force(keyColumns)
  force(controlTable)
  force(columnsToCopy)
  force(checkNames)
  force(use_data_table)
  force(strict)
  force(tmp_name_source)
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  columns_produced <- c(keyColumns,
                        as.character(unlist(controlTable[, -1, drop=FALSE]))) # TODO: work this out.
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    blocks_to_rowrecs_q(tallTable = incoming_table_name,
                        keyColumns = keyColumns,
                        controlTable = controlTable,
                        my_db = db,
                        columnsToCopy = columnsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = strict,
                        checkNames = checkNames,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        dropDups = FALSE,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d) {
    blocks_to_rowrecs.default(tallTable = d,
                              keyColumns = keyColumns,
                              controlTable = controlTable,
                              columnsToCopy = columnsToCopy,
                              checkNames = checkNames,
                              strict = strict,
                              use_data_table = use_data_table)
  }
  nd <- rquery::non_sql_node(tallTable,
                             f_db = f_db,
                             f_df = f_df,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = paste0("blocks_to_rowrecs(.)"),
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}


#' @param tmp_name_source a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param temporary logical, if TRUE make result temporary.
#'
#' @export
#' @rdname rowrecs_to_blocks
rowrecs_to_blocks.relop <- function(wideTable,
                                    controlTable,
                                    ...,
                                    checkNames = TRUE,
                                    strict = FALSE,
                                    columnsToCopy = NULL,
                                    use_data_table = TRUE,
                                    tmp_name_source = wrapr::mk_tmp_name_source("rrtbl"),
                                    temporary = TRUE) {
  stop("Not implmented yet.") # TODO: remove this stop().
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!("relop" %in% class(wideTable))) {
    stop("cdata::rowrecs_to_blocks.relop wideTable should be of class relop")
  }
  if(!is.data.frame(controlTable)) {
    stop("cdata::rowrecs_to_blocks controlTable should be a data.frame")
  }
  force(wideTable)
  force(controlTable)
  force(columnsToCopy)
  force(checkNames)
  force(use_data_table)
  force(strict)
  force(tmp_name_source)
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  columns_produced <- c(columnsToCopy, colnames(controlTable)) # TODO: work this out.
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    rowrecs_to_blocks_q(wideTable = incoming_table_name,
                        controlTable = controlTable,
                        my_db = db,
                        columnsToCopy = columnsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = strict,
                        checkNames = checkNames,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d) {
    rowrecs_to_blocks.default(wideTable = d,
                              controlTable = controlTable,
                              checkNames = checkNames,
                              strict = strict,
                              columnsToCopy = columnsToCopy,
                              use_data_table = use_data_table)
  }
  nd <- rquery::non_sql_node(wideTable,
                             f_db = f_db,
                             f_df = f_df,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = paste0("rowrecs_to_blocks(.)"),
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}
