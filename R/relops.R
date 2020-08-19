
#' build_pivot_control.relop
#'
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
                   outgoing_table_name,
                   nd = NULL,
                   ...) {
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
  f_df <- function(d, nd = NULL) {
    build_pivot_control.default(table = d,
                                columnToTakeKeysFrom = columnToTakeKeysFrom,
                                columnToTakeValuesFrom = columnToTakeValuesFrom,
                                prefix = prefix,
                                sep = sep)
  }
  nd <- rquery::non_sql_node(table,
                             f_db = f_db,
                             f_df = f_df,
                             f_dt = NULL,
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


#' unpivot_to_blocks.relop
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
                                    checkNames = TRUE,
                                    checkKeys = FALSE,
                                    strict = FALSE,
                                    nameForNewClassColumn = NULL,
                                    tmp_name_source = wrapr::mk_tmp_name_source("upb"),
                                    temporary = TRUE,
                                    allow_rqdatatable = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::unpivot_to_blocks.relop")
  if(!("relop" %in% class(data))) {
    stop("cdata::unpivot_to_blocks.relop must be of class relop")
  }
  force(data)
  force(nameForNewKeyColumn)
  force(nameForNewValueColumn)
  force(columnsToTakeFrom)
  force(temporary)
  force(strict)
  force(checkNames)
  force(checkKeys)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  cT <- build_unpivot_control(nameForNewKeyColumn = nameForNewKeyColumn,
                              nameForNewValueColumn = nameForNewValueColumn,
                              columnsToTakeFrom = columnsToTakeFrom)
  colsToCopy <- setdiff(colnames(data), columnsToTakeFrom)
  columns_produced <- c(colsToCopy, nameForNewKeyColumn, nameForNewValueColumn, nameForNewClassColumn)
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name,
                   nd = NULL,
                   ...) {
    rowrecs_to_blocks_q(wideTable = incoming_table_name,
                        controlTable = cT,
                        my_db = db,
                        ...,
                        columnsToCopy = colsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = strict,
                        controlTableKeys = colnames(cT)[[1]],
                        checkNames = checkNames,
                        checkKeys = checkKeys,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d, nd = NULL) {
    unpivot_to_blocks.default(data = d,
                              nameForNewKeyColumn = nameForNewKeyColumn,
                              nameForNewValueColumn = nameForNewValueColumn,
                              columnsToTakeFrom = columnsToTakeFrom,
                              nameForNewClassColumn = nameForNewClassColumn)
  }
  nd <- rquery::non_sql_node(data,
                             f_db = f_db,
                             f_df = f_df,
                             f_dt = NULL,
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


#' blocks_to_rowrecs.relop
#'
#' @examples
#'
#' d <- data.frame(meas = c('AUC', 'R2'),
#'                 val = c(0.6, 0.2))
#' cT <- build_pivot_control(
#'   d,
#'   columnToTakeKeysFrom= 'meas',
#'   columnToTakeValuesFrom= 'val')
#'
#' ops <- rquery::local_td(d) %.>%
#'   blocks_to_rowrecs(.,
#'                     keyColumns = NULL,
#'                     controlTable = cT)
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
#' @rdname blocks_to_rowrecs
blocks_to_rowrecs.relop <- function(tallTable,
                                    keyColumns,
                                    controlTable,
                                    ...,
                                    columnsToCopy = NULL,
                                    checkNames = TRUE,
                                    checkKeys = FALSE,
                                    strict = FALSE,
                                    controlTableKeys = colnames(controlTable)[[1]],
                                    tmp_name_source = wrapr::mk_tmp_name_source("bltrr"),
                                    temporary = TRUE,
                                    allow_rqdatatable = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs")
  if(!("relop" %in% class(tallTable))) {
    stop("cdata::blocks_to_rowrecs.relop tallTable must be of class relop")
  }
  check_blocks_to_rowrecs_args(tallTable_columns = rquery::column_names(tallTable),
                               keyColumns = keyColumns,
                               controlTable = controlTable,
                               columnsToCopy = columnsToCopy,
                               checkNames = checkNames,
                               strict = strict,
                               controlTableKeys = controlTableKeys)
  force(tallTable)
  force(keyColumns)
  force(controlTable)
  force(columnsToCopy)
  force(checkNames)
  force(strict)
  force(checkNames)
  force(checkKeys)
  force(controlTableKeys)
  force(tmp_name_source)
  force(temporary)
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::blocks_to_rowrecs.relop", cCheck))
  }
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  controlTableValueColumns <- setdiff(colnames(controlTable), controlTableKeys)
  columns_produced <- c(keyColumns,
                        unique(as.character(unlist(controlTable[, controlTableValueColumns]))))
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name,
                   nd = NULL,
                   ...) {
    blocks_to_rowrecs_q(tallTable = incoming_table_name,
                        keyColumns = keyColumns,
                        controlTable = controlTable,
                        my_db = db,
                        columnsToCopy = columnsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = strict,
                        controlTableKeys = controlTableKeys,
                        checkNames = checkNames,
                        checkKeys = checkKeys,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        dropDups = TRUE,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d, nd = NULL) {
    blocks_to_rowrecs.default(tallTable = d,
                              keyColumns = keyColumns,
                              controlTable = controlTable,
                              columnsToCopy = columnsToCopy,
                              checkNames = checkNames,
                              strict = strict,
                              controlTableKeys = controlTableKeys)
  }
  df <- 'blocks_to_rowrecs(.)'
  df <- blocks_to_rowrecs_q(tallTable = "IN",
                            keyColumns = keyColumns,
                            controlTable = controlTable,
                            my_db = rquery::rquery_default_db_info(),
                            columnsToCopy = columnsToCopy,
                            tempNameGenerator = tmp_name_source,
                            strict = strict,
                            controlTableKeys = controlTableKeys,
                            checkNames = FALSE,
                            checkKeys = FALSE,
                            showQuery = FALSE,
                            defaultValue = NULL,
                            dropDups = TRUE,
                            temporary = temporary,
                            resultName = "OUT",
                            executeQuery = FALSE)
  nd <- rquery::non_sql_node(tallTable,
                             f_db = f_db,
                             f_df = f_df,
                             f_dt = NULL,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = df,
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}


#' rowrecs_to_blocks.relop
#'
#'
#' @examples
#'
#' d <- data.frame(AUC = 0.6, R2 = 0.2)
#' cT <- build_unpivot_control(
#'   nameForNewKeyColumn= 'meas',
#'   nameForNewValueColumn= 'val',
#'   columnsToTakeFrom= c('AUC', 'R2'))
#'
#' ops <- rquery::local_td(d) %.>%
#'   rowrecs_to_blocks(., cT)
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
#' @rdname rowrecs_to_blocks
rowrecs_to_blocks.relop <- function(wideTable,
                                    controlTable,
                                    ...,
                                    checkNames = TRUE,
                                    checkKeys = FALSE,
                                    strict = FALSE,
                                    controlTableKeys = colnames(controlTable)[[1]],
                                    columnsToCopy = NULL,
                                    tmp_name_source = wrapr::mk_tmp_name_source("rrtbl"),
                                    temporary = TRUE,
                                    allow_rqdatatable = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks")
  if(!("relop" %in% class(wideTable))) {
    stop("cdata::rowrecs_to_blocks.relop wideTable should be of class relop")
  }
  check_rowrecs_to_blocks_args(wideTable_columns = rquery::column_names(wideTable),
                               controlTable = controlTable,
                               checkNames = checkNames,
                               strict = strict,
                               controlTableKeys = controlTableKeys,
                               columnsToCopy = columnsToCopy)
  force(wideTable)
  force(controlTable)
  force(columnsToCopy)
  force(checkNames)
  force(strict)
  force(checkNames)
  force(checkKeys)
  force(controlTableKeys)
  force(tmp_name_source)
  force(temporary)
  cCheck <- checkControlTable(controlTable, controlTableKeys, strict)
  if(!is.null(cCheck)) {
    stop(paste("cdata::rowrecs_to_blocks.relop", cCheck))
  }
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  columns_produced <- c(columnsToCopy, colnames(controlTable))
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name,
                   nd = NULL,
                   ...) {
    rowrecs_to_blocks_q(wideTable = incoming_table_name,
                        controlTable = controlTable,
                        my_db = db,
                        columnsToCopy = columnsToCopy,
                        tempNameGenerator = tmp_name_source,
                        strict = strict,
                        controlTableKeys = controlTableKeys,
                        checkNames = checkNames,
                        checkKeys = checkKeys,
                        showQuery = FALSE,
                        defaultValue = NULL,
                        temporary = temporary,
                        resultName = outgoing_table_name)
  }
  f_df <- function(d, nd = NULL) {
    rowrecs_to_blocks.default(wideTable = d,
                              controlTable = controlTable,
                              checkNames = checkNames,
                              strict = strict,
                              controlTableKeys = controlTableKeys,
                              columnsToCopy = columnsToCopy)
  }
  df <- "rowrecs_to_blocks(.)"
  df <- rowrecs_to_blocks_q(wideTable = "IN",
                            controlTable = controlTable,
                            my_db = rquery::rquery_default_db_info(),
                            columnsToCopy = columnsToCopy,
                            tempNameGenerator = tmp_name_source,
                            strict = strict,
                            controlTableKeys = controlTableKeys,
                            checkNames = FALSE,
                            checkKeys = FALSE,
                            showQuery = FALSE,
                            defaultValue = NULL,
                            temporary = temporary,
                            resultName = "OUT",
                            executeQuery = FALSE)
  nd <- rquery::non_sql_node(wideTable,
                             f_db = f_db,
                             f_df = f_df,
                             f_dt = NULL,
                             incoming_table_name = incoming_table_name,
                             outgoing_table_name = outgoing_table_name,
                             columns_produced = columns_produced,
                             display_form = df,
                             orig_columns = FALSE,
                             temporary = temporary)
  nd
}
