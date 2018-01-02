
#' buildUnPivotControlTable deprecated, functionality moved to: build_unpivot_control
#' @rdname build_unpivot_control
#' @export
buildUnPivotControlTable <- function(nameForNewKeyColumn,
                                     nameForNewValueColumn,
                                     columnsToTakeFrom,
                                     ...) {
  # .Deprecated(new = "build_unpivot_control",
  #             old = "buildUnPivotControlTable")
  build_unpivot_control(nameForNewKeyColumn,
                        nameForNewValueColumn,
                        columnsToTakeFrom,
                        ...)
}

#' moveValuesToRowsN deprecated, functionality moved to: rowrecs_to_blocks_q
#' @rdname rowrecs_to_blocks_q
#' @export
moveValuesToRowsN <- function(wideTable,
                              controlTable,
                              my_db,
                              ...,
                              columnsToCopy = NULL,
                              tempNameGenerator = makeTempNameGenerator('mvtrq'),
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL,
                              temporary = FALSE,
                              resultName = NULL) {
  # .Deprecated(new = "rowrecs_to_blocks_q",
  #             old = "moveValuesToRowsN")
  rowrecs_to_blocks_q(wideTable,
                      controlTable,
                      my_db,
                      ...,
                      columnsToCopy = columnsToCopy,
                      tempNameGenerator = tempNameGenerator,
                      strict = strict,
                      checkNames = checkNames,
                      showQuery = showQuery,
                      defaultValue = defaultValue,
                      temporary = temporary,
                      resultName = resultName)
}

#' moveValuesToRowsD deprecated, functionality moved to: rowrecs_to_blocks
#' @rdname rowrecs_to_blocks
#' @export
moveValuesToRowsD <- function(wideTable,
                              controlTable,
                              ...,
                              columnsToCopy = NULL,
                              strict = FALSE,
                              checkNames = TRUE,
                              showQuery = FALSE,
                              defaultValue = NULL,
                              env = parent.frame()) {
  # .Deprecated(new = "rowrecs_to_blocks",
  #             old = "moveValuesToRowsD")
  rowrecs_to_blocks(wideTable,
                    controlTable,
                    ...,
                    columnsToCopy = columnsToCopy,
                    strict = strict,
                    checkNames = checkNames,
                    showQuery = showQuery,
                    defaultValue = defaultValue,
                    env = env)
}

#' buildPivotControlTableN deprecated, functionality moved to: build_pivot_control_q
#' @rdname build_pivot_control_q
#' @export
buildPivotControlTableN <- function(tableName,
                                    columnToTakeKeysFrom,
                                    columnToTakeValuesFrom,
                                    my_db,
                                    ...,
                                    prefix = columnToTakeKeysFrom,
                                    sep = NULL) {
  # .Deprecated(new = "build_pivot_control_q",
  #             old = "buildPivotControlTableN")
  build_pivot_control_q(tableName,
                        columnToTakeKeysFrom,
                        columnToTakeValuesFrom,
                        my_db,
                        ...,
                        prefix = prefix,
                        sep = sep)
}

#' buildPivotControlTableD deprecated, functionality moved to: build_pivot_control
#' @rdname build_pivot_control
#' @export
buildPivotControlTableD <- function(table,
                                    columnToTakeKeysFrom,
                                    columnToTakeValuesFrom,
                                    ...,
                                    prefix = columnToTakeKeysFrom,
                                    sep = NULL,
                                    env = parent.frame()) {
  # .Deprecated(new = "build_pivot_control",
  #             old = "buildPivotControlTableD")
  build_pivot_control(table,
                      columnToTakeKeysFrom,
                      columnToTakeValuesFrom,
                      ...,
                      prefix = prefix,
                      sep = sep,
                      env = env)
}

#' moveValuesToColumnsN deprecated, functionality moved to: blocks_to_rowrecs_q
#' @rdname blocks_to_rowrecs_q
#' @export
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
                                 dropDups = FALSE,
                                 temporary = FALSE,
                                 resultName = NULL) {
  # .Deprecated(new = "blocks_to_rowrecs_q",
  #             old = "moveValuesToColumnsN")
  blocks_to_rowrecs_q(tallTable,
                      keyColumns,
                      controlTable,
                      my_db,
                      ...,
                      columnsToCopy = columnsToCopy,
                      tempNameGenerator = tempNameGenerator,
                      strict = strict,
                      checkNames = checkNames,
                      showQuery = showQuery,
                      defaultValue = defaultValue,
                      dropDups = dropDups,
                      temporary = temporary,
                      resultName = resultName)
}

#' moveValuesToColumnsD deprecated, functionality moved to: blocks_to_rowrecs
#' @rdname blocks_to_rowrecs
#' @export
moveValuesToColumnsD <-  function(tallTable,
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
  # .Deprecated(new = "blocks_to_rowrecs",
  #             old = "moveValuesToColumnsD")
  blocks_to_rowrecs(tallTable,
                    keyColumns,
                    controlTable,
                    ...,
                    columnsToCopy = columnsToCopy,
                    strict = strict,
                    checkNames = checkNames,
                    showQuery = showQuery,
                    defaultValue = defaultValue,
                    dropDups = dropDups,
                    env = env)
}

#' unpivotValuesToRows deprecated, functionality moved to: unpivot_to_blocks
#' @rdname unpivot_to_blocks
#' @export
unpivotValuesToRows <-  function(data,
                                 nameForNewKeyColumn,
                                 nameForNewValueColumn,
                                 columnsToTakeFrom,
                                 ...,
                                 nameForNewClassColumn = NULL,
                                 env = parent.frame()) {
  # .Deprecated(new = "unpivot_to_blocks",
  #             old = "unpivotValuesToRows")
  unpivot_to_blocks(data,
                    nameForNewKeyColumn,
                    nameForNewValueColumn,
                    columnsToTakeFrom,
                    ...,
                    nameForNewClassColumn = nameForNewClassColumn,
                    env = env)
}

#' pivotValuesToColumns deprecated, functionality moved to: pivot_to_rowrecs
#' @rdname pivot_to_rowrecs
#' @export
pivotValuesToColumns <- function(data,
                                 columnToTakeKeysFrom,
                                 columnToTakeValuesFrom,
                                 rowKeyColumns,
                                 ...,
                                 sep = NULL,
                                 env = parent.frame()) {
  # .Deprecated(new = "pivot_to_rowrecs",
  #             old = "pivotValuesToColumns")
  pivot_to_rowrecs(data,
                   columnToTakeKeysFrom,
                   columnToTakeValuesFrom,
                   rowKeyColumns,
                   ...,
                   sep = sep,
                   env = env)
}

