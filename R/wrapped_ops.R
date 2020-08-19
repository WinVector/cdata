

#' @export
#' @keywords internal
#'
build_pivot_control.wrapped_relop <-
  function(table,
           columnToTakeKeysFrom,
           columnToTakeValuesFrom,
           ...,
           prefix = columnToTakeKeysFrom,
           sep = NULL,
           tmp_name_source = wrapr::mk_tmp_name_source("bpc"),
           temporary = FALSE) {
    wrapr::stop_if_dot_args(substitute(list(...)),
                            "cdata::build_pivot_control.wrapped_relop")
    underlying = build_pivot_control(table,
                                     columnToTakeKeysFrom,
                                     columnToTakeValuesFrom,
                                     prefix = prefix,
                                     sep = sep,
                                     tmp_name_source = tmp_name_source,
                                     temporary = temporary)
    res <- list(underlying = underlying,
                data_map = source$data_map)
    class(res) <- 'wrapped_relop'
    return(res)
  }

#' @export
#' @keywords internal
#'
blocks_to_rowrecs.wrapped_relop <-
  function(tallTable,
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
    wrapr::stop_if_dot_args(substitute(list(...)),
                            "cdata::blocks_to_rowrecs.wrapped_relop")
    underlying = blocks_to_rowrecs(tallTable,
                                   keyColumns,
                                   controlTable,
                                   columnsToCopy = columnsToCopy,
                                   checkNames = checkNames,
                                   checkKeys = checkKeys,
                                   strict = strict,
                                   controlTableKeys = controlTableKeys,
                                   tmp_name_source = tmp_name_source,
                                   temporary = temporary,
                                   allow_rqdatatable = allow_rqdatatable)
    res <- list(underlying = underlying,
                data_map = source$data_map)
    class(res) <- 'wrapped_relop'
    return(res)
  }


#' @export
#' @keywords internal
#'
unpivot_to_blocks.wrapped_relop <-
  function(data,
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
    wrapr::stop_if_dot_args(substitute(list(...)),
                            "cdata::unpivot_to_blocks.wrapped_relop")
    underlying = unpivot_to_blocks(data,
                                   nameForNewKeyColumn,
                                   nameForNewValueColumn,
                                   columnsToTakeFrom,
                                   checkNames = checkNames,
                                   checkKeys = checkKeys,
                                   strict = strict,
                                   nameForNewClassColumn = nameForNewClassColumn,
                                   tmp_name_source = tmp_name_source,
                                   temporary = temporary,
                                   allow_rqdatatable = allow_rqdatatable)
    res <- list(underlying = underlying,
                data_map = source$data_map)
    class(res) <- 'wrapped_relop'
    return(res)
  }

# TODO: documenting tests on above.
