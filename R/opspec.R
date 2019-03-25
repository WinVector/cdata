

#' @importFrom methods setGeneric
NULL

#' Create a record specification.
#'
#' Create a record specification object that holds the pivot control table, spefication of
#' extra row keys, and control table keys.
#'
#' @param controlTable an all character data frame or cdata pivot control.
#' @param ... not used, force later arguments to bind by name.
#' @param recordKeys vector of columns identifying records.
#' @param controlTableKeys vector of keying columns of the controlTable.
#' @return a record specification object
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id", "measure", "value" |
#'   1   , "AUC"    , 0.7     |
#'   1   , "R2"     , 0.4     |
#'   2   , "AUC"    , 0.8     |
#'   2   , "R2"     , 0.5     )
#'
#' record_spec <- new_record_spec(
#'   wrapr::qchar_frame(
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
#'   recordKeys = "id")
#'
#' (d2 <- record_spec %pivot% d)
#'
#' # identity
#' ( record_spec %pivot% d ) %pivot% record_spec
#'
#' # identity
#' record_spec %pivot% d %pivot% record_spec
#'
#' # identity
#' record_spec %pivot% ( d2 %pivot% record_spec )
#'
#' @export
#'
new_record_spec <- function(controlTable,
                           ...,
                           recordKeys = character(0),
                           controlTableKeys = colnames(controlTable)[[1]]) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::new_record_spec")
  ck <- checkControlTable(controlTable = controlTable, controlTableKeys = controlTableKeys, strict = FALSE)
  if(!is.null(ck)) {
    stop(paste("cdata::new_record_spec", ck))
  }
  r <- list(controlTable = controlTable,
            recordKeys = recordKeys,
            controlTableKeys = controlTableKeys)
  class(r) <- "cdata_record_spec"
  r
}

#' Multiply/join row records into block records.
#'
#' Call \code{rowrecs_to_blocks()}.
#'
#' @param table data (data.frame or relop).
#' @param record_spec a cdata_record_spec.
#' @return rowrecs_to_blocks() result.
#'
#' @examples
#'
#' d <- z <- wrapr::build_frame(
#'   "id", "AUC", "R2" |
#'   1   , 0.7  , 0.4  |
#'   2   , 0.8  , 0.5  )
#'
#' record_spec <- new_record_spec(
#'   wrapr::qchar_frame(
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
#'   recordKeys = "id")
#'
#' d %**% record_spec
#'
#' @export
#'
`%**%` <- function(table, record_spec) {
  rowrecs_to_blocks(wideTable = table,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys,
                    columnsToCopy = record_spec$recordKeys)
}

#' Aggregate/project block records into row records.
#'
#' Call \code{blocks_to_rowrecs()}.
#'
#' @param table data (data.frame or relop).
#' @param record_spec a cdata_record_spec.
#' @return blocks_to_rowrecs() result.
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id", "measure", "value" |
#'   1   , "AUC"    , 0.7     |
#'   1   , "R2"     , 0.4     |
#'   2   , "AUC"    , 0.8     |
#'   2   , "R2"     , 0.5     )
#'
#' record_spec <- new_record_spec(
#'   wrapr::qchar_frame(
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
#'   recordKeys = "id")
#'
#' d %//% record_spec
#'
#' @export
#'
`%//%` <- function(table, record_spec) {
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = record_spec$recordKeys,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys)
}

#' Convert data shape.
#'
#' If \code{b} is a \code{cdata_record_spec} then call \code{blocks_to_rowrecs()} (multiply or join out to the record spec).
#' If \code{a} is a \code{cdata_record_spec} then call \code{rowrecs_to_blocks()} (aggregate or project from the record spec).
#'
#'
#' @param a data (data.frame or relop) or cdata_record_spec.
#' @param b data (data.frame or relop) or cdata_record_spec.
#' @return blocks_to_rowrecs() result.
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id", "measure", "value" |
#'   1   , "AUC"    , 0.7     |
#'   1   , "R2"     , 0.4     |
#'   2   , "AUC"    , 0.8     |
#'   2   , "R2"     , 0.5     )
#'
#' record_spec <- new_record_spec(
#'   wrapr::qchar_frame(
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
#'   recordKeys = "id")
#'
#' (d2 <- record_spec %pivot% d)
#'
#' # identity
#' ( record_spec %pivot% d ) %pivot% record_spec
#'
#' # identity
#' record_spec %pivot% d %pivot% record_spec
#'
#' # identity
#' record_spec %pivot% ( d2 %pivot% record_spec )
#'
#' @export
#'
`%pivot%` <- function(a, b) {
  if("cdata_record_spec" %in% class(a)) {
    record_spec <- a
    table <- b
    return(blocks_to_rowrecs(tallTable = table,
                             keyColumns = record_spec$recordKeys,
                             controlTable = record_spec$controlTable,
                             controlTableKeys = record_spec$controlTableKeys))
  }
  if("cdata_record_spec" %in% class(b)) {
    record_spec <- b
    table <- a
    return(rowrecs_to_blocks(wideTable = table,
                             controlTable = record_spec$controlTable,
                             controlTableKeys = record_spec$controlTableKeys,
                             columnsToCopy = record_spec$recordKeys))
  }
  stop("cdata::`%pivot%`: one argument must be of class cdata_record_spec")
}
