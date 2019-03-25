

#' @importFrom methods setGeneric
NULL

#' Create a record specification.
#'
#' Create a record specification object that holds the pivot control table, spefication of
#' extra row keys, and control table keys.
#'
#' @param controlTable an all character data frame or cdata pivot control.
#' @param recordKeys vector of columns identifying records.
#' @param controlTableKeys vector of keying columns of the controlTable.
#' @return a record specification object
#'
#' @export
#'
new_record_spec <- function(controlTable,
                           ...,
                           recordKeys = character(0),
                           controlTableKeys = colnames(controlTable)[[1]]) {
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

#' Multiply row records into block records.
#'
#' Call \code{rowrecs_to_blocks()}.
#'
#' @param table data (data.frame or relop).
#' @param record_spec a cdata_record_spec.
#' @return rowrecs_to_blocks() result.
#'
#' @export
#'
`%**%` <- function(table, record_spec) {
  rowrecs_to_blocks(wideTable = table,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys,
                    columnsToCopy = record_spec$recordKeys)
}

#' Divide or aggregate block records into row records.
#'
#' Call \code{blocks_to_rowrecs()}.
#'
#' @param table data (data.frame or relop).
#' @param record_spec a cdata_record_spec.
#' @return blocks_to_rowrecs() result.
#'
#' @export
#'
`%//%` <- function(table, record_spec) {
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = record_spec$recordKeys,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys)
}
