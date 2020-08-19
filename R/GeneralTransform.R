
#' General transform from arbitrary record shape to arbitrary record shape.
#'
#' @param table data.frame or relop.
#' @param incoming_shape data.frame, definition of incoming record shape.
#' @param outgoing_shape data.frame, defintion of outgoing record shape.
#' @param ... force later arguments to bind by name.
#' @param keyColumns character vector of column defining incoming row groups
#' @param columnsToCopy_in character array of incoming column names to copy.
#' @param checkNames logical, if TRUE check names.
#' @param checkKeys logical, if TRUE check columnsToCopy form row keys (not a requirement, unless you want to be able to invert the operation).
#' @param strict logical, if TRUE check control table name forms.
#' @param incoming_controlTableKeys character, which column names of the incoming control table are considered to be keys.
#' @param outgoing_controlTableKeys character, which column names of the outgoing control table are considered to be keys.
#' @param tmp_name_source a tempNameGenerator from cdata::mk_tmp_name_source()
#' @param temporary logical, if TRUE use temporary tables
#' @param allow_rqdatatable_in logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @param allow_rqdatatable_out logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @return processing pipeline or transformed table
#'
#'
#' @examples
#'
#'
#' incoming_shape <- qchar_frame(
#'   "row",  "col1", "col2", "col3" |
#'   "row1",   v11,     v12,  v13   |
#'   "row2",   v21,     v22,  v23   |
#'   "row3",   v31,     v32,  v33   )
#'
#'
#' outgoing_shape <- qchar_frame(
#'   "column", "row1", "row2", "row3" |
#'   "col1",      v11,  v21  ,  v31   |
#'   "col2",      v12,  v22  ,  v32   |
#'   "col3",      v13,  v23  ,  v33   )
#'
#' data <- build_frame(
#'   'record_id', 'row',  'col1', 'col2', 'col3'  |
#'   1,           'row1',  1,      2,      3      |
#'   1,           'row2',  4,      5,      6      |
#'   1,           'row3',  7,      8,      9      |
#'   2,           'row1',  11,     12,     13     |
#'   2,           'row2',  14,     15,     16     |
#'   2,           'row3',  17,     18,     19     )
#'
#' print(data)
#'
#' convert_records(
#'   data,
#'   keyColumns = 'record_id',
#'   incoming_shape = incoming_shape,
#'   outgoing_shape = outgoing_shape)
#'
#' td <- rquery::local_td(data)
#'
#' ops <- convert_records(
#'   td,
#'   keyColumns = 'record_id',
#'   incoming_shape = incoming_shape,
#'   outgoing_shape = outgoing_shape)
#'
#' cat(format(ops))
#'
#'
#'
#' @export
#'
convert_records <- function(table,
                            incoming_shape = NULL,
                            outgoing_shape = NULL,
                            ...,
                            keyColumns = NULL,
                            columnsToCopy_in = NULL,
                            checkNames = TRUE,
                            checkKeys = FALSE,
                            strict = FALSE,
                            incoming_controlTableKeys = colnames(incoming_shape)[[1]],
                            outgoing_controlTableKeys = colnames(outgoing_shape)[[1]],
                            tmp_name_source = wrapr::mk_tmp_name_source("crec"),
                            temporary = TRUE,
                            allow_rqdatatable_in = FALSE,
                            allow_rqdatatable_out = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::convert_records")
  if(!is.null(incoming_shape)) {
    if(!is.data.frame(incoming_shape)) {
      stop("cdata::convert_records incoming_shape should be a data.frame")
    }
  }
  if(!is.null(outgoing_shape)) {
    if(!is.data.frame(outgoing_shape)) {
      stop("cdata::convert_records outgoing_shape should be a data.frame")
    }
  }
  result <- table
  if(!is.null(incoming_shape)) {
    result <- blocks_to_rowrecs(
      result,
      keyColumns = keyColumns,
      controlTable = incoming_shape,
      columnsToCopy = columnsToCopy_in,
      checkNames = checkNames,
      strict = strict,
      controlTableKeys = incoming_controlTableKeys,
      tmp_name_source = tmp_name_source,
      temporary = temporary,
      allow_rqdatatable = allow_rqdatatable_in)
  }
  if(!is.null(outgoing_shape)) {
    result <- rowrecs_to_blocks(
      result,
      controlTable = outgoing_shape,
      checkNames = checkNames,
      checkKeys = checkKeys,
      strict = strict,
      controlTableKeys = outgoing_controlTableKeys,
      columnsToCopy = c(keyColumns, columnsToCopy_in),
      tmp_name_source = tmp_name_source,
      temporary = temporary,
      allow_rqdatatable = allow_rqdatatable_out)
  }
  result
}
