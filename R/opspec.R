

#' @importFrom methods setGeneric
NULL

#' Create a row records to block records transform specification.
#'
#' Create a row records to block records transform specification object that holds the pivot control table, specification of
#' extra row keys, and control table keys.
#'
#' @param controlTable an all character data frame or cdata pivot control.
#' @param ... not used, force later arguments to bind by name.
#' @param recordKeys vector of columns identifying records.
#' @param controlTableKeys vector of keying columns of the controlTable.
#' @param checkNames passed to rowrecs_to_blocks.
#' @param checkKeys passed to rowrecs_to_blocks.
#' @param strict passed to rowrecs_to_blocks.
#' @param allow_rqdatatable logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @return a record specification object
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id"  , "AUC", "R2" |
#'     1   , 0.7  , 0.4  |
#'     2   , 0.8  , 0.5  )
#'
#' transform <- rowrecs_to_blocks_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' print(transform)
#'
#' d %.>% transform
#'
#' inv_transform <- t(transform)
#' print(inv_transform)
#'
#' # identity (in structure)
#' d %.>% transform %.>% inv_transform
#'
#' # identity again (using .() "immediate" notation)
#' d %.>% transform %.>% .(t(transform))
#'
#' @export
#'
rowrecs_to_blocks_spec <- function(controlTable,
                                   ...,
                                   recordKeys = character(0),
                                   controlTableKeys = colnames(controlTable)[[1]],
                                   checkNames = TRUE,
                                   checkKeys = FALSE,
                                   strict = FALSE,
                                   allow_rqdatatable = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks_spec")
  controlTable <- as.data.frame(controlTable)
  rownames(controlTable) <- NULL
  ck <- checkControlTable(controlTable = controlTable, controlTableKeys = controlTableKeys, strict = FALSE)
  if(!is.null(ck)) {
    stop(paste("cdata::rowrecs_to_blocks_spec", ck))
  }
  if(length(intersect(recordKeys, colnames(controlTable)))>0) {
    stop("cdata::rowrecs_to_blocks_spec recordKeys intersected control table columns")
  }
  r <- list(controlTable = controlTable,
            recordKeys = recordKeys,
            controlTableKeys = controlTableKeys,
            checkNames = checkNames,
            checkKeys = checkKeys,
            strict = strict,
            allow_rqdatatable = allow_rqdatatable)
  class(r) <- "rowrecs_to_blocks_spec"
  r
}

#' Create a block records to row records transform specification.
#'
#' Create a block records to row records transform specification object that holds the pivot control table, specification of
#' extra row keys, and control table keys.
#'
#' @param controlTable an all character data frame or cdata pivot control.
#' @param ... not used, force later arguments to bind by name.
#' @param recordKeys vector of columns identifying records.
#' @param controlTableKeys vector of keying columns of the controlTable.
#' @param checkNames passed to blocks_to_rowrecs.
#' @param checkKeys passed to blocks_to_rowrecs.
#' @param strict passed to blocks_to_rowrecs.
#' @param allow_rqdatatable logical, if TRUE allow rqdatatable shortcutting on simple conversions.
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
#' transform <- blocks_to_rowrecs_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' print(transform)
#'
#' d %.>% transform
#'
#' inv_transform <- t(transform)
#' print(inv_transform)
#'
#' # identity (in structure)
#' d %.>% transform %.>% inv_transform
#'
#' # identity again (using .() "immediate" notation)
#' d %.>% transform %.>% .(t(transform))
#'
#'
#' @export
#'
blocks_to_rowrecs_spec <- function(controlTable,
                                   ...,
                                   recordKeys = character(0),
                                   controlTableKeys = colnames(controlTable)[[1]],
                                   checkNames = TRUE,
                                   checkKeys = TRUE,
                                   strict = FALSE,
                                   allow_rqdatatable = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs_spec")
  controlTable <- as.data.frame(controlTable)
  rownames(controlTable) <- NULL
  ck <- checkControlTable(controlTable = controlTable, controlTableKeys = controlTableKeys, strict = FALSE)
  if(!is.null(ck)) {
    stop(paste("cdata::blocks_to_rowrecs_spec", ck))
  }
  if(length(intersect(recordKeys, colnames(controlTable)))>0) {
    stop("cdata::blocks_to_rowrecs_spec recordKeys intersected control table columns")
  }
  r <- list(controlTable = controlTable,
            recordKeys = recordKeys,
            controlTableKeys = controlTableKeys,
            checkNames = checkNames,
            checkKeys = checkKeys,
            strict = strict,
            allow_rqdatatable = allow_rqdatatable)
  class(r) <- "blocks_to_rowrecs_spec"
  r
}

#' Upack details of a cdata record transform.
#'
#' Unpack details, especially: generate data frames representing both sides of a transform.
#'
#' @param x blocks_to_rowrecs_spec or rowrecs_to_blocks_spec
#' @return detailed fields
#'
#' @export
#' @keywords internal
#'
get_transform_details <- function(x) {
  controlTable = x$controlTable
  controlTableKeys = x$controlTableKeys
  recordKeys = x$recordKeys
  # build a of both sides of transform
  kf <- data.frame(x = 1)
  kf$x <- NULL
  for(cn in recordKeys) {
    kf[[cn]] <- "."
  }
  row <- blocks_to_rowrecs(controlTable,
                           keyColumns = character(0),
                           controlTable = controlTable,
                           controlTableKeys = controlTableKeys)
  row <- cbind(kf, row)
  block <- cbind(kf, controlTable)
  args <- x[setdiff(names(x), c("controlTable", "recordKeys", "controlTableKeys"))]
  list(block_record = block,
       block_keys = c(recordKeys, controlTableKeys),
       row_record = row,
       row_keys = recordKeys,
       args = args,
       class = class(x))
}

#' @export
#'
format.rowrecs_to_blocks_spec <- function(x, ...) {
  sides <- get_transform_details(x)
  row_record <- sides$row_record
  block_record <- sides$block_record
  row_str <- wrapr::draw_framec(row_record, unquote_cols = colnames(row_record))
  block_str <- wrapr::draw_framec(block_record, unquote_cols = setdiff(colnames(block_record), x$controlTableKeys))
  args <- x[setdiff(names(x), c("controlTable", "recordKeys", "controlTableKeys"))]
  fmt_str <- paste0("{\n ",
                    row_str,
                    " row_keys <- ", wrapr::map_to_char(x$recordKeys),
                    "\n\n # becomes\n\n " ,
                    block_str,
                    " block_keys <- ", wrapr::map_to_char(c(x$recordKeys, x$controlTableKeys)),
                    "\n\n # args: ", gsub("['\"]+", "", wrapr::map_to_char(args)),
                    "\n}\n\n")
  fmt_str
}

#' @export
#'
print.rowrecs_to_blocks_spec <- function(x, ...) {
  fmt_str <- format(x)
  cat(fmt_str)
  invisible(fmt_str)
}


#' @export
#'
format.blocks_to_rowrecs_spec <- function(x, ...) {
  sides <- get_transform_details(x)
  row_record <- sides$row_record
  block_record <- sides$block_record
  row_str <- wrapr::draw_framec(row_record, unquote_cols = colnames(row_record))
  block_str <- wrapr::draw_framec(block_record, unquote_cols = setdiff(colnames(block_record), x$controlTableKeys))
  args <- x[setdiff(names(x), c("controlTable", "recordKeys", "controlTableKeys"))]
  fmt_str <- paste0("{\n ",
                    block_str,
                    " block_keys <- ", wrapr::map_to_char(c(x$recordKeys, x$controlTableKeys)),
                    "\n\n # becomes\n\n " ,
                    row_str,
                    " row_keys <- ", wrapr::map_to_char(x$recordKeys),
                    "\n\n # args: ", gsub("['\"]+", "", wrapr::map_to_char(args)),
                    "\n}\n\n")
  fmt_str
}

#' @export
#'
print.blocks_to_rowrecs_spec <- function(x, ...) {
  fmt_str <- format(x)
  cat(fmt_str)
  invisible(fmt_str)
}

#' @export
#'
t.rowrecs_to_blocks_spec <- function(x) {
  class(x) <- "blocks_to_rowrecs_spec"
  return(x)
}

#' @export
#'
t.blocks_to_rowrecs_spec <- function(x) {
  class(x) <- "rowrecs_to_blocks_spec"
  return(x)
}





# use the transform spec

#' Use transform spec to layout data.
#'
#' @param transform object of class rowrecs_to_blocks_spec
#' @param table data.frame or relop.
#' @return re-arranged data or data reference (relop).
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id"  , "AUC", "R2" |
#'     1   , 0.7  , 0.4  |
#'     2   , 0.8  , 0.5  )
#' transform <- rowrecs_to_blocks_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#' print(transform)
#' layout_by(transform, d)
#'
#' d <- wrapr::build_frame(
#'   "id", "measure", "value" |
#'   1   , "AUC"    , 0.7     |
#'   1   , "R2"     , 0.4     |
#'   2   , "AUC"    , 0.8     |
#'   2   , "R2"     , 0.5     )
#' transform <- blocks_to_rowrecs_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#' print(transform)
#' layout_by(transform, d)
#'
#' @export
#'
layout_by <- function(transform, table) {
  UseMethod("layout_by")
}

#' Use transform spec to layout data.
#'
#' @param transform object of class rowrecs_to_blocks_spec
#' @param table data.frame or relop.
#' @return re-arranged data or data reference (relop).
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id"  , "AUC", "R2" |
#'     1   , 0.7  , 0.4  |
#'     2   , 0.8  , 0.5  )
#'
#' transform <- rowrecs_to_blocks_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' print(transform)
#' layout_by(transform, d)
#'
#' @export
#'
layout_by.rowrecs_to_blocks_spec <- function(transform, table) {
  rowrecs_to_blocks(wideTable = table,
                    controlTable = transform$controlTable,
                    controlTableKeys = transform$controlTableKeys,
                    columnsToCopy = transform$recordKeys,
                    checkNames = transform$checkNames,
                    checkKeys = transform$checkKeys,
                    strict = transform$strict,
                    allow_rqdatatable = transform$allow_rqdatatable)
}



#' Use transform spec to layout data.
#'
#' @param transform object of class blocks_to_rowrecs_spec.
#' @param table data.frame or relop.
#' @return re-arranged data or data reference (relop).
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
#' transform <- blocks_to_rowrecs_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' print(transform)
#'
#' layout_by(transform, d)
#'
#' @export
#'
layout_by.blocks_to_rowrecs_spec <- function(transform, table) {
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = transform$recordKeys,
                    controlTable = transform$controlTable,
                    controlTableKeys = transform$controlTableKeys,
                    checkNames = transform$checkNames,
                    checkKeys = transform$checkKeys,
                    strict = transform$strict,
                    allow_rqdatatable = transform$allow_rqdatatable)
}




#' Multiply/join row records into block records.
#'
#' Call \code{rowrecs_to_blocks()}.
#'
#' @param table data (data.frame or relop).
#' @param transform a rowrecs_to_blocks_spec.
#' @return rowrecs_to_blocks() result.
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'   "id", "AUC", "R2" |
#'   1   , 0.7  , 0.4  |
#'   2   , 0.8  , 0.5  )
#'
#' transform <- rowrecs_to_blocks_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' d %**% transform
#'
#' # identity (in structure)
#' d %**% transform %//% t(transform)
#'
#' @export
#'
`%**%` <- function(table, transform) {
  if(!("rowrecs_to_blocks_spec" %in% class(transform))) {
    stop("cdata::`%**%` transform must be of class rowrecs_to_blocks_spec")
  }
  rowrecs_to_blocks(wideTable = table,
                    controlTable = transform$controlTable,
                    controlTableKeys = transform$controlTableKeys,
                    columnsToCopy = transform$recordKeys,
                    checkNames = transform$checkNames,
                    checkKeys = transform$checkKeys,
                    strict = transform$strict,
                    allow_rqdatatable = transform$allow_rqdatatable)
}

#' Factor-out (aggregate/project) block records into row records.
#'
#' Call \code{blocks_to_rowrecs()}.
#'
#' @param table data (data.frame or relop).
#' @param transform a rowrecs_to_blocks_spec.
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
#' transform <- blocks_to_rowrecs_spec(
#'   wrapr::qchar_frame(
#'     "measure", "value" |
#'     "AUC"    , AUC     |
#'     "R2"     , R2      ),
#'   recordKeys = "id")
#'
#' d %//% transform
#'
#' # identity (in structure)
#' d %//% transform %**% t(transform)
#'
#' @export
#'
`%//%` <- function(table, transform) {
  if(!("blocks_to_rowrecs_spec" %in% class(transform))) {
    stop("cdata::`%//%` transform must be of class blocks_to_rowrecs_spec")
  }
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = transform$recordKeys,
                    controlTable = transform$controlTable,
                    controlTableKeys = transform$controlTableKeys,
                    checkNames = transform$checkNames,
                    checkKeys = transform$checkKeys,
                    strict = transform$strict,
                    allow_rqdatatable = transform$allow_rqdatatable)
}



# wrapr dot-pipe interface



#' @export
#'
apply_right.rowrecs_to_blocks_spec <- function(pipe_left_arg,
                                               pipe_right_arg,
                                               pipe_environment,
                                               left_arg_name,
                                               pipe_string,
                                               right_arg_name) {
  table <- pipe_left_arg
  transform_spec <- pipe_right_arg
  rowrecs_to_blocks(wideTable = table,
                    controlTable = transform_spec$controlTable,
                    controlTableKeys = transform_spec$controlTableKeys,
                    columnsToCopy = transform_spec$recordKeys,
                    checkNames = transform_spec$checkNames,
                    checkKeys = transform_spec$checkKeys,
                    strict = transform_spec$strict,
                    allow_rqdatatable = transform_spec$allow_rqdatatable)
}

#' @export
#'
apply_right.blocks_to_rowrecs_spec <- function(pipe_left_arg,
                                               pipe_right_arg,
                                               pipe_environment,
                                               left_arg_name,
                                               pipe_string,
                                               right_arg_name) {
  table <- pipe_left_arg
  transform_spec <- pipe_right_arg
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = transform_spec$recordKeys,
                    controlTable = transform_spec$controlTable,
                    controlTableKeys = transform_spec$controlTableKeys,
                    checkNames = transform_spec$checkNames,
                    checkKeys = transform_spec$checkKeys,
                    strict = transform_spec$strict,
                    allow_rqdatatable = transform_spec$allow_rqdatatable)
}




# general spec


#' Create a record to record spec.
#'
#' Create a general record to record transform specification.
#'
#' @param incoming_shape data.frame, definition of incoming record shape.
#' @param outgoing_shape data.frame, defintion of outgoing record shape.
#' @param ... not used, force later arguments to bind by name.
#' @param recordKeys vector of columns identifying records.
#' @param incoming_controlTableKeys character, which column names of the incoming control table are considered to be keys.
#' @param outgoing_controlTableKeys character, which column names of the outgoing control table are considered to be keys.
#' @param checkNames passed to rowrecs_to_blocks.
#' @param checkKeys passed to rowrecs_to_blocks.
#' @param strict passed to rowrecs_to_blocks.
#' @param allow_rqdatatable_in logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @param allow_rqdatatable_out logical, if TRUE allow rqdatatable shortcutting on simple conversions.
#' @return a record specification object
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
#' layout <- layout_specification(
#'   incoming_shape = incoming_shape,
#'   outgoing_shape = outgoing_shape,
#'   recordKeys = 'record_id')
#'
#' print(layout)
#'
#' data %.>% layout
#'
#' data %.>% layout %.>% .(t(layout))
#'
#' @export
#'
layout_specification <- function(incoming_shape = NULL,
                                 outgoing_shape = NULL,
                                 ...,
                                 recordKeys = character(0),
                                 incoming_controlTableKeys = colnames(incoming_shape)[[1]],
                                 outgoing_controlTableKeys = colnames(outgoing_shape)[[1]],
                                 checkNames = TRUE,
                                 checkKeys = TRUE,
                                 strict = FALSE,
                                 allow_rqdatatable_in = FALSE,
                                 allow_rqdatatable_out = FALSE) {
  # check for some trivial cases
  if(is.null(incoming_shape)) {
    if(is.null(outgoing_shape)) {
      stop("at least one of incoming_shape or outgoing_shape must not be NULL")
    }
    return(rowrecs_to_blocks_spec(outgoing_shape,
                                  controlTableKeys = outgoing_controlTableKeys,
                                  recordKeys = character(0),
                                  checkNames = checkNames,
                                  checkKeys = checkKeys,
                                  strict = strict,
                                  allow_rqdatatable = allow_rqdatatable_out))
  }
  if(is.null(outgoing_shape)) {
    return(blocks_to_rowrecs_spec(incoming_shape,
                                  controlTableKeys = incoming_controlTableKeys,
                                  recordKeys = recordKeys,
                                  checkNames = checkNames,
                                  checkKeys = checkKeys,
                                  strict = strict,
                                  allow_rqdatatable = allow_rqdatatable_in))
  }
  ca <- blocks_to_rowrecs_spec(incoming_shape,
                               controlTableKeys = incoming_controlTableKeys,
                               recordKeys = character(0),
                               checkNames = checkNames,
                               checkKeys = checkKeys,
                               strict = strict,
                               allow_rqdatatable = allow_rqdatatable_in)
  cb <- rowrecs_to_blocks_spec(outgoing_shape,
                               controlTableKeys = outgoing_controlTableKeys,
                               recordKeys = character(0),
                               checkNames = checkNames,
                               checkKeys = checkKeys,
                               strict = strict,
                               allow_rqdatatable = allow_rqdatatable_out)
  if((nrow(outgoing_shape)==1) && (length(outgoing_controlTableKeys)==0)) {
    ra <- incoming_shape %.>% ca
    if(isTRUE(all.equal(sort(colnames(ra)), sort(colnames(outgoing_shape))))) {
      return(blocks_to_rowrecs_spec(incoming_shape,
                                    controlTableKeys = incoming_controlTableKeys,
                                    recordKeys = recordKeys,
                                    checkNames = checkNames,
                                    checkKeys = checkKeys,
                                    strict = strict,
                                    allow_rqdatatable = allow_rqdatatable_in))
    }
  }
  if((nrow(incoming_shape)==1) && (length(incoming_controlTableKeys)==0)) {
    cb_inv <- t(cb)
    rb <- outgoing_shape %.>% cb_inv
    if(isTRUE(all.equal(sort(colnames(rb)), sort(colnames(incoming_shape))))) {
      return(rowrecs_to_blocks_spec(outgoing_shape,
                                    controlTableKeys = outgoing_controlTableKeys,
                                    recordKeys = character(0),
                                    checkNames = checkNames,
                                    checkKeys = checkKeys,
                                    strict = strict,
                                    allow_rqdatatable = allow_rqdatatable_out))
    }
  }
  # check transform makes sense
  tryCatch(
    { incoming_shape %.>% ca %.>% cb },
    error = function(e) {
      stop("cdata::layout_specification, incoming and outgoing shape not compatible")
    }
  )
  a <- blocks_to_rowrecs_spec(incoming_shape,
                              controlTableKeys = incoming_controlTableKeys,
                              recordKeys = recordKeys,
                              checkNames = checkNames,
                              checkKeys = checkKeys,
                              strict = strict,
                              allow_rqdatatable = allow_rqdatatable_in)
  b <- rowrecs_to_blocks_spec(outgoing_shape,
                              controlTableKeys = outgoing_controlTableKeys,
                              recordKeys = recordKeys,
                              checkNames = checkNames,
                              checkKeys = checkKeys,
                              strict = strict,
                              allow_rqdatatable = allow_rqdatatable_out)
  r <- list(
    blocks_to_rowrecs_spec = a,
    rowrecs_to_blocks_spec = b)
  class(r) <- "cdata_general_transform_spec"
  r
}

#' @export
#'
apply_right.cdata_general_transform_spec <- function(pipe_left_arg,
                                                     pipe_right_arg,
                                                     pipe_environment,
                                                     left_arg_name,
                                                     pipe_string,
                                                     right_arg_name) {
  table <- pipe_left_arg
  transform_spec <- pipe_right_arg
  blocks_to_rowrecs_spec <- transform_spec$blocks_to_rowrecs_spec
  if(!is.null(blocks_to_rowrecs_spec)) {
    table <- blocks_to_rowrecs(tallTable = table,
                               keyColumns = blocks_to_rowrecs_spec$recordKeys,
                               controlTable = blocks_to_rowrecs_spec$controlTable,
                               controlTableKeys = blocks_to_rowrecs_spec$controlTableKeys,
                               checkNames = blocks_to_rowrecs_spec$checkNames,
                               checkKeys = blocks_to_rowrecs_spec$checkKeys,
                               strict = blocks_to_rowrecs_spec$strict,
                               allow_rqdatatable = blocks_to_rowrecs_spec$allow_rqdatatable)
  }
  rowrecs_to_blocks_spec <- transform_spec$rowrecs_to_blocks_spec
  if(!is.null(rowrecs_to_blocks_spec)) {
    table <- rowrecs_to_blocks(wideTable = table,
                               controlTable = rowrecs_to_blocks_spec$controlTable,
                               controlTableKeys = rowrecs_to_blocks_spec$controlTableKeys,
                               columnsToCopy = rowrecs_to_blocks_spec$recordKeys,
                               checkNames = rowrecs_to_blocks_spec$checkNames,
                               checkKeys = rowrecs_to_blocks_spec$checkKeys,
                               strict = rowrecs_to_blocks_spec$strict,
                               allow_rqdatatable = rowrecs_to_blocks_spec$allow_rqdatatable)
  }
  return(table)
}

#' @export
#'
t.cdata_general_transform_spec <- function(x) {
  layout_specification(incoming_shape = x$rowrecs_to_blocks_spec$controlTable,
                       outgoing_shape = x$blocks_to_rowrecs_spec$controlTable,
                       recordKeys = x$rowrecs_to_blocks_spec$recordKeys,
                       incoming_controlTableKeys = x$rowrecs_to_blocks_spec$controlTableKeys,
                       outgoing_controlTableKeys = x$blocks_to_rowrecs_spec$controlTableKeys,
                       checkNames = x$rowrecs_to_blocks_spec$checkNames,
                       checkKeys = x$rowrecs_to_blocks_spec$checkKeys,
                       strict = x$rowrecs_to_blocks_spec$strict,
                       allow_rqdatatable = x$rowrecs_to_blocks_spec$allow_rqdatatable)
}


#' Use transform spec to layout data.
#'
#' @param transform object of class blocks_to_rowrecs_spec.
#' @param table data.frame or relop.
#' @return re-arranged data or data reference (relop).
#'
#'
#' @export
#'
layout_by.cdata_general_transform_spec <- function(transform, table) {
  blocks_to_rowrecs_spec <- transform$blocks_to_rowrecs_spec
  table <- blocks_to_rowrecs(tallTable = table,
                             keyColumns = blocks_to_rowrecs_spec$recordKeys,
                             controlTable = blocks_to_rowrecs_spec$controlTable,
                             controlTableKeys = blocks_to_rowrecs_spec$controlTableKeys,
                             checkNames = blocks_to_rowrecs_spec$checkNames,
                             checkKeys = blocks_to_rowrecs_spec$checkKeys,
                             strict = blocks_to_rowrecs_spec$strict,
                             allow_rqdatatable = blocks_to_rowrecs_spec$allow_rqdatatable)
  rowrecs_to_blocks_spec <- transform$rowrecs_to_blocks_spec
  rowrecs_to_blocks(wideTable = table,
                    controlTable = rowrecs_to_blocks_spec$controlTable,
                    controlTableKeys = rowrecs_to_blocks_spec$controlTableKeys,
                    columnsToCopy = rowrecs_to_blocks_spec$recordKeys,
                    checkNames = rowrecs_to_blocks_spec$checkNames,
                    checkKeys = rowrecs_to_blocks_spec$checkKeys,
                    strict = rowrecs_to_blocks_spec$strict,
                    allow_rqdatatable = rowrecs_to_blocks_spec$allow_rqdatatable)
}

#' @export
#'
format.cdata_general_transform_spec <- function(x, ...) {
  details_in <- get_transform_details(x$blocks_to_rowrecs_spec)
  details_out <- get_transform_details(x$rowrecs_to_blocks_spec)
  in_record <- details_in$block_record
  out_record <- details_out$block_record
  in_str <- wrapr::draw_framec(in_record, unquote_cols = setdiff(colnames(in_record),
                                                                 x$blocks_to_rowrecs_spec$controlTableKeys))
  out_str <- wrapr::draw_framec(out_record, unquote_cols = setdiff(colnames(out_record),
                                                                   x$rowrecs_to_blocks_spec$controlTableKeys))
  args <- x$blocks_to_rowrecs_spec[setdiff(names(x$blocks_to_rowrecs_spec),
                                           c("controlTable", "recordKeys", "controlTableKeys"))]
  fmt_str <- paste0("{\n ",
                    in_str,
                    " in_keys <- ", wrapr::map_to_char(c(x$blocks_to_rowrecs_spec$recordKeys,
                                                         x$blocks_to_rowrecs_spec$controlTableKeys)),
                    "\n\n # becomes\n\n " ,
                    out_str,
                    " out_keys <- ", wrapr::map_to_char(c(x$rowrecs_to_blocks_spec$recordKeys,
                                                          x$rowrecs_to_blocks_spec$controlTableKeys)),
                    "\n\n # args: ", gsub("['\"]+", "", wrapr::map_to_char(args)),
                    "\n}\n\n")
  fmt_str
}

#' @export
#'
print.cdata_general_transform_spec <- function(x, ...) {
  fmt_str <- format(x)
  cat(fmt_str)
  invisible(fmt_str)
}

