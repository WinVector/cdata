

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
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
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
#'
#'
#' @export
#'
rowrecs_to_blocks_spec <- function(controlTable,
                                   ...,
                                   recordKeys = character(0),
                                   controlTableKeys = colnames(controlTable)[[1]]) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::rowrecs_to_blocks_spec")
  ck <- checkControlTable(controlTable = controlTable, controlTableKeys = controlTableKeys, strict = FALSE)
  if(!is.null(ck)) {
    stop(paste("cdata::rowrecs_to_blocks_spec", ck))
  }
  r <- list(controlTable = controlTable,
            recordKeys = recordKeys,
            controlTableKeys = controlTableKeys)
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
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
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
#'
#' @export
#'
blocks_to_rowrecs_spec <- function(controlTable,
                                   ...,
                                   recordKeys = character(0),
                                   controlTableKeys = colnames(controlTable)[[1]]) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::blocks_to_rowrecs_spec")
  ck <- checkControlTable(controlTable = controlTable, controlTableKeys = controlTableKeys, strict = FALSE)
  if(!is.null(ck)) {
    stop(paste("cdata::blocks_to_rowrecs_spec", ck))
  }
  r <- list(controlTable = controlTable,
            recordKeys = recordKeys,
            controlTableKeys = controlTableKeys)
  class(r) <- "blocks_to_rowrecs_spec"
  r
}

# generate data frames representing both sides of a transform
get_transform_pair <- function(controlTable, controlTableKeys, recordKeys) {
  # build a of both sides of transform
  kf <- data.frame(x = 1)
  kf$x <- NULL
  for(cn in recordKeys) {
    kf[[cn]] <- "*"
  }
  row <- blocks_to_rowrecs(controlTable,
                           keyColumns = character(0),
                           controlTable = controlTable,
                           controlTableKeys = controlTableKeys)
  row <- cbind(kf, row)
  block <- cbind(kf, controlTable)
  list(block_record = block,
       block_keys = c(recordKeys, controlTableKeys),
       row_record = row,
       row_keys = recordKeys)
}

#' @export
#'
format.rowrecs_to_blocks_spec <- function(x, ...) {
  sides <- get_transform_pair(x$controlTable, x$controlTableKeys, x$recordKeys)
  row_record <- sides$row_record
  block_record <- sides$block_record
  row_str <- wrapr::draw_frame(row_record)
  block_str <- wrapr::draw_frame(block_record)
  fmt_str <- paste0("{\n ",
                    row_str,
                    " row_keys <- ", wrapr::map_to_char(x$recordKeys),
                    "\n\n # becomes\n\n " ,
                    block_str,
                    " block_keys <- ", wrapr::map_to_char(c(x$recordKeys, x$controlTableKeys)),
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
  sides <- get_transform_pair(x$controlTable, x$controlTableKeys, x$recordKeys)
  row_record <- sides$row_record
  block_record <- sides$block_record
  row_str <- wrapr::draw_frame(row_record)
  block_str <- wrapr::draw_frame(block_record)
  fmt_str <- paste0("{\n ",
                    block_str,
                    " block_keys <- ", wrapr::map_to_char(c(x$recordKeys, x$controlTableKeys)),
                    "\n\n # becomes\n\n " ,
                    row_str,
                    " row_keys <- ", wrapr::map_to_char(x$recordKeys),
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
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
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
                    columnsToCopy = transform$recordKeys)
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
#'     measure, value |
#'     AUC    , "AUC" |
#'     R2     , "R2"  ),
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
                    controlTableKeys = transform$controlTableKeys)
}



# pipe interface



#' @export
#'
apply_right.rowrecs_to_blocks_spec <- function(pipe_left_arg,
                                               pipe_right_arg,
                                               pipe_environment,
                                               left_arg_name,
                                               pipe_string,
                                               right_arg_name) {
  table <- pipe_left_arg
  record_spec <- pipe_right_arg
  rowrecs_to_blocks(wideTable = table,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys,
                    columnsToCopy = record_spec$recordKeys)
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
  record_spec <- pipe_right_arg
  blocks_to_rowrecs(tallTable = table,
                    keyColumns = record_spec$recordKeys,
                    controlTable = record_spec$controlTable,
                    controlTableKeys = record_spec$controlTableKeys)
}


