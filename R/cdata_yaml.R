
#' @importFrom methods is
NULL

convert_yaml_to_data_frame <- function(obj) {
  n <- length(obj)
  if (n<1) {
    return(NULL)
  }
  n1 = names(obj)[[1]]
  d = data.frame(x = obj[[n1]],
                 stringsAsFactors = FALSE)
  colnames(d) <- n1
  if (n<2) {
    return(d)
  }
  for (i in (2:n)) {
    ni = names(obj)[[i]]
    d[[ni]] <- obj[[ni]]
  }
  return(d)
}

convert_data_frame_to_yaml <- function(d) {
  lst = list()
  for(n in colnames(d)) {
    lst[[n]] <- d[[n]]
  }
  return(lst)
}


convert_yaml_to_record_spec <- function(obj) {
  record_keys <- NULL
  control_table_keys <- NULL
  if ("record_keys" %in% names(obj)) {
    record_keys = obj$record_keys
  }
  if ("control_table_keys" %in% names(obj)) {
    control_table_keys = obj$control_table_keys
  }
  control_table <- convert_yaml_to_data_frame(obj$control_table)
  return(list('record_keys' = record_keys,
              'control_table_keys' = control_table_keys,
              'control_table' = control_table))
}


#' Read a cdata record transform from a simple object (such as is imported from YAML).
#'
#' @param obj object to convert
#' @return cdata transform specification
#'
#' @export
#'
convert_yaml_to_cdata_spec <- function(obj) {
  blocks_in = NULL
  blocks_out = NULL
  if ("blocks_out"  %in% names(obj)) {
    blocks_out = convert_yaml_to_record_spec(obj$blocks_out)
  }
  if ("blocks_in"  %in% names(obj)) {
    blocks_in = convert_yaml_to_record_spec(obj$blocks_in)
  }
  # TODO: work on check/strict options
  if ((!is.null(blocks_in)) && (!is.null(blocks_out))) {
    # TODO: work on recordKeys point
    return(layout_specification(incoming_shape = blocks_in$control_table,
                                outgoing_shape = blocks_out$control_table,
                                recordKeys = blocks_in$record_keys,
                                incoming_controlTableKeys = blocks_in$control_table_keys,
                                outgoing_controlTableKeys = blocks_out$control_table_keys))
  }
  if (!is.null(blocks_in)) {
    return(blocks_to_rowrecs_spec(controlTable = blocks_in$control_table,
                                  recordKeys = blocks_in$record_keys,
                                  controlTableKeys = blocks_in$control_table_keys))
  }
  if (!is.null(blocks_out)) {
    return(rowrecs_to_blocks_spec(controlTable = blocks_out$control_table,
                                  recordKeys = blocks_out$record_keys,
                                  controlTableKeys = blocks_out$control_table_keys))
  }
  return(NULL)
}


#' Convert a layout_specification, blocks_to_rowrecs_spec, or rowrecs_to_blocks_spec to a simple object.
#'
#' @param spec a layout_specification, blocks_to_rowrecs_spec, or rowrecs_to_blocks_spec
#' @return a simple object suitable for YAML serialization
#'
#' @export
#'
convert_cdata_spec_to_yaml <- function(spec) {
  if(is(spec, "blocks_to_rowrecs_spec")) {
    return(list(
      blocks_in = list(
        record_keys = spec$recordKeys,
        control_table_keys = spec$controlTableKeys,
        control_table = spec$controlTable
      )
    ))
  }
  if(is(spec, "rowrecs_to_blocks_spec")) {
    return(list(
      blocks_out = list(
        record_keys = spec$recordKeys,
        control_table_keys = spec$controlTableKeys,
        control_table = spec$controlTable
      )
    ))
  }
  if(is(spec, "cdata_general_transform_spec")) {
    blocks_in = convert_cdata_spec_to_yaml(spec$blocks_to_rowrecs_spec)
    blocks_out = convert_cdata_spec_to_yaml(spec$rowrecs_to_blocks_spec)
    return(list(
      blocks_in = blocks_in$blocks_in,
      blocks_out = blocks_out$blocks_out
    ))
  }
  stop(paste("unexpected class: ", paste(class(spec), collapse = ', ')))
}

