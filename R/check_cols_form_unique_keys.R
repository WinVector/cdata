
#' @importFrom rqdatatable ex_data_table
NULL

#' Check columns form unique keys
#'
#' @param data data.frame to check
#' @param keyColNames character, names of columns to consider as keys
#' @return logical TRUE if rows are uniquely keyed by named columns
#'
#' @export
#' @keywords internal
#'
check_cols_form_unique_keys <- function(data, keyColNames) {
  # check args
  if(!is.data.frame(data)) {
    stop("cdata:::check_cols_form_unique_keys data should be a data.frame")
  }
  if(length(keyColNames)!=length(unique(keyColNames, allowNAKeys=TRUE))) {
    stop("cdata:::check_cols_form_unique_keys keyColNames must not have duplicates/NAs")
  }
  cn <- colnames(data)
  if(length(setdiff(keyColNames, cn))>0) {
    stop("cdata:::check_cols_form_unique_keys all keyColNames must be columns of data")
  }
  # get corner cases
  ndata <- nrow(data)
  if(ndata<=1) {
    return(TRUE)
  }
  if(length(keyColNames) <= 0) {
    return(FALSE)
  }
  {
    . <- NULL
    cdata_one_column <- NULL # don't look unbound
    cops <- rquery::local_td(data) %.>%
      rquery::select_columns(., keyColNames) %.>%
      rquery::extend(., cdata_one_column = 1) %.>%
      rquery::project(.,
              groupby = keyColNames,
              cdata_one_column = sum(cdata_one_column)) %.>%
      rquery::project(.,
              groupby = character(0),
              cdata_one_column = max(cdata_one_column))
    ctab <- rqdatatable::ex_data_table(cops, tables = list(data = data))
    count <- ctab[, 1, drop = TRUE]
    return(count<=1)
  }
  # wrapr::checkColsFormUniqueKeys(data, keyColNames)
}
