
#' Check if table rows are uniquely keyed by keyset.
#'
#' Return TRUE if table rows are uniquely keyed by key_columns.
#'
#' @param table_rep rquery op_tree
#' @param key_columns character vector names of key columns
#' @param db database handle
#' @return TRUE if table rows are uniquely keyed by key columns
#'
#' @export
#' @keywords internal
#'
rows_are_uniquely_keyed <- function(table_rep, key_columns, db) {
  nk <- length(key_columns)
  if(nk<1) {
    return(rquery::rq_nrow(db, table_rep$table_name)<=1)
  }
  . <- NULL # don't look unbound for checks
  `:=` <- wrapr::`:=` # don't look unbound for checks
  tmp_col_name <- setdiff(
    paste0("cdata_temp_", seq_len(nk+1)),
    key_columns)[[1]]
  tmp_col_name <- as.name(tmp_col_name)
  ops <- table_rep %.>%
    rquery::select_columns(., key_columns) %.>%
    rquery::extend(., .(tmp_col_name) := 1) %.>%
    rquery::project(., .(tmp_col_name) := sum(.(tmp_col_name)),
                    groupby = key_columns) %.>%
    rquery::project(., mx = max(.(tmp_col_name)),
                    groupby = character(0))
  res <- rquery::execute(db, ops)
  res$mx[[1]]<=1
}

# my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
# d <- rquery::rq_copy_to(my_db,
#                         'd',
#                         data.frame(a = c("a", "a"),
#                                    b = c("1", "2"),
#                                    stringsAsFactors = FALSE),
#                         overwrite = TRUE,
#                         temporary = TRUE)
# cdata:::rows_are_uniquely_keyed(d, "a", my_db)
# cdata:::rows_are_uniquely_keyed(d, "b", my_db)
# cdata:::rows_are_uniquely_keyed(d, character(0), my_db)

