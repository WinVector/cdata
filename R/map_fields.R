
#' Map field values from one column into new derived columns (query based, takes name of table).
#'
#' @param dname name of table to re-map.
#' @param cname name of column to re-map.
#' @param mname name of table of data describing the mapping (cname column is source, derived columns are destinations).
#' @param my_db database handle.
#' @param rname name of result table.
#' @param ... force later arguments to be by name.
#' @param d_qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @param m_qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return re-mapped table
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   DBI::dbWriteTable(
#'     my_db,
#'     'd',
#'     data.frame(what = c("acc", "loss",
#'                         "val_acc", "val_loss"),
#'                score = c(0.8, 1.2,
#'                          0.7, 1.7),
#'                stringsAsFactors = FALSE),
#'     overwrite = TRUE,
#'     temporary = TRUE)
#'   DBI::dbWriteTable(
#'     my_db,
#'     'm',
#'     data.frame(what = c("acc", "loss",
#'                         "val_acc", "val_loss"),
#'                measure = c("accuracy", "log-loss",
#'                            "accuracy", "log-loss"),
#'                dataset = c("train", "train", "validation", "validation"),
#'                stringsAsFactors = FALSE),
#'     overwrite = TRUE,
#'     temporary = TRUE)
#'
#'   map_fields_q('d', 'what', 'm', my_db, "dm")
#'   cdata::qlook(my_db, 'dm')
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
map_fields_q <- function(dname, cname, mname, my_db, rname,
                         ...,
                         d_qualifiers = NULL,
                         m_qualifiers = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "cdata::map_fields_q")
  dests <- vapply(setdiff(cols(my_db,mname), cname),
                  function(di) {
                    rquery::quote_identifier(my_db, di)
                  }, character(1))
  qt <- paste0("m.",
               dests)
  q <- paste0("CREATE TABLE ",
              rname,
              " AS SELECT d.*, ",
              paste(qt, collapse = ", "),
              " FROM ",
              rquery::quote_table_name(my_db, dname, qualifiers = d_qualifiers),
              " d LEFT JOIN ",
              rquery::quote_table_name(my_db, mname, qualifiers = m_qualifiers),
              " m ON ",
              " d.", rquery::quote_identifier(my_db, cname),
              " = ",
              " m.", rquery::quote_identifier(my_db, cname))
  rquery::rq_execute(my_db, q)
  rname
}

#' Map field values from one column into new derived columns (takes a \code{data.frame}).
#'
#' @param d name of table to re-map.
#' @param cname name of column to re-map.
#' @param m name of table of data describing the mapping (cname column is source, derived columns are destinations).
#' @return re-mapped table
#'
#' @examples
#'
#' d <- data.frame(what = c("acc", "loss",
#'                          "val_acc", "val_loss"),
#'                 score = c(0.8, 1.2,
#'                        0.7, 1.7),
#'                 stringsAsFactors = FALSE)
#' m <- data.frame(what = c("acc", "loss",
#'                          "val_acc", "val_loss"),
#'                 measure = c("accuracy", "log-loss",
#'                             "accuracy", "log-loss"),
#'                 dataset = c("train", "train", "validation", "validation"),
#'                 stringsAsFactors = FALSE)
#' map_fields(d, 'what', m)
#'
#' @export
#'
map_fields <- function(d, cname, m) {
  dests <- setdiff(colnames(m), cname)
  for(ci in dests) {
    mp <- as.character(m[[ci]])
    names(mp) <- as.character(m[[cname]])
    d[[ci]] <- mp[d[[cname]]]
  }
  d
}

