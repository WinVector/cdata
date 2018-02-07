
#' Map field values from one column into new derived columns (query based, takes name of table).
#'
#' @param dname name of table to re-map.
#' @param cname name of column to re-map.
#' @param mname name of table of data describing the mapping (cname column is source, derived columns are destinations).
#' @param my_db DBI database handle.
#' @param rname name of result table.
#' @return re-mapped table
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                         ":memory:")
#' DBI::dbWriteTable(
#'   my_db,
#'   'd',
#'   data.frame(what = c("acc", "loss",
#'                       "val_acc", "val_loss"),
#'              score = c(0.8, 1.2,
#'                        0.7, 1.7),
#'              stringsAsFactors = FALSE),
#'   overwrite = TRUE,
#'   temporary = TRUE)
#' DBI::dbWriteTable(
#'   my_db,
#'   'm',
#'   data.frame(what = c("acc", "loss",
#'                       "val_acc", "val_loss"),
#'              measure = c("accuracy", "log-loss",
#'                          "accuracy", "log-loss"),
#'              dataset = c("train", "train", "validation", "validation"),
#'              stringsAsFactors = FALSE),
#'   overwrite = TRUE,
#'   temporary = TRUE)
#'
#' map_fields_q('d', 'what', 'm', my_db, "dm")
#' cdata::qlook(my_db, 'dm')
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
map_fields_q <- function(dname, cname, mname, my_db, rname) {
  dests <- vapply(setdiff(cols(my_db,mname), cname),
                  function(di) {
                    DBI::dbQuoteIdentifier(my_db, di)
                  }, character(1))
  qt <- paste0("m.",
               dests)
  q <- paste0("CREATE TABLE ",
              rname,
              " AS SELECT d.*, ",
              paste(qt, collapse = ", "),
              " FROM ",
              DBI::dbQuoteIdentifier(my_db, dname),
              " d LEFT JOIN ",
              DBI::dbQuoteIdentifier(my_db, mname),
              " m ON ",
              " d.", DBI::dbQuoteIdentifier(my_db, cname),
              " = ",
              " m.", DBI::dbQuoteIdentifier(my_db, cname))
  DBI::dbGetQuery(my_db, q)
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

