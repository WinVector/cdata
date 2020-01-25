
library(rqdatatable)

test_complex_example <- function() {
  # example data
  d <- wrapr::build_frame(
    "ID"  , "OP", "DATE"                |
      1   , "A" , "2001-01-02 00:00:00" |
      1   , "B" , "2015-04-25 00:00:00" |
      2   , "A" , "2000-04-01 00:00:00" |
      3   , "D" , "2014-04-07 00:00:00" |
      4   , "C" , "2012-12-01 00:00:00" |
      4   , "A" , "2005-06-16 00:00:00" |
      4   , "D" , "2009-01-20 00:00:00" |
      4   , "B" , "2009-01-20 00:00:00" |
      5   , "A" , "2010-10-10 00:00:00" |
      5   , "B" , "2003-11-09 00:00:00" |
      6   , "B" , "2004-01-09 00:00:00" )

  # draw a picture of the record format
  diagram <- wrapr::build_frame(
    "rank", "DATE", "OP"  |
      "1", "DATE1", "OP1" |
      "2", "DATE2", "OP2" |
      "3", "DATE3", "OP3" )

  transform <- blocks_to_rowrecs_spec(
    controlTable = diagram,
    recordKeys = 'ID')

  # get the time based ranking, and grouped ops
  # and then transform
  ops <- local_td(d) %.>%
    extend(.,
           rank %:=% row_number(),
           partitionby = "ID",
           orderby = c("DATE", "OP")) %.>%
    transform %.>%
    orderby(.,
            'ID')

  res <- d %.>% ops

  expect <- wrapr::build_frame(
    "ID"  , "DATE1"              , "OP1", "DATE2"              , "OP2"        , "DATE3"              , "OP3"         |
      1   , "2001-01-02 00:00:00", "A"  , "2015-04-25 00:00:00", "B"          , NA_character_        , NA_character_ |
      2   , "2000-04-01 00:00:00", "A"  , NA_character_        , NA_character_, NA_character_        , NA_character_ |
      3   , "2014-04-07 00:00:00", "D"  , NA_character_        , NA_character_, NA_character_        , NA_character_ |
      4   , "2005-06-16 00:00:00", "A"  , "2009-01-20 00:00:00", "B"          , "2009-01-20 00:00:00", "D"           |
      5   , "2003-11-09 00:00:00", "B"  , "2010-10-10 00:00:00", "A"          , NA_character_        , NA_character_ |
      6   , "2004-01-09 00:00:00", "B"  , NA_character_        , NA_character_, NA_character_        , NA_character_ )

  RUnit::checkTrue(wrapr::check_equiv_frames(res, expect))

  db_info <- rquery::rquery_default_db_info()
  sql <- to_sql(ops, db_info)

  if(requireNamespace('DBI', quietly = TRUE) &&
     requireNamespace('RSQLite', quietly = TRUE)) {
    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::initExtension(raw_connection)
    db <- rquery_db_info(
      connection = raw_connection,
      is_dbi = TRUE,
      connection_options = rq_connection_tests(raw_connection))
    rquery::rq_copy_to(db, table_name = 'd', d)
    rquery::materialize(db, ops, table_name = 'res')
    res_db <- DBI::dbReadTable(raw_connection, 'res')
    DBI::dbDisconnect(raw_connection)
    RUnit::checkTrue(wrapr::check_equiv_frames(res_db, expect))
  }

  invisible(NULL)
}

test_complex_example_list <- function() {
  # https://community.rstudio.com/t/tidying-data-reorganizing-tibble/48292/

  # example data
  d <- wrapr::build_frame(
    "ID"  , "OP", "DATE"                |
      1   , "A" , "2001-01-02 00:00:00" |
      1   , "B" , "2015-04-25 00:00:00" |
      2   , "A" , "2000-04-01 00:00:00" |
      3   , "D" , "2014-04-07 00:00:00" |
      4   , "C" , "2012-12-01 00:00:00" |
      4   , "A" , "2005-06-16 00:00:00" |
      4   , "D" , "2009-01-20 00:00:00" |
      4   , "B" , "2009-01-20 00:00:00" |
      5   , "A" , "2010-10-10 00:00:00" |
      5   , "B" , "2003-11-09 00:00:00" |
      6   , "B" , "2004-01-09 00:00:00" )

  # draw a picture of the record format
  diagram <- wrapr::build_frame(
    "rank", "DATE", "OP"  |
      "1", "DATE1", "OP1" |
      "2", "DATE2", "OP2" |
      "3", "DATE3", "OP3" )

  transform <- blocks_to_rowrecs_spec(
    controlTable = diagram,
    recordKeys = 'ID')

  # get the time based ranking, and grouped ops
  # and then transform
  ops <- local_td(d) %.>%
    project(.,
            OP %:=% list(sort(unique(OP))),
            groupby = c("ID", "DATE")) %.>%
    extend(.,
           rank %:=% row_number(),
           partitionby = "ID",
           orderby = "DATE") %.>%
    transform %.>%
    orderby(.,
            'ID')

  res <- d %.>% ops

  expect <-
    structure(
      list(
        ID = c(1, 2, 3, 4, 5, 6),
        DATE1 = c(
          "2001-01-02 00:00:00",
          "2000-04-01 00:00:00",
          "2014-04-07 00:00:00",
          "2005-06-16 00:00:00",
          "2003-11-09 00:00:00",
          "2004-01-09 00:00:00"
        ),
        OP1 = list("A",
                   "A", "D", "A", "B", "B"),
        DATE2 = c(
          "2015-04-25 00:00:00",
          NA,
          NA,
          "2009-01-20 00:00:00",
          "2010-10-10 00:00:00",
          NA
        ),
        OP2 = list(
          "B",
          NA_character_,
          NA_character_,
          c("B", "D"),
          "A",
          NA_character_
        ),
        DATE3 = c(NA, NA, NA, "2012-12-01 00:00:00", NA, NA),
        OP3 = list(
          NA_character_,
          NA_character_,
          NA_character_,
          "C",
          NA_character_,
          NA_character_
        )
      ),
      row.names = c(NA,-6L),
      class = "data.frame"
    )

  RUnit::checkTrue(wrapr::check_equiv_frames(res, expect))

  invisible(NULL)
}

