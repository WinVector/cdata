
test_not_seen_level <- function() {
  # from https://community.rstudio.com/t/tidying-data-reorganizing-tibble/48292

  # # all the steps from the start
  # d <- wrapr::build_frame(
  #   "ID"  , "OP", "DATE"                |
  #     1   , "A" , "2001-01-02 00:00:00" |
  #     1   , "B" , "2015-04-25 00:00:00" |
  #     2   , "A" , "2000-04-01 00:00:00" |
  #     3   , "C" , "2014-04-07 00:00:00" |
  #     4   , "C" , "2012-12-01 00:00:00" |
  #     4   , "A" , "2005-06-16 00:00:00" |
  #     4   , "D" , "2009-01-20 00:00:00" |
  #     5   , "A" , "2010-10-10 00:00:00" |
  #     5   , "B" , "2003-11-09 00:00:00" |
  #     6   , "B" , "2004-01-09 00:00:00" )
  #
  # # get the time based ranking
  # d <- rquery::extend(d,
  #                     rank %:=% row_number(),
  #                     partitionby = "ID", orderby = "DATE")

  # skip to the after rquery step
  d <- wrapr::build_frame(
    "ID"  , "OP", "DATE"               , "rank" |
      1   , "A" , "2001-01-02 00:00:00", 1      |
      1   , "B" , "2015-04-25 00:00:00", 2      |
      2   , "A" , "2000-04-01 00:00:00", 1      |
      3   , "C" , "2014-04-07 00:00:00", 1      |
      4   , "A" , "2005-06-16 00:00:00", 1      |
      4   , "D" , "2009-01-20 00:00:00", 2      |
      4   , "C" , "2012-12-01 00:00:00", 3      |
      5   , "B" , "2003-11-09 00:00:00", 1      |
      5   , "A" , "2010-10-10 00:00:00", 2      |
      6   , "B" , "2004-01-09 00:00:00", 1      )

  diagram <- wrapr::build_frame(
    "rank", "DATE", "OP" |
      "1", "DATE1", "OP1" |
      "2", "DATE2", "OP2" |
      "3", "DATE3", "OP3" |
      "4", "DATE4", "OP4" |
      "5", "DATE5", "OP5" )


  res <- blocks_to_rowrecs(d, keyColumns = "ID", controlTable = diagram)

  expect <- wrapr::build_frame(
    "ID"  , "DATE1"              , "OP1", "DATE2"              , "OP2"        , "DATE3"              , "OP3"        , "DATE4"      , "OP4"        , "DATE5"      , "OP5"         |
      1   , "2001-01-02 00:00:00", "A"  , "2015-04-25 00:00:00", "B"          , NA_character_        , NA_character_, NA_character_, NA_character_, NA_character_, NA_character_ |
      2   , "2000-04-01 00:00:00", "A"  , NA_character_        , NA_character_, NA_character_        , NA_character_, NA_character_, NA_character_, NA_character_, NA_character_ |
      3   , "2014-04-07 00:00:00", "C"  , NA_character_        , NA_character_, NA_character_        , NA_character_, NA_character_, NA_character_, NA_character_, NA_character_ |
      4   , "2005-06-16 00:00:00", "A"  , "2009-01-20 00:00:00", "D"          , "2012-12-01 00:00:00", "C"          , NA_character_, NA_character_, NA_character_, NA_character_ |
      5   , "2003-11-09 00:00:00", "B"  , "2010-10-10 00:00:00", "A"          , NA_character_        , NA_character_, NA_character_, NA_character_, NA_character_, NA_character_ |
      6   , "2004-01-09 00:00:00", "B"  , NA_character_        , NA_character_, NA_character_        , NA_character_, NA_character_, NA_character_, NA_character_, NA_character_ )

  RUnit::checkTrue(wrapr::check_equiv_frames(res, expect))

  invisible(NULL)
}
