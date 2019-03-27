
test_operators <- function() {
  d <- wrapr::build_frame(
    "model_id", "measure", "value" |
      1   , "AUC"    , 0.7     |
      1   , "R2"     , 0.4     |
      2   , "AUC"    , 0.8     |
      2   , "R2"     , 0.5     )

  d2 <- wrapr::build_frame(
    "model_id", "AUC", "R2" |
      1         , 0.7  , 0.4  |
      2         , 0.8  , 0.5  )

  record_spec <- new_record_spec(
    wrapr::qchar_frame(
      measure, value |
        AUC    , "AUC" |
        R2     , "R2"  ),
    recordKeys = "model_id")

  if(requireNamespace("DBI", quietly = TRUE) ||
     requireNamespace("RSQLite", quietly = TRUE)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:")

    d_db <- rquery::rq_copy_to(my_db, "d_db", d,
                               temporary = TRUE, overwrite = TRUE)

    d %//% record_spec -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    d %//% record_spec %**% record_spec -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

    d_db %//% record_spec %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    d_db %//% record_spec %**% record_spec %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

    record_spec %pivot% d_db %pivot% record_spec %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

    d_db %.>% .(t(record_spec)) %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    DBI::dbDisconnect(my_db)
  }
  invisible(NULL)
}