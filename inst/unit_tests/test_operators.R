
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

  transform <- rowrecs_to_blocks_spec(
    wrapr::qchar_frame(
      measure, value |
        AUC    , "AUC" |
        R2     , "R2"  ),
    recordKeys = "model_id")

  d %//% t(transform) -> r
  RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

  d %//% t(transform) %**% transform -> r
  RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

  r <- rowrecs_to_blocks(d2,
                         controlTable = transform$controlTable,
                         controlTableKeys = transform$controlTableKeys,
                         columnsToCopy = transform$recordKeys,
                         checkNames = TRUE,
                         strict = TRUE,
                         checkKeys = TRUE)
  RUnit::checkTrue(wrapr::check_equiv_frames(d, r))
  r <- blocks_to_rowrecs(d,
                         keyColumns = transform$recordKeys,
                         controlTable = transform$controlTable,
                         controlTableKeys = transform$controlTableKeys,
                         checkNames = TRUE,
                         strict = TRUE,
                         checkKeys = TRUE)
  RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

  if(requireNamespace("DBI", quietly = TRUE) ||
     requireNamespace("RSQLite", quietly = TRUE)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:")

    d_db <- rquery::rq_copy_to(my_db, "d_db", d,
                               temporary = TRUE, overwrite = TRUE)
    d2_db <- rquery::rq_copy_to(my_db, "d2_db", d2,
                               temporary = TRUE, overwrite = TRUE)

    d_db %//% t(transform) %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    d_db %//% t(transform) %**% transform %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

    d_db %.>% .(t(transform)) %.>% rquery::execute(my_db, .) -> r
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    r <- rowrecs_to_blocks(d2_db,
                           controlTable = transform$controlTable,
                           controlTableKeys = transform$controlTableKeys,
                           columnsToCopy = transform$recordKeys,
                           checkNames = TRUE,
                           strict = TRUE,
                           checkKeys = TRUE) %.>% rquery::execute(my_db, .)
    RUnit::checkTrue(wrapr::check_equiv_frames(d, r))

    r <- blocks_to_rowrecs(d_db,
                           keyColumns = transform$recordKeys,
                           controlTable = transform$controlTable,
                           controlTableKeys = transform$controlTableKeys,
                           checkNames = TRUE,
                           strict = TRUE,
                           checkKeys = TRUE) %.>% rquery::execute(my_db, .)
    RUnit::checkTrue(wrapr::check_equiv_frames(d2, r))

    DBI::dbDisconnect(my_db)
  }
  invisible(NULL)
}