
test_there_and_back_db <- function() {

  if((!requireNamespace("DBI", quietly = TRUE)) ||
     (!requireNamespace("RSQLite", quietly = TRUE))) {
    return(invisible(NULL))
  }

  raw_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                   ":memory:")
  RSQLite::initExtension(raw_connection)
  dbopts <- rquery::rq_connection_tests(raw_connection)
  db <- rquery::rquery_db_info(connection = raw_connection,
                               is_dbi = TRUE,
                               connection_options = dbopts)

  d <- wrapr::build_frame(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "id" |
    5.1           , 3.5          , 1.4           , 0.2          , "setosa" , 1L   |
    4.9           , 3            , 1.4           , 0.2          , "setosa" , 2L   )
  d_db <- rquery::rq_copy_to(db, "d", d,
                          temporary = TRUE,
                          overwrite = TRUE)

  control_table <- wrapr::qchar_frame(
    Measure,       Value        |
    Sepal.Length,  Sepal.Length |
    Sepal.Width,   Sepal.Width  |
    Petal.Length,  Petal.Length |
    Petal.Width,   Petal.Width  )

  res_db <- rowrecs_to_blocks(d_db,
                              control_table,
                              columnsToCopy = c("id", "Species"))
  res_t <- rquery::materialize(db, res_db)
  res_sql <- rquery::to_sql(res_db, db)
  res <- rquery::execute(db, res_t)

  expect <- wrapr::build_frame(
    "id", "Species", "Measure"     , "Value" |
    1L  , "setosa" , "Sepal.Length", 5.1     |
    1L  , "setosa" , "Sepal.Width" , 3.5     |
    1L  , "setosa" , "Petal.Length", 1.4     |
    1L  , "setosa" , "Petal.Width" , 0.2     |
    2L  , "setosa" , "Sepal.Length", 4.9     |
    2L  , "setosa" , "Sepal.Width" , 3       |
    2L  , "setosa" , "Petal.Length", 1.4     |
    2L  , "setosa" , "Petal.Width" , 0.2     )

  expect_equal(sort(colnames(expect)), sort(colnames(res)))
  res <- res[, colnames(expect), drop = FALSE]

  expect <- expect[wrapr::orderv(expect[, c("id", "Species", "Measure")]), , drop = FALSE]
  res <- res[wrapr::orderv(res[, c("id", "Species", "Measure")]), , drop = FALSE]
  expect_equal(expect, res)

  back_db <- blocks_to_rowrecs(res_db,
                            keyColumns = c("id", "Species"),
                            control_table)
  back_t <- rquery::materialize(db, back_db)
  back_sql <- rquery::to_sql(back_db, db)
  back <- rquery::execute(db, back_t)

  expect_equal(sort(colnames(d)), sort(colnames(back)))
  back <- back[, colnames(d), drop = FALSE]

  d <- d[wrapr::orderv(d), , drop = FALSE]
  back <- back[wrapr::orderv(back), , drop = FALSE]
  expect_equal(d, back)

  DBI::dbDisconnect(raw_connection)
}

test_there_and_back_db()


