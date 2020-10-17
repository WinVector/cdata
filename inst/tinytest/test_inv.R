
test_inv <- function() {

  # get the data
  dir <- system.file("tinytest", package = "cdata", mustWork = TRUE)
  iris <- readRDS(paste(dir, "iris.RDS", sep = "/"))

  # get into the form we want
  iris$iris_id <- seq_len(nrow(iris))
  iris$Species <- as.character(iris$Species)

  # declare our columns of interest
  meas_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  category_variable <- "Species"

  # build a control with all pairs of variables as value columns
  # and pair_key as the key column
  controlTable <- data.frame(expand.grid(meas_vars, meas_vars,
                                         stringsAsFactors = FALSE))
  # one copy of columns is coordinate names second copy is values
  controlTable <- cbind(controlTable, controlTable)
  # name the value columns value1 and value2
  colnames(controlTable) <- c("v1", "v2", "value1", "value2")
  transform <- rowrecs_to_blocks_spec(
    controlTable,
    recordKeys = c("iris_id", "Species"),
    controlTableKeys = c("v1", "v2"),
    checkKeys = FALSE)

  # do the unpivot to convert the row records to multiple block records
  iris_aug <- iris %.>% transform

  # try inv
  inv <- t(transform)
  iris_back <- iris_aug %.>% inv

  expect_true(wrapr::check_equiv_frames(iris, iris_back))

  if(requireNamespace("DBI", quietly = TRUE) &&
     requireNamespace("RSQLite", quietly = TRUE)) {
    db_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                    ":memory:")
    db <- rquery::rquery_db_info(
      connection = db_connection,
      is_dbi = TRUE,
      connection_options = rquery::rq_connection_tests(db_connection))
    iris_db <- rquery::rq_copy_to(db, "iris_db", iris,
                                  temporary = TRUE, overwrite = TRUE)

    iris_aug_db_res <- iris_db %.>%
      transform %.>%
      rquery::execute(db, .)
    expect_true(wrapr::check_equiv_frames(iris_aug, iris_aug_db_res))

    iris_aug_db <- rquery::rq_copy_to(db, "iris_aug", iris_aug,
                                      temporary = TRUE, overwrite = TRUE)
    iris_back_db_res <- iris_aug_db %.>%
      inv %.>%
      rquery::execute(db, .)
    expect_true(wrapr::check_equiv_frames(iris_back, iris_back_db_res))

    DBI::dbDisconnect(db_connection)
  }

  invisible(NULL)
}

test_inv()
