
test_unique <- function() {
  d <- data.frame(x = c(1, 1, 2), y = c(1, 2, 1))
  expect_true(!check_cols_form_unique_keys(d, "x"))
  expect_true(!check_cols_form_unique_keys(d, "y"))
  expect_true(check_cols_form_unique_keys(d, c("x", "y")))
  invisible(NULL)
}

test_unique()
