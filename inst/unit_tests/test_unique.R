
test_unique <- function() {
  d <- data.frame(x = c(1, 1, 2), y = c(1, 2, 1))
  RUnit::checkTrue(!check_cols_form_unique_keys(d, "x"))
  RUnit::checkTrue(!check_cols_form_unique_keys(d, "y"))
  RUnit::checkTrue(check_cols_form_unique_keys(d, c("x", "y")))
  invisible(NULL)
}