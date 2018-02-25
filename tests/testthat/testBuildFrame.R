library('cdata')

context("buildFrame")

test_that("testBuildFrame.R", {
  testBFRT <- function(d) {
    txt <- draw_frame(d)
    d2 <- eval(parse(text = txt))
    testthat::expect_equivalent(d, d2)
  }

  d <- data.frame(
    measure = c("minus binary cross entropy", "accuracy"),
    training = c(5, 0.8),
    validataion = c(-7, 0.6),
    stringsAsFactors = FALSE)
  testBFRT(d)

  d <- data.frame(x = c(-1, 2))
  testBFRT(d)

  d <- data.frame(x = 1)
  testBFRT(d)

  d1 <- qchar_frame(
    measure,                      training, validation |
    "minus binary cross entropy", loss,     val_loss   |
    accuracy,                     acc,      val_acc    )
  d2 <- data.frame(
    measure = c("minus binary cross entropy", "accuracy"),
    training = c("loss", "acc"),
    validataion = c("val_loss", "val_acc"),
    stringsAsFactors = FALSE)
  testthat::expect_equivalent(d1, d2)

  d1 <- qchar_frame(
    x |
    1 |
    2 )
  d2 <- data.frame(
    x = c("1", "2"),
    stringsAsFactors = FALSE)
  testthat::expect_equivalent(d1, d2)
})