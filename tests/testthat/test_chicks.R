library('cdata')

context("chicks")

test_that("test_chicks.R", {
  lst <- readRDS("chick.RDS")
  ChickWeight <- lst$ChickWeight
  ChickWeight_wide <- lst$ChickWeight_wide

  ChickWeight_wide2 <- pivot_to_rowrecs(
    ChickWeight,
    columnToTakeKeysFrom = "Time",
    columnToTakeValuesFrom = "weight",
    rowKeyColumns = "Chick")
  ChickWeight_wide2 <- ChickWeight_wide2[order(ChickWeight_wide2$Chick), , drop = FALSE]
  row.names(ChickWeight_wide2) <- NULL
  testthat::expect(isTRUE(all.equal(ChickWeight_wide, ChickWeight_wide2)), failure_message = "mismatch")
})
