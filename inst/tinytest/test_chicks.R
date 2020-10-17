
test_chicks <- function() {
  dir <- system.file("tinytest", package = "cdata", mustWork = TRUE)
  lst <- readRDS(paste(dir, "chickw.RDS", sep = "/"))
  ChickWeight <- lst$ChickWeight
  ChickWeight_wide <- lst$ChickWeight_wide

  ChickWeight_wide2 <- pivot_to_rowrecs(
    ChickWeight,
    columnToTakeKeysFrom = "Time",
    columnToTakeValuesFrom = "weight",
    rowKeyColumns = "Chick")
  ChickWeight_wide2 <- ChickWeight_wide2[order(ChickWeight_wide2$Chick), , drop = FALSE]
  row.names(ChickWeight_wide2) <- NULL
  expect_true(isTRUE(all.equal(ChickWeight_wide, ChickWeight_wide2)))

  invisible(NULL)
}

test_chicks()
