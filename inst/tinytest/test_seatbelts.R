
test_seatbelts <- function() {
  dir <- system.file("tinytest", package = "cdata", mustWork = TRUE)
  lst <- readRDS(paste(dir, "seatbelts.RDS", sep = "/"))
  seatbelts <- lst$seatbelts
  seatbelts_long <- lst$seatbelts_long

  seatbelts_long2 <- unpivot_to_blocks(
    seatbelts,
    nameForNewKeyColumn = "victim_type",
    nameForNewValueColumn = "nvictims",
    columnsToTakeFrom = c("DriversKilled", "front", "rear"))
  seatbelts_long2 <- seatbelts_long2[wrapr::orderv(seatbelts_long2), , drop = FALSE]
  row.names(seatbelts_long2) <- NULL
  expect_true(isTRUE(all.equal(seatbelts_long, seatbelts_long2)))

  invisible(NULL)
}

test_seatbelts()

