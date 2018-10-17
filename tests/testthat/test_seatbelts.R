library('cdata')

context("seatbelts")

test_that("test_seatbelts.R", {
  lst <- readRDS("seatbelts.RDS")
  seatbelts <- lst$seatbelts
  seatbelts_long <- lst$seatbelts_long

  seatbelts_long2 <- unpivot_to_blocks(
    seatbelts,
    nameForNewKeyColumn = "victim_type",
    nameForNewValueColumn = "nvictims",
    columnsToTakeFrom = c("DriversKilled", "front", "rear"))
  seatbelts_long2 <- seatbelts_long2[wrapr::orderv(seatbelts_long2), , drop = FALSE]
  row.names(seatbelts_long2) <- NULL
  testthat::expect(isTRUE(all.equal(seatbelts_long, seatbelts_long2)), failure_message = "mismatch")
})
