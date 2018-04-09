library('cdata')

context("xform")

test_that("testXForm.R", {
  controlTable = build_frame(
    "measure"                     , "training", "validation" |
    "minus binary cross entropy",   "loss"    , "val_loss"   |
    "accuracy"                  ,   "acc"     , "val_acc"    )

  rowrec <- blocks_to_rowrecs(controlTable, NULL, controlTable)
  longform = build_frame(
    "loss", "acc", "val_loss", "val_acc" |
    "loss", "acc", "val_loss", "val_acc" )
  expect_equal(longform, rowrec)

  blockrec <- rowrecs_to_blocks(rowrec, controlTable)
  blockrec <- blockrec[order(blockrec$measure, decreasing = TRUE), , drop = FALSE]

  expect_equal(controlTable, blockrec)

})
