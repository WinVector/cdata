library('cdata')

context("xform")

test_that("testXForm.R", {
  # Example from: http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html

  # non-trivial control table
  controlTable = build_frame(
    "measure"                     , "training", "validation" |
    "minus binary cross entropy",   "loss"    , "val_loss"   |
    "accuracy"                  ,   "acc"     , "val_acc"    )

  # operating on self
  rowrec <- blocks_to_rowrecs(controlTable, NULL, controlTable)
  longform = build_frame(
    "loss", "acc", "val_loss", "val_acc" |
    "loss", "acc", "val_loss", "val_acc" )
  testthat::expect_equal(sort(colnames(longform)), sort(colnames(rowrec)))
  testthat::expect_equal(longform, rowrec[, colnames(longform), drop=FALSE])

  blockrec <- rowrecs_to_blocks(rowrec, controlTable)
  blockrec <- blockrec[order(blockrec$measure, decreasing = TRUE), , drop = FALSE]
  rownames(blockrec) <- NULL
  testthat::expect_equal(sort(colnames(controlTable)), sort(colnames(blockrec)))
  testthat::expect_equal(controlTable, blockrec[ , colnames(controlTable), drop=FALSE])

  # operating on example data
  dOrig <- build_frame(
      "val_loss", "val_acc", "loss"  , "acc"  |
      0.377     , 0.8722   , 0.5067  , 0.7852 |
      0.2997    , 0.8895   , 0.3002  , 0.904  |
      0.2964    , 0.8822   , 0.2166  , 0.9303 |
      0.2779    , 0.8899   , 0.1739  , 0.9428 |
      0.2843    , 0.8861   , 0.1411  , 0.9545 |
      0.312     , 0.8817   , 0.1136  , 0.9656 |
      0.3138    , 0.8832   , 0.09555 , 0.9721 |
      0.379     , 0.8694   , 0.07964 , 0.9767 |
      0.3669    , 0.876    , 0.0649  , 0.9827 |
      0.3873    , 0.8751   , 0.05509 , 0.985  |
      0.4221    , 0.8756   , 0.04285 , 0.9901 |
      0.4546    , 0.8694   , 0.03744 , 0.9912 |
      0.4753    , 0.8739   , 0.02861 , 0.9939 |
      0.5059    , 0.8717   , 0.02283 , 0.9957 |
      0.538     , 0.8682   , 0.01685 , 0.9981 |
      0.5863    , 0.8651   , 0.01457 , 0.9981 |
      0.6224    , 0.8628   , 0.01138 , 0.9986 |
      0.6437    , 0.8653   , 0.00973 , 0.9987 |
      0.6762    , 0.8654   , 0.009653, 0.998  |
      0.696     , 0.8649   , 0.003934, 0.9999 )
  dReady <- build_frame(
     "epoch", "measure"                   , "training", "validation" |
      1L    , "minus binary cross entropy", 0.5067    , 0.377        |
      1L    , "accuracy"                  , 0.7852    , 0.8722       |
      2L    , "minus binary cross entropy", 0.3002    , 0.2997       |
      2L    , "accuracy"                  , 0.904     , 0.8895       |
      3L    , "minus binary cross entropy", 0.2166    , 0.2964       |
      3L    , "accuracy"                  , 0.9303    , 0.8822       |
      4L    , "minus binary cross entropy", 0.1739    , 0.2779       |
      4L    , "accuracy"                  , 0.9428    , 0.8899       |
      5L    , "minus binary cross entropy", 0.1411    , 0.2843       |
      5L    , "accuracy"                  , 0.9545    , 0.8861       |
      6L    , "minus binary cross entropy", 0.1136    , 0.312        |
      6L    , "accuracy"                  , 0.9656    , 0.8817       |
      7L    , "minus binary cross entropy", 0.09555   , 0.3138       |
      7L    , "accuracy"                  , 0.9721    , 0.8832       |
      8L    , "minus binary cross entropy", 0.07964   , 0.379        |
      8L    , "accuracy"                  , 0.9767    , 0.8694       |
      9L    , "minus binary cross entropy", 0.0649    , 0.3669       |
      9L    , "accuracy"                  , 0.9827    , 0.876        |
      10L    , "minus binary cross entropy", 0.05509   , 0.3873       |
      10L    , "accuracy"                  , 0.985     , 0.8751       |
      11L    , "minus binary cross entropy", 0.04285   , 0.4221       |
      11L    , "accuracy"                  , 0.9901    , 0.8756       |
      12L    , "minus binary cross entropy", 0.03744   , 0.4546       |
      12L    , "accuracy"                  , 0.9912    , 0.8694       |
      13L    , "minus binary cross entropy", 0.02861   , 0.4753       |
      13L    , "accuracy"                  , 0.9939    , 0.8739       |
      14L    , "minus binary cross entropy", 0.02283   , 0.5059       |
      14L    , "accuracy"                  , 0.9957    , 0.8717       |
      15L    , "minus binary cross entropy", 0.01685   , 0.538        |
      15L    , "accuracy"                  , 0.9981    , 0.8682       |
      16L    , "minus binary cross entropy", 0.01457   , 0.5863       |
      16L    , "accuracy"                  , 0.9981    , 0.8651       |
      17L    , "minus binary cross entropy", 0.01138   , 0.6224       |
      17L    , "accuracy"                  , 0.9986    , 0.8628       |
      18L    , "minus binary cross entropy", 0.00973   , 0.6437       |
      18L    , "accuracy"                  , 0.9987    , 0.8653       |
      19L    , "minus binary cross entropy", 0.009653  , 0.6762       |
      19L    , "accuracy"                  , 0.998     , 0.8654       |
      20L    , "minus binary cross entropy", 0.003934  , 0.696        |
      20L    , "accuracy"                  , 0.9999    , 0.8649       )
  dReady <- dReady[order(dReady$epoch, dReady$measure), , drop = FALSE]
  rownames(dReady) <- NULL
  dOrig$epoch <- seq_len(nrow(dOrig))
  dBlocks <- rowrecs_to_blocks(dOrig, controlTable, columnsToCopy = 'epoch')
  dBlocks <- dBlocks[order(dBlocks$epoch, dBlocks$measure), , drop = FALSE]
  dBlocks <- dBlocks[, colnames(dReady), drop = FALSE]
  rownames(dBlocks) <- NULL
  testthat::expect_equal(dReady, dBlocks)
  dBack <- blocks_to_rowrecs(dBlocks, "epoch", controlTable)
  dBack <- dBack[order(dBack$epoch), , drop = FALSE]
  dBack <- dBack[, colnames(dOrig), drop = FALSE]
  expect_equal(dOrig, dBack)

  # same tests on db path
  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbWriteTable(my_db,
                      'dOrig',
                      dOrig,
                      temporary = TRUE)
    DBI::dbWriteTable(my_db,
                      'dReady',
                      dReady,
                      temporary = TRUE)

    tab1_name <- rowrecs_to_blocks_q('dOrig',
                                controlTable,
                                my_db,
                                columnsToCopy = "epoch")
    tab1 <- DBI::dbGetQuery(my_db, paste("SELECT * FROM",
                                         tab1_name))
    tab1 <- tab1[order(tab1$epoch, tab1$measure), , drop = FALSE]
    tab1 <- tab1[, colnames(dReady), drop = FALSE]
    rownames(tab1) <- NULL
    expect_equal(dReady, tab1)

    tab2_name <- blocks_to_rowrecs_q('dReady',
                               keyColumns = "epoch",
                               controlTable = controlTable,
                               my_db = my_db)
    tab2 <- DBI::dbGetQuery(my_db, paste("SELECT * FROM",
                                         tab2_name))
    tab2 <- tab2[order(tab2$epoch), , drop = FALSE]
    tab2 <- tab2[, colnames(dOrig), drop = FALSE]
    rownames(tab2) <- NULL
    expect_equal(dOrig, tab2)

    DBI::dbDisconnect(my_db)
  }


})
