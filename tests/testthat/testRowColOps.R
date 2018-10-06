library('cdata')

context("rowColOps")

test_that("testRowColOps.R", {
    # some small corner cases

    d <- data.frame(AUC = 0.6, R2 = 0.2)
    res <- unpivot_to_blocks(d,
                             nameForNewKeyColumn = 'meas',
                             nameForNewValueColumn = 'val',
                             columnsToTakeFrom = c('AUC', 'R2'))
    res <- res[order(res$meas), , drop=FALSE]
    ex <- data.frame(meas = c('AUC', 'R2'),
                     val = c(0.6, 0.2),
                     stringsAsFactors = FALSE)
    expect_equivalent(ex,
                      res[, colnames(ex), drop = FALSE])

    d <- data.frame(meas = c('AUC', 'R2'),
                    val = c(0.6, 0.2))
    res <- pivot_to_rowrecs(d,
                            columnToTakeKeysFrom = 'meas',
                            columnToTakeValuesFrom = 'val',
                            rowKeyColumns = c())
    ex <- data.frame(AUC = 0.6,
                     R2 = 0.2,
                     stringsAsFactors = FALSE)
    expect_equivalent(ex,
                      res[, colnames(ex), drop = FALSE])

    d <- data.frame(key = c('a', 'a'),
                    meas = c('AUC', 'R2'),
                    val = c(0.6, 0.2),
                    stringsAsFactors = FALSE)
    res <- pivot_to_rowrecs(d,
                            columnToTakeKeysFrom = 'meas',
                            columnToTakeValuesFrom = 'val',
                            rowKeyColumns = c('key'))
    ex <- data.frame(key = 'a',
                     AUC = 0.6,
                     R2 = 0.2,
                     stringsAsFactors = FALSE)
    expect_equivalent(ex,
                      res[, colnames(ex), drop = FALSE])
})