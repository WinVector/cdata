library('cdata')

context("rowColOps")

test_that("testRowColOps.R", {
  # some small corner cases

  d <- data.frame(AUC= 0.6, R2= 0.2)
  moveValuesToRows(d,
                   nameForNewKeyColumn= 'meas',
                   nameForNewValueColumn= 'val',
                   columnsToTakeFrom= c('AUC', 'R2'))

  d <- data.frame(meas= c('AUC', 'R2'),
                  val= c(0.6, 0.2))
  moveValuesToColumns(d,
                      columnToTakeKeysFrom= 'meas',
                      columnToTakeValuesFrom= 'val',
                      rowKeyColumns= c())

  d <- data.frame(key = c('a', 'a'),
                  meas= c('AUC', 'R2'),
                  val= c(0.6, 0.2))
  moveValuesToColumns(d,
                      columnToTakeKeysFrom= 'meas',
                      columnToTakeValuesFrom= 'val',
                      rowKeyColumns= c('key'))
})