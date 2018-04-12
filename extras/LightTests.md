LightTests
================

``` r
library("cdata")
```

    ## Loading required package: wrapr

``` r
runTests <- function() {
  d <- build_frame(
   "AUC", "R2" |
   0.6  , 0.2  )
  d2 <- build_frame(
   "meas", "val" |
   "AUC" , 0.6   |
   "R2"  , 0.2   )
  res1 <- unpivot_to_blocks(d,
                           nameForNewKeyColumn = 'meas',
                           nameForNewValueColumn = 'val',
                           columnsToTakeFrom = c('AUC', 'R2'))
  res1 <- res1[order(res1$meas), , drop=FALSE]
  testthat::expect_equivalent(d2,
                              res1)
  
  res2 <- pivot_to_rowrecs(d2,
                          columnToTakeKeysFrom = 'meas',
                          columnToTakeValuesFrom = 'val',
                          rowKeyColumns = c())
  res2 <- res2[, c("AUC", "R2"), drop = FALSE]
  testthat::expect_equivalent(d,
                              res2)
  
  d3 <- build_frame(
   "key", "meas", "val" |
   "a"  , "AUC" , 0.6   |
   "a"  , "R2"  , 0.2   )
  d4 <- build_frame(
   "key", "AUC", "R2" |
   "a"  , 0.6  , 0.2  )
  res3 <- pivot_to_rowrecs(d3,
                          columnToTakeKeysFrom = 'meas',
                          columnToTakeValuesFrom = 'val',
                          rowKeyColumns = c('key'))
  res3 <- res3[, c("key", "AUC", "R2"), drop = FALSE]
  testthat::expect_equivalent(d4,
                              res3)
}

runTests()

db <- DBI::dbConnect(RSQLite::SQLite(), 
                     ":memory:")
winvector_temp_db_handle <- list(db = db)
runTests()
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)


winvector_temp_db_handle <- NULL
runTests()


db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')
winvector_temp_db_handle <- list(db = db)
runTests()
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)

db <- sparklyr::spark_connect(version='2.2.0', 
                                   master = "local")
winvector_temp_db_handle <- list(db = db)
runTests()
winvector_temp_db_handle <- NULL
sparklyr::spark_disconnect(db)
```
