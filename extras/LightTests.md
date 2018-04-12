LightTests
================

``` r
library("cdata")
```

    ## Loading required package: wrapr

``` r
runTests <- function(db) {
  d <- build_frame(
    "AUC", "R2" |
      0.6  , 0.2  )
  d2 <- build_frame(
    "meas", "val" |
      "AUC" , 0.6   |
      "R2"  , 0.2   )
  d3 <- build_frame(
    "key", "meas", "val" |
      "a"  , "AUC" , 0.6   |
      "a"  , "R2"  , 0.2   )
  d4 <- build_frame(
    "key", "AUC", "R2" |
      "a"  , 0.6  , 0.2  )
  cT <- build_frame(
    "meas", "val" |
      "AUC" , "AUC" |
      "R2"  , "R2"  )
  
  if(!is.null(db)) {
    for(tabname in c("d", "d2", "d3")) {
      tryCatch(
        DBI::dbExecute(db, paste("DROP TABLE", tabname)),
        error = function(e) { e })
    }
    DBI::dbWriteTable(db, "d", d)
    DBI::dbWriteTable(db, "d2", d2)
    DBI::dbWriteTable(db, "d3", d3)
  }
  
  if(!is.null(db)) {
    res1_n <- rowrecs_to_blocks_q("d", cT, my_db = db)
    res1 <- DBI::dbGetQuery(db, paste("SELECT * FROM", res1_n))
  } else {
    res1 <- rowrecs_to_blocks(d, cT)
  }
  res1 <- res1[order(res1$meas), , drop=FALSE]
  testthat::expect_equivalent(d2,
                              res1)
  
  if(!is.null(db)) {
    res2_n <- blocks_to_rowrecs_q("d2",
                                  keyColumns = NULL,
                                  cT,
                                  my_db = db)
    res2 <- DBI::dbGetQuery(db, paste("SELECT * FROM", res2_n))
  } else {
    res2 <- blocks_to_rowrecs(d2,
                              keyColumns = NULL,
                              cT)
  }
  res2 <- res2[, c("AUC", "R2"), drop = FALSE]
  testthat::expect_equivalent(d,
                              res2)
  
  if(!is.null(db)) {
    res3_n <- blocks_to_rowrecs_q("d3",
                                  keyColumns = "key",
                                  cT,
                                  my_db = db)
    res3 <- DBI::dbGetQuery(db, paste("SELECT * FROM", res3_n))
  } else {
    res3 <- blocks_to_rowrecs(d3,
                              keyColumns = "key",
                              cT)
  }
  res3 <- res3[, c("key", "AUC", "R2"), drop = FALSE]
  testthat::expect_equivalent(d4,
                              res3)
  if(!is.null(db)) {
    for(tabname in c("d", "d2", "d3",
                     res1_n, res2_n, res3_n)) {
      tryCatch(
        DBI::dbExecute(db, paste("DROP TABLE", tabname)),
        error = function(e) { e })
    }
  }
  TRUE
}

runTests(NULL)
```

    ## [1] TRUE

``` r
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     ":memory:")
runTests(db)
```

    ## [1] TRUE

``` r
DBI::dbDisconnect(db)



db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')
runTests(db)
```

    ## [1] TRUE

``` r
DBI::dbDisconnect(db)

db <- sparklyr::spark_connect(version='2.2.0', 
                              master = "local")
runTests(db)
```

    ## [1] TRUE

``` r
sparklyr::spark_disconnect(db)
```
