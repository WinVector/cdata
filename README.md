
<!-- README.md is generated from README.Rmd. Please edit that file -->
`cdata` is a running explanation of the ["coordinatized data" theory](https://github.com/WinVector/cdata/blob/master/extras/RowsAndColumns.md) and includes an implementation of the ["fluid data" methodology](https://github.com/WinVector/cdata/blob/master/extras/FluidData.md).

![](https://raw.githubusercontent.com/WinVector/cdata/master/tools/cdata.png)

Briefly `cdata` supplies data transform operators that:

-   Work on local data or any `DBI` data source.
-   Are powerful generalizations of the operators commonly called `pivot` and `un-pivot`.

A quick example:

``` r
library("cdata")
```

    ## Loading required package: wrapr

``` r
my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# pivot example
d <- data.frame(meas = c('AUC', 'R2'), val = c(0.6, 0.2))
DBI::dbWriteTable(my_db,
                  'd',
                  d,
                  temporary = TRUE)
qlook(my_db, 'd')
```

    ## table `d` SQLiteConnection 
    ##  nrow: 2 
    ## 'data.frame':    2 obs. of  2 variables:
    ##  $ meas: chr  "AUC" "R2"
    ##  $ val : num  0.6 0.2

``` r
cT <- buildPivotControlTableN('d',
                              columnToTakeKeysFrom= 'meas',
                              columnToTakeValuesFrom= 'val',
                              my_db = my_db)
tab <- moveValuesToColumnsN('d',
                            keyColumns = NULL,
                            controlTable = cT,
                            my_db = my_db)
qlook(my_db, tab)
```

    ## table `mvtcq_lchtpcfpsvycaafc9zlu_0000000001` SQLiteConnection 
    ##  nrow: 1 
    ## 'data.frame':    1 obs. of  2 variables:
    ##  $ AUC: num 0.6
    ##  $ R2 : num 0.2

``` r
DBI::dbDisconnect(my_db)
```

Install via CRAN:

``` r
install.packages("cdata")
```

Or from Github using devtools:

``` r
devtools::install_github("WinVector/cdata")
```

Note: `cdata` is targeted at data with "tame column names", that is column names that are valid both in databases, and as `R` variable names.
