
<!-- README.md is generated from README.Rmd. Please edit that file -->
The `cdata` package is a demonstration of the ["coordinatized data" theory](http://winvector.github.io/FluidData/RowsAndColumns.html) and includes an implementation of the ["fluid data" methodology](http://winvector.github.io/FluidData/FluidData.html). The recommended tutorial is: [Fluid data reshaping with cdata](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html). Another example can be found [here](http://winvector.github.io/FluidData/DataWranglingAtScale.html).

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
cT <- build_pivot_control_q('d',
                              columnToTakeKeysFrom= 'meas',
                              columnToTakeValuesFrom= 'val',
                              my_db = my_db)
tab <- blocks_to_rowrecs_q('d',
                            keyColumns = NULL,
                            controlTable = cT,
                            my_db = my_db)
qlook(my_db, tab)
```

    ## table `mvtcq_zg7rrsptqga0pcrunpx9_0000000001` SQLiteConnection 
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

Note: `cdata` is targeted at data with "tame column names" (column names that are valid both in databases, and as `R` unquoted variable names) and basic types (column values that are simple `R` types such as `character`, `numeric`, `logical`, and so on).
