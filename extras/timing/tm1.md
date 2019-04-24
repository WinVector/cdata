tm1
================

From: <https://github.com/tidyverse/tidyr/issues/613>.

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ───────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.1       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

    ## ── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
require(data.table)
```

    ## Loading required package: data.table

    ## Warning: package 'data.table' was built under R version 3.5.2

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
data.test <- matrix(
  data = sample(
    x = c(0L, 1L, 2L, NA_integer_),#the genotypes
    size = 2e+07,
    replace = TRUE,
    prob = c(0.8, 0.10, 0.05, 0.05)
    ),
  nrow = 20000,#number of SNPs/markers
  ncol = 1000,#number of samples
  dimnames = list(rownames = seq(1, 20000, 1), colnames = seq(1, 1000, 1))
  ) %>%
  tibble::as_tibble(x = ., rownames = "MARKERS") 
```

``` r
library("cdata")
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
data.test <- data.frame(data.test)
```

test1: data.table::melt.data.table

``` r
system.time(
test1 <- data.table::as.data.table(data.test) %>%
  data.table::melt.data.table(
    data = .,
    id.vars = "MARKERS",
    variable.name = "INDIVIDUALS",
    value.name = "GENOTYPES",
    variable.factor = FALSE) 
)
```

    ##    user  system elapsed 
    ##   0.616   0.134   0.805

``` r
# reported: #~0.41sec
```

``` r
test1 <- orderby(test1, qc(MARKERS, INDIVIDUALS, GENOTYPES)) 
```

test2: tidyr::gather

``` r
system.time(
test2 <- tidyr::gather(
  data = data.test,
  key = "INDIVIDUALS",
  value = "GENOTYPES",
  -MARKERS)
)
```

    ##    user  system elapsed 
    ##   0.465   0.121   0.619

``` r
# reported: #~0.39sec
```

``` r
test2 <- orderby(test2, qc(MARKERS, INDIVIDUALS, GENOTYPES)) 
stopifnot(isTRUE(all.equal(test1, test2)))
```

test3: latest tidyr::pivot\_longer

``` r
run_pivot_longer <- exists('pivot_longer', 
                           where = 'package:tidyr', 
                           mode = 'function')
```

``` r
system.time(
test3 <- tidyr::pivot_longer(
  df = data.test,
  cols = -MARKERS,
  names_to = "INDIVIDUALS",
  values_to = "GENOTYPES")
)
# reported: #~90sec !!!
```

``` r
test3 <- orderby(test3, qc(MARKERS, INDIVIDUALS, GENOTYPES)) 
stopifnot(isTRUE(all.equal(test1, test3)))
```

test 4: cdata::unpivot\_to\_blocks()

Also slow, not optimized for this many columns.

``` r
system.time({
  cT <- build_unpivot_control(
    nameForNewKeyColumn = "INDIVIDUALS",
    nameForNewValueColumn = "GENOTYPES",
    columnsToTakeFrom = setdiff(colnames(data.test), 
                                c("MARKERS", "INDIVIDUALS", "GENOTYPES")))
  layout <- rowrecs_to_blocks_spec(
    cT,
    recordKeys = "MARKERS")
  
  test4 <- layout_by(layout, data.test)
})
```

    ##    user  system elapsed 
    ##  52.758  21.731  78.815

``` r
test4 <- orderby(test4, qc(MARKERS, INDIVIDUALS, GENOTYPES)) 
stopifnot(isTRUE(all.equal(test1, test4)))
```

``` r
system.time({
  back4 <- layout_by(t(layout), test4)
})
```

    ##    user  system elapsed 
    ## 366.398  35.514 405.453

``` r
back4 <- orderby(back4, colnames(back4)) 
data.test <- orderby(back4, colnames(data.test)) 
stopifnot(isTRUE(all.equal(data.test, back4)))
```
