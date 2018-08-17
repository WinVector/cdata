node\_version
================

Showing that `cdata` works the same with an `rquery` pipeline both in memory and on databases. Example from ["Fluid data reshaping with cdata"](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html).

``` r
library("cdata")
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
# example from: http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html
d <- wrapr::build_frame(
  "val_loss", "val_acc", "loss" , "acc" , "epoch" |
  -0.377    , 0.8722   , -0.5067, 0.7852, 1       |
  -0.2997   , 0.8895   , -0.3002, 0.904 , 2       |
  -0.2964   , 0.8822   , -0.2166, 0.9303, 3       |
  -0.2779   , 0.8899   , -0.1739, 0.9428, 4       |
  -0.2843   , 0.8861   , -0.1411, 0.9545, 5       |
  -0.312    , 0.8817   , -0.1136, 0.9656, 6       )

controlTable = build_frame(
  "measure"                   , "training", "validation" |
  "minus binary cross entropy", "loss"    , "val_loss"   |
  "accuracy"                  , "acc"     , "val_acc"    )
```

``` r
d1 <- rowrecs_to_blocks(
  d,
  controlTable = controlTable,
  columnsToCopy = "epoch")

knitr::kable(d1)
```

|  epoch| measure                    |  training|  validation|
|------:|:---------------------------|---------:|-----------:|
|      1| accuracy                   |    0.7852|      0.8722|
|      1| minus binary cross entropy |   -0.5067|     -0.3770|
|      2| accuracy                   |    0.9040|      0.8895|
|      2| minus binary cross entropy |   -0.3002|     -0.2997|
|      3| accuracy                   |    0.9303|      0.8822|
|      3| minus binary cross entropy |   -0.2166|     -0.2964|
|      4| accuracy                   |    0.9428|      0.8899|
|      4| minus binary cross entropy |   -0.1739|     -0.2779|
|      5| accuracy                   |    0.9545|      0.8861|
|      5| minus binary cross entropy |   -0.1411|     -0.2843|
|      6| accuracy                   |    0.9656|      0.8817|
|      6| minus binary cross entropy |   -0.1136|     -0.3120|

``` r
d2 <- blocks_to_rowrecs(
  d1,
  controlTable = controlTable,
  keyColumns = "epoch")

knitr::kable(d2)
```

|  epoch|     acc|     loss|  val\_acc|  val\_loss|
|------:|-------:|--------:|---------:|----------:|
|      1|  0.7852|  -0.5067|    0.8722|    -0.3770|
|      2|  0.9040|  -0.3002|    0.8895|    -0.2997|
|      3|  0.9303|  -0.2166|    0.8822|    -0.2964|
|      4|  0.9428|  -0.1739|    0.8899|    -0.2779|
|      5|  0.9545|  -0.1411|    0.8861|    -0.2843|
|      6|  0.9656|  -0.1136|    0.8817|    -0.3120|

``` r
ops1 <- local_td(d) %.>%
  rowrecs_to_blocks(
    .,
    controlTable = controlTable,
    columnsToCopy = "epoch")

cat(format(ops1))
```

    ## table(d; 
    ##   val_loss,
    ##   val_acc,
    ##   loss,
    ##   acc,
    ##   epoch) %.>%
    ##  non_sql_node(., rowrecs_to_blocks(.))

``` r
d %.>% 
  ops1 %.>%
  knitr::kable(.)
```

|  epoch| measure                    |  training|  validation|
|------:|:---------------------------|---------:|-----------:|
|      1| accuracy                   |    0.7852|      0.8722|
|      1| minus binary cross entropy |   -0.5067|     -0.3770|
|      2| accuracy                   |    0.9040|      0.8895|
|      2| minus binary cross entropy |   -0.3002|     -0.2997|
|      3| accuracy                   |    0.9303|      0.8822|
|      3| minus binary cross entropy |   -0.2166|     -0.2964|
|      4| accuracy                   |    0.9428|      0.8899|
|      4| minus binary cross entropy |   -0.1739|     -0.2779|
|      5| accuracy                   |    0.9545|      0.8861|
|      5| minus binary cross entropy |   -0.1411|     -0.2843|
|      6| accuracy                   |    0.9656|      0.8817|
|      6| minus binary cross entropy |   -0.1136|     -0.3120|

``` r
ops2 <- local_td(d1) %.>%
  blocks_to_rowrecs(
    .,
    controlTable = controlTable,
    keyColumns = "epoch")

cat(format(ops2))
```

    ## table(d1; 
    ##   epoch,
    ##   measure,
    ##   training,
    ##   validation) %.>%
    ##  non_sql_node(., blocks_to_rowrecs(.))

``` r
d1 %.>% 
  ops2 %.>%
  knitr::kable(.)
```

|  epoch|     acc|     loss|  val\_acc|  val\_loss|
|------:|-------:|--------:|---------:|----------:|
|      1|  0.7852|  -0.5067|    0.8722|    -0.3770|
|      2|  0.9040|  -0.3002|    0.8895|    -0.2997|
|      3|  0.9303|  -0.2166|    0.8822|    -0.2964|
|      4|  0.9428|  -0.1739|    0.8899|    -0.2779|
|      5|  0.9545|  -0.1411|    0.8861|    -0.2843|
|      6|  0.9656|  -0.1136|    0.8817|    -0.3120|

``` r
db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

rq_copy_to(db, "d", d)
```

    ## [1] "table(`d`; val_loss, val_acc, loss, acc, epoch)"

``` r
db %.>% 
  ops1 %.>%
  knitr::kable(.)
```

|  epoch| measure                    |  training|  validation|
|------:|:---------------------------|---------:|-----------:|
|      1| minus binary cross entropy |   -0.5067|     -0.3770|
|      1| accuracy                   |    0.7852|      0.8722|
|      2| minus binary cross entropy |   -0.3002|     -0.2997|
|      2| accuracy                   |    0.9040|      0.8895|
|      3| minus binary cross entropy |   -0.2166|     -0.2964|
|      3| accuracy                   |    0.9303|      0.8822|
|      4| minus binary cross entropy |   -0.1739|     -0.2779|
|      4| accuracy                   |    0.9428|      0.8899|
|      5| minus binary cross entropy |   -0.1411|     -0.2843|
|      5| accuracy                   |    0.9545|      0.8861|
|      6| minus binary cross entropy |   -0.1136|     -0.3120|
|      6| accuracy                   |    0.9656|      0.8817|

``` r
rq_copy_to(db, "d1", d1)
```

    ## [1] "table(`d1`; epoch, measure, training, validation)"

``` r
db %.>% ops2 %.>%
  knitr::kable(.)
```

|  epoch|     loss|     acc|  val\_loss|  val\_acc|
|------:|--------:|-------:|----------:|---------:|
|      1|  -0.5067|  0.7852|    -0.3770|    0.8722|
|      2|  -0.3002|  0.9040|    -0.2997|    0.8895|
|      3|  -0.2166|  0.9303|    -0.2964|    0.8822|
|      4|  -0.1739|  0.9428|    -0.2779|    0.8899|
|      5|  -0.1411|  0.9545|    -0.2843|    0.8861|
|      6|  -0.1136|  0.9656|    -0.3120|    0.8817|

``` r
DBI::dbDisconnect(db)
```
