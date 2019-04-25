table
================

Recently a deceptively simple problem through us a day layout curve-ball: how do you convert an object of class `table` into a `data.frame` with an actual show similar to what `table` displays?

Here is our example:

``` r
confmat <- readRDS("confmat.rds")

print(confmat)
```

    ##      pred
    ## truth FALSE TRUE
    ##     0   581   33
    ##     1    30  147

``` r
as.data.frame(confmat)
```

    ##   truth  pred Freq
    ## 1     0 FALSE  581
    ## 2     1 FALSE   30
    ## 3     0  TRUE   33
    ## 4     1  TRUE  147

Notice the `data.frame` is in a tall form, not the same as `print(confmat)`.

Now there are ways to get the data into the form we want: `as.matrix()` and also `unclass()`.

``` r
as.matrix(confmat)
```

    ##      pred
    ## truth FALSE TRUE
    ##     0   581   33
    ##     1    30  147

``` r
unclass(confmat)
```

    ##      pred
    ## truth FALSE TRUE
    ##     0   581   33
    ##     1    30  147

But let's suppose we are starting from the data-frame form.

``` r
conf_frame <- as.data.frame(confmat)

knitr::kable(conf_frame)
```

| truth | pred  |  Freq|
|:------|:------|-----:|
| 0     | FALSE |   581|
| 1     | FALSE |    30|
| 0     | TRUE  |    33|
| 1     | TRUE  |   147|

The transform can be built up as follows. We draw what we think is coming in (with symbolic names `tFpF`, `tTpF`, `tFpT`, and `tTpT` for the values to be determined).

``` r
example_in <- wrapr::qchar_frame(
  "truth" , "pred" , "Freq" |
    "0"   , "FALSE", tFpF   |
    "1"   , "FALSE", tTpF   |
    "0"   , "TRUE" , tFpT   |
    "1"   , "TRUE" , tTpT   )

knitr::kable(example_in)
```

| truth | pred  | Freq |
|:------|:------|:-----|
| 0     | FALSE | tFpF |
| 1     | FALSE | tTpF |
| 0     | TRUE  | tFpT |
| 1     | TRUE  | tTpT |

And also draw what we want.

``` r
example_out <- wrapr::qchar_frame(
  "_"                , "pred: FALSE", "pred: TRUE" |
    "truth: FALSE"   ,  tFpF,         tFpT         |
    "truth: TRUE"    ,  tTpF,         tTpT         )

knitr::kable(example_out)
```

| \_           | pred: FALSE | pred: TRUE |
|:-------------|:------------|:-----------|
| truth: FALSE | tFpF        | tFpT       |
| truth: TRUE  | tTpF        | tTpT       |

Now we use the `cdata` package and build a `layout_specification` with the incoming shape as what we expect to see and the outgoing shape as what we want.

``` r
library("cdata")

layout <- layout_specification(
  incoming_shape = example_in,
  outgoing_shape = example_out)
```

    ## Error in blocks_to_rowrecs_spec(incoming_shape, controlTableKeys = incoming_controlTableKeys, : cdata::blocks_to_rowrecs_spec controlTable rows must be uniquely keyed by controlTableKeys key columns

Now, that wasn't allowed as the first column of the incoming example did not identify the rows of the example (the first column was not sufficient to key). So we try again and add the deceleration that the incoming shape is keyed by "`truth`" plus "`pred`".

``` r
layout <- layout_specification(
  incoming_shape = example_in,
  incoming_controlTableKeys = c("truth", "pred"),
  outgoing_shape = example_out)
```

And now we can apply the layout transformation.

``` r
conf_frame %.>% layout
```

    ##              _ pred: FALSE pred: TRUE
    ## 1 truth: FALSE         581         33
    ## 2  truth: TRUE          30        147

As always, we can also print the layout transformation.

``` r
print(layout)
```

    ## {
    ##  in_record <- wrapr::qchar_frame(
    ##    "truth"  , "pred" , "Freq" |
    ##      "0"    , "FALSE", tFpF   |
    ##      "1"    , "FALSE", tTpF   |
    ##      "0"    , "TRUE" , tFpT   |
    ##      "1"    , "TRUE" , tTpT   )
    ##  in_keys <- c('truth', 'pred')
    ## 
    ##  # becomes
    ## 
    ##  out_record <- wrapr::qchar_frame(
    ##    "_"             , "pred: FALSE", "pred: TRUE" |
    ##      "truth: FALSE", tFpF         , tFpT         |
    ##      "truth: TRUE" , tTpF         , tTpT         )
    ##  out_keys <- c('_')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = TRUE, strict = FALSE, allow_rqdatatable = FALSE)
    ## }

This is a lot of work to transform the table, but it turns out this little transform is a hard one.
