Transform and Inverse
================

This is a quick re-work of the [`Keras` record transform
example](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html)
in `R`. For a `Python` version please see
[here](https://github.com/WinVector/data_algebra/blob/master/Examples/Inverse/Inverse.md).

In the [original
article](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html)
we had `Keras` model performance data, which looked like the following.

``` r
library(wrapr)
library(cdata)

df <- wrapr::build_frame(
  "val_loss"  , "val_acc", "loss" , "acc" , "epoch" |
    -0.377    , 0.8722   , -0.5067, 0.7852, 1       |
    -0.2997   , 0.8895   , -0.3002, 0.904 , 2       |
    -0.2964   , 0.8822   , -0.2166, 0.9303, 3       |
    -0.2779   , 0.8899   , -0.1739, 0.9428, 4       |
    -0.2843   , 0.8861   , -0.1411, 0.9545, 5       |
    -0.312    , 0.8817   , -0.1136, 0.9656, 6       )
knitr::kable(df)
```

| val\_loss | val\_acc |     loss |    acc | epoch |
| --------: | -------: | -------: | -----: | ----: |
|  \-0.3770 |   0.8722 | \-0.5067 | 0.7852 |     1 |
|  \-0.2997 |   0.8895 | \-0.3002 | 0.9040 |     2 |
|  \-0.2964 |   0.8822 | \-0.2166 | 0.9303 |     3 |
|  \-0.2779 |   0.8899 | \-0.1739 | 0.9428 |     4 |
|  \-0.2843 |   0.8861 | \-0.1411 | 0.9545 |     5 |
|  \-0.3120 |   0.8817 | \-0.1136 | 0.9656 |     6 |

But for plotting, it is more convenient to have the data in the
following form:

| epoch | measure                    | training | validation |
| ----: | :------------------------- | -------: | ---------: |
|     1 | minus binary cross entropy | \-0.5067 |   \-0.3770 |
|     1 | accuracy                   |   0.7852 |     0.8722 |
|     … |                            |          |            |

[The
article](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html)
uses ideas similar to
[these](https://winvector.github.io/cdata/articles/design.html) to
visualize the desired record structure and then write down this
visualization as a concrete data record example.

The principle is: if you have a visualization of the input and output,
it is then trivial to marshal these into a graphical representation of
the desired transform. And if you can’t work out what the input and
output look like, then you really are not quite ready to perform the
transform. Knowing what we want is the minimum requirement and with this
methodology it is also all that is needed.

``` r
shape <- wrapr::build_frame(
  "measure"                     , "training", "validation" |
    "minus binary cross entropy", "loss"    , "val_loss"   |
    "accuracy"                  , "acc"     , "val_acc"    )
knitr::kable(shape)
```

| measure                    | training | validation |
| :------------------------- | :------- | :--------- |
| minus binary cross entropy | loss     | val\_loss  |
| accuracy                   | acc      | val\_acc   |

This description of the desired record shape is easily transformed into
a data transformation specification.

``` r
xform <- rowrecs_to_blocks_spec(
  controlTable = shape,
  recordKeys = 'epoch') 

print(xform)
```

    ## {
    ##  row_record <- wrapr::qchar_frame(
    ##    "epoch"  , "loss", "acc", "val_loss", "val_acc" |
    ##      .      , loss  , acc  , val_loss  , val_acc   )
    ##  row_keys <- c('epoch')
    ## 
    ##  # becomes
    ## 
    ##  block_record <- wrapr::qchar_frame(
    ##    "epoch"  , "measure"                   , "training", "validation" |
    ##      .      , "minus binary cross entropy", loss      , val_loss     |
    ##      .      , "accuracy"                  , acc       , val_acc      )
    ##  block_keys <- c('epoch', 'measure')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = FALSE, strict = FALSE, allow_rqdatatable = TRUE)
    ## }

(Please see
[`help("rowrecs_to_blocks_spec")`](https://winvector.github.io/cdata/reference/rowrecs_to_blocks_spec.html),
[`help("blocks_to_rowrecs_spec")`](https://winvector.github.io/cdata/reference/blocks_to_rowrecs_spec.html),
and
[`help("layout_specification")`](https://winvector.github.io/cdata/reference/layout_specification.html)
for how to specify the various data transforms.)

We can easily apply this transform to our data.

``` r
res <- df %.>% xform

knitr::kable(res)
```

| epoch | measure                    | training | validation |
| ----: | :------------------------- | -------: | ---------: |
|     1 | minus binary cross entropy | \-0.5067 |   \-0.3770 |
|     1 | accuracy                   |   0.7852 |     0.8722 |
|     2 | minus binary cross entropy | \-0.3002 |   \-0.2997 |
|     2 | accuracy                   |   0.9040 |     0.8895 |
|     3 | minus binary cross entropy | \-0.2166 |   \-0.2964 |
|     3 | accuracy                   |   0.9303 |     0.8822 |
|     4 | minus binary cross entropy | \-0.1739 |   \-0.2779 |
|     4 | accuracy                   |   0.9428 |     0.8899 |
|     5 | minus binary cross entropy | \-0.1411 |   \-0.2843 |
|     5 | accuracy                   |   0.9545 |     0.8861 |
|     6 | minus binary cross entropy | \-0.1136 |   \-0.3120 |
|     6 | accuracy                   |   0.9656 |     0.8817 |

And it is simple to build an inverse transform.

``` r
inverse <- t(xform)

print(inverse)
```

    ## {
    ##  block_record <- wrapr::qchar_frame(
    ##    "epoch"  , "measure"                   , "training", "validation" |
    ##      .      , "minus binary cross entropy", loss      , val_loss     |
    ##      .      , "accuracy"                  , acc       , val_acc      )
    ##  block_keys <- c('epoch', 'measure')
    ## 
    ##  # becomes
    ## 
    ##  row_record <- wrapr::qchar_frame(
    ##    "epoch"  , "loss", "acc", "val_loss", "val_acc" |
    ##      .      , loss  , acc  , val_loss  , val_acc   )
    ##  row_keys <- c('epoch')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = FALSE, strict = FALSE, allow_rqdatatable = TRUE)
    ## }

And equally easy to apply this inverse transform to data.

``` r
res %.>% 
  inverse %.>% 
  knitr::kable(.)
```

| epoch |     loss |    acc | val\_loss | val\_acc |
| ----: | -------: | -----: | --------: | -------: |
|     1 | \-0.5067 | 0.7852 |  \-0.3770 |   0.8722 |
|     2 | \-0.3002 | 0.9040 |  \-0.2997 |   0.8895 |
|     3 | \-0.2166 | 0.9303 |  \-0.2964 |   0.8822 |
|     4 | \-0.1739 | 0.9428 |  \-0.2779 |   0.8899 |
|     5 | \-0.1411 | 0.9545 |  \-0.2843 |   0.8861 |
|     6 | \-0.1136 | 0.9656 |  \-0.3120 |   0.8817 |

Notice how each step can be inspected and checked as we worked. I would
definitely recommend re-reading [the original
article](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html)
with the new transform notation in mind. In any case, please check out
the `cdata` [package](https://github.com/WinVector/cdata) and
[documentation](https://winvector.github.io/cdata/).
