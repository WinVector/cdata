LightTests
================

``` r
# row rec form
d_rowrec_form <- wrapr::build_frame(
  'val_loss', 'val_acc', 'loss', 'acc' , 'epoch' |
  0.377     , 0.8722   , 0.5067, 0.7852, 1L      |
  0.2997    , 0.8895   , 0.3002, 0.904 , 2L      |
  0.2964    , 0.8822   , 0.2166, 0.9303, 3L      |
  0.2779    , 0.8899   , 0.1739, 0.9428, 4L      |
  0.2843    , 0.8861   , 0.1411, 0.9545, 5L      |
  0.312     , 0.8817   , 0.1136, 0.9656, 6L      )

# block form
d_block_form <- wrapr::build_frame(
  'epoch', 'measure'                   , 'training', 'validation' |
  1L     , 'accuracy'                  , 0.8722    , 0.7852       |
  1L     , 'minus binary cross entropy', 0.377     , 0.5067       |
  2L     , 'accuracy'                  , 0.8895    , 0.904        |
  2L     , 'minus binary cross entropy', 0.2997    , 0.3002       |
  3L     , 'accuracy'                  , 0.8822    , 0.9303       |
  3L     , 'minus binary cross entropy', 0.2964    , 0.2166       |
  4L     , 'accuracy'                  , 0.8899    , 0.9428       |
  4L     , 'minus binary cross entropy', 0.2779    , 0.1739       |
  5L     , 'accuracy'                  , 0.8861    , 0.9545       |
  5L     , 'minus binary cross entropy', 0.2843    , 0.1411       |
  6L     , 'accuracy'                  , 0.8817    , 0.9656       |
  6L     , 'minus binary cross entropy', 0.312     , 0.1136       )

# conversion control table
controlTable <- wrapr::build_frame(
  'measure'                   , 'training', 'validation' |
  'minus binary cross entropy', 'loss'    , 'val_loss'   |
  'accuracy'                  , 'acc'     , 'val_acc'    )


cdata::rowrecs_to_blocks(d_rowrec_form, controlTable, columnsToCopy = "epoch", use_data_table = TRUE)
```

    ##    epoch                    measure training validation
    ## 1      1                   accuracy   0.7852     0.8722
    ## 2      1 minus binary cross entropy   0.5067     0.3770
    ## 3      2                   accuracy   0.9040     0.8895
    ## 4      2 minus binary cross entropy   0.3002     0.2997
    ## 5      3                   accuracy   0.9303     0.8822
    ## 6      3 minus binary cross entropy   0.2166     0.2964
    ## 7      4                   accuracy   0.9428     0.8899
    ## 8      4 minus binary cross entropy   0.1739     0.2779
    ## 9      5                   accuracy   0.9545     0.8861
    ## 10     5 minus binary cross entropy   0.1411     0.2843
    ## 11     6                   accuracy   0.9656     0.8817
    ## 12     6 minus binary cross entropy   0.1136     0.3120

``` r
cdata::rowrecs_to_blocks(d_rowrec_form, controlTable, columnsToCopy = "epoch", use_data_table = FALSE)
```

    ##    epoch                    measure training validation
    ## 1      1 minus binary cross entropy   0.5067     0.3770
    ## 2      1                   accuracy   0.7852     0.8722
    ## 3      2 minus binary cross entropy   0.3002     0.2997
    ## 4      2                   accuracy   0.9040     0.8895
    ## 5      3 minus binary cross entropy   0.2166     0.2964
    ## 6      3                   accuracy   0.9303     0.8822
    ## 7      4 minus binary cross entropy   0.1739     0.2779
    ## 8      4                   accuracy   0.9428     0.8899
    ## 9      5 minus binary cross entropy   0.1411     0.2843
    ## 10     5                   accuracy   0.9545     0.8861
    ## 11     6 minus binary cross entropy   0.1136     0.3120
    ## 12     6                   accuracy   0.9656     0.8817

``` r
cdata::blocks_to_rowrecs(d_block_form, controlTable, keyColumns = "epoch", use_data_table = TRUE)
```

    ##   epoch    acc   loss val_acc val_loss
    ## 1     1 0.8722 0.3770  0.7852   0.5067
    ## 2     2 0.8895 0.2997  0.9040   0.3002
    ## 3     3 0.8822 0.2964  0.9303   0.2166
    ## 4     4 0.8899 0.2779  0.9428   0.1739
    ## 5     5 0.8861 0.2843  0.9545   0.1411
    ## 6     6 0.8817 0.3120  0.9656   0.1136

``` r
cdata::blocks_to_rowrecs(d_block_form, controlTable, keyColumns = "epoch", use_data_table = FALSE)
```

    ##   epoch   loss    acc val_loss val_acc
    ## 1     1 0.3770 0.8722   0.5067  0.7852
    ## 2     2 0.2997 0.8895   0.3002  0.9040
    ## 3     3 0.2964 0.8822   0.2166  0.9303
    ## 4     4 0.2779 0.8899   0.1739  0.9428
    ## 5     5 0.2843 0.8861   0.1411  0.9545
    ## 6     6 0.3120 0.8817   0.1136  0.9656
