PlotExample
================
Win-Vector LLC
12/20/2017

``` r
library("ggplot2")
library("cdata")
```

    ## Loading required package: wrapr

``` r
library("seplyr")

d <- readRDS("metrics.rds")
d$epoch <- seq_len(nrow(d))
print(head(d, n=1))
```

    ##    val_loss val_acc      loss   acc epoch
    ## 1 0.3961181  0.8603 0.5129159 0.786     1

``` r
cT <- dplyr::tribble(
  ~measure,               ~training, ~validation,
  "binary_cross_entropy", "loss",    "val_loss",
  "accuracy",              "acc",     "val_acc"
)
print(cT)
```

    ## # A tibble: 2 x 3
    ##   measure              training validation
    ##   <chr>                <chr>    <chr>     
    ## 1 binary_cross_entropy loss     val_loss  
    ## 2 accuracy             acc      val_acc

``` r
dT <- moveValuesToRowsD(
  d,
  controlTable = cT,
  columnsToCopy = "epoch")
print(head(dT))
```

    ##   epoch              measure  training validation
    ## 1     1 binary_cross_entropy 0.5129159  0.3961181
    ## 2     1             accuracy 0.7860000  0.8603000
    ## 3     2 binary_cross_entropy 0.3118882  0.3287723
    ## 4     2             accuracy 0.9010000  0.8705000
    ## 5     3 binary_cross_entropy 0.2313340  0.2847305
    ## 6     3             accuracy 0.9241333  0.8882000

``` r
pick <- dT %.>%
  filter_se(.,
            qe(measure == "binary_cross_entropy")) %.>%
  .$epoch[[which.min(.$validation)]]


ggplot(data = dT, aes(x = epoch,
                      xend = epoch,
                      y = validation,
                      yend = training,
                      ymin = pmin(validation, training),
                      ymax = pmax(validation, training))) +
  geom_segment() +
  geom_point() +
  geom_point(aes(y = training), shape = 3) +
  geom_smooth(se = FALSE) +
  geom_ribbon(alpha=0.2) +
  geom_vline(xintercept = pick, alpha=0.7, color='darkgreen') +
  facet_wrap(~measure, ncol=1, scales = 'free_y') +
  ylab("performance") +
  ggtitle("plotting over-fit as a function of epoch")
```

    ## `geom_smooth()` using method = 'loess'

![](PlotExample_files/figure-markdown_github/lineplot-1.png)
