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
head(d)
```

    ##    val_loss val_acc      loss       acc epoch
    ## 1 0.3961181  0.8603 0.5129159 0.7860000     1
    ## 2 0.3287723  0.8705 0.3118882 0.9010000     2
    ## 3 0.2847305  0.8882 0.2313340 0.9241333     3
    ## 4 0.2750951  0.8910 0.1819643 0.9403333     4
    ## 5 0.2996226  0.8809 0.1505077 0.9523333     5
    ## 6 0.2899937  0.8867 0.1222806 0.9618000     6

``` r
cT <- buildUnPivotControlTable(
  nameForNewKeyColumn= 'orig_col_name',
  nameForNewValueColumn= 'performance',
  columnsToTakeFrom= c('val_loss',
                       'val_acc',
                       'loss',
                       'acc' ))
dT <- moveValuesToRowsD(
  d,
  controlTable = cT,
  columnsToCopy = "epoch")
head(dT)
```

    ##   epoch orig_col_name performance
    ## 1     1      val_loss   0.3961181
    ## 2     1       val_acc   0.8603000
    ## 3     1          loss   0.5129159
    ## 4     1           acc   0.7860000
    ## 5     2      val_loss   0.3287723
    ## 6     2       val_acc   0.8705000

``` r
mp <- data.frame(
  orig_col_name = qc(val_loss, val_acc, 
                     loss, acc),
  dataset = qc("validation", "validation", 
               "training", "training"),
  measure = qc("binary cross entropy", "accuracy",
               "binary cross entropy", "accuracy"),
  stringsAsFactors = FALSE)
print(mp)
```

    ##   orig_col_name    dataset              measure
    ## 1      val_loss validation binary cross entropy
    ## 2       val_acc validation             accuracy
    ## 3          loss   training binary cross entropy
    ## 4           acc   training             accuracy

``` r
dT <- map_fieldsD(dT, 
                  "orig_col_name",
                  mp)
head(dT)
```

    ##   epoch orig_col_name performance    dataset              measure
    ## 1     1      val_loss   0.3961181 validation binary cross entropy
    ## 2     1       val_acc   0.8603000 validation             accuracy
    ## 3     1          loss   0.5129159   training binary cross entropy
    ## 4     1           acc   0.7860000   training             accuracy
    ## 5     2      val_loss   0.3287723 validation binary cross entropy
    ## 6     2       val_acc   0.8705000 validation             accuracy

``` r
pick <- dT %.>%
  filter_se(.,
            qe(measure == "binary cross entropy",
               dataset == "validation")) %.>%
  .$epoch[[which.min(.$performance)]]

ggplot(data = dT, 
       aes(x = epoch, 
           y = performance,
           color = dataset)) +
  geom_point() +
  geom_line() +
  facet_wrap(~measure, ncol=1, scales = "free_y") +
  geom_vline(xintercept = pick, alpha=0.7, color='darkgreen') +
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("model performance by epoch, dataset, and measure")
```

![](PlotExample_files/figure-markdown_github/firstplot-1.png)

``` r
print(head(d, n=1))
```

    ##    val_loss val_acc      loss   acc epoch
    ## 1 0.3961181  0.8603 0.5129159 0.786     1

``` r
cT <- dplyr::tribble(
  ~measure,               ~training, ~validation,
  "binary cross entropy", "loss",    "val_loss",
  "accuracy",              "acc",     "val_acc"
)
print(cT)
```

    ## # A tibble: 2 x 3
    ##   measure              training validation
    ##   <chr>                <chr>    <chr>     
    ## 1 binary cross entropy loss     val_loss  
    ## 2 accuracy             acc      val_acc

``` r
dT <- moveValuesToRowsD(
  d,
  controlTable = cT,
  columnsToCopy = "epoch")
print(head(dT))
```

    ##   epoch              measure  training validation
    ## 1     1 binary cross entropy 0.5129159  0.3961181
    ## 2     1             accuracy 0.7860000  0.8603000
    ## 3     2 binary cross entropy 0.3118882  0.3287723
    ## 4     2             accuracy 0.9010000  0.8705000
    ## 5     3 binary cross entropy 0.2313340  0.2847305
    ## 6     3             accuracy 0.9241333  0.8882000

``` r
pick <- dT %.>%
  filter_se(.,
            qe(measure == "binary cross entropy")) %.>%
  .$epoch[[which.min(.$validation)]]


ggplot(data = dT, 
       aes(x = epoch,
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
