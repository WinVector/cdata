
<!-- README.md is generated from README.Rmd. Please edit that file -->
[`cdata`](https://CRAN.R-project.org/package=cdata) is a general data re-shaper that has the great virtue of adhering to the so-called "Rule of Representation":

> Fold knowledge into data, so program logic can be stupid and robust.
>
> [*The Art of Unix Programming*, Erick S. Raymond, Addison-Wesley , 2003](http://www.catb.org/esr/writings/taoup/html/ch01s06.html#id2878263)

The point being: it is much easier to reason about data than to try to reason about code, so using data to control your code is often a very good trade-off.

![](https://raw.githubusercontent.com/WinVector/cdata/master/tools/cdata.png)

Briefly: `cdata` supplies data transform operators that:

-   Work on local data or with any `DBI` data source.
-   Are powerful generalizations of the operations commonly called `pivot` and `un-pivot`.

A quick example: plot iris petal and sepal dimensions in a faceted graph.

``` r
iris <- data.frame(iris)

library("ggplot2")
library("cdata")

#
# build a control table with a "key column" flower_part
# and "value columns" Length and Width
#
controlTable <- wrapr::qchar_frame(
   flower_part, Length      , Width       |
   Petal    , Petal.Length, Petal.Width |
   Sepal    , Sepal.Length, Sepal.Width )

# do the unpivot to convert the row records to block records
iris_aug <- rowrecs_to_blocks(
  iris,
  controlTable,
  columnsToCopy = c("Species"))


ggplot(iris_aug, aes(x=Length, y=Width)) +
  geom_point(aes(color=Species, shape=Species)) + 
  facet_wrap(~flower_part, labeller = label_both, scale = "free") +
  ggtitle("Iris dimensions") +  scale_color_brewer(palette = "Dark2")
```

![](tools/README-ex0-1.png)

More details on the above example can be found [here](http://www.win-vector.com/blog/2018/10/faceted-graphs-with-cdata-and-ggplot2/). A tutorial on how to design a `controlTable` can be found [here](https://winvector.github.io/cdata/articles/design.html).
And some discussion of the nature of records in `cdata` can be found [here](https://winvector.github.io/cdata/articles/blocksrecs.html).

------------------------------------------------------------------------

We can also exhibit a larger example of using `cdata` to create a scatter-plot matrix, or pair plot:

``` r

iris <- data.frame(iris)

library("ggplot2")
library("cdata")

# declare our columns of interest
meas_vars <- qc(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
category_variable <- "Species"

# build a control with all pairs of variables as value columns
# and pair_key as the key column
controlTable <- data.frame(expand.grid(meas_vars, meas_vars, 
                                       stringsAsFactors = FALSE))
# name the value columns value1 and value2
colnames(controlTable) <- qc(value1, value2)
# insert first, or key column
controlTable <- cbind(
  data.frame(pair_key = paste(controlTable[[1]], controlTable[[2]]),
             stringsAsFactors = FALSE),
  controlTable)


# do the unpivot to convert the row records to multiple block records
iris_aug <- rowrecs_to_blocks(
  iris,
  controlTable,
  columnsToCopy = category_variable)

# unpack the key column into two variable keys for the facet_grid
splt <- strsplit(iris_aug$pair_key, split = " ", fixed = TRUE)
iris_aug$v1 <- vapply(splt, function(si) si[[1]], character(1))
iris_aug$v2 <- vapply(splt, function(si) si[[2]], character(1))


ggplot(iris_aug, aes(x=value1, y=value2)) +
  geom_point(aes_string(color=category_variable, shape=category_variable)) + 
  facet_grid(v2~v1, labeller = label_both, scale = "free") +
  ggtitle("Iris dimensions") +
  scale_color_brewer(palette = "Dark2") +
  ylab(NULL) + 
  xlab(NULL)
```

![](tools/README-ex0_1-1.png)

The above is now wrapped into a [one-line command in `WVPlots`](https://winvector.github.io/WVPlots/reference/PairPlot.html).

And a quick database example:

``` r
library("cdata")
library("rquery")

use_spark <- FALSE

if(use_spark) {
  my_db <- sparklyr::spark_connect(version='2.2.0', 
                                   master = "local")
} else {
  my_db <- DBI::dbConnect(RSQLite::SQLite(),
                          ":memory:")
}



# pivot example
d <- wrapr::build_frame(
   "meas", "val" |
   "AUC" , 0.6   |
   "R2"  , 0.2   )
DBI::dbWriteTable(my_db,
                  'd',
                  d,
                  temporary = TRUE)
rstr(my_db, 'd')
 #  table `d` SQLiteConnection 
 #   nrow: 2 
 #  'data.frame':   2 obs. of  2 variables:
 #   $ meas: chr  "AUC" "R2"
 #   $ val : num  0.6 0.2
td <- db_td(my_db, "d")
td
 #  [1] "table(`d`; meas, val)"

cT <- td %.>%
  build_pivot_control(.,
                      columnToTakeKeysFrom= 'meas',
                      columnToTakeValuesFrom= 'val') %.>%
  execute(my_db, .)
print(cT)
 #    meas val
 #  1  AUC AUC
 #  2   R2  R2

tab <- td %.>%
  blocks_to_rowrecs(.,
                    keyColumns = NULL,
                    controlTable = cT,
                    temporary = FALSE) %.>%
  materialize(my_db, .)

print(tab)
 #  [1] "table(`rquery_mat_17213185689570943899_0000000000`; AUC, R2)"
  
rstr(my_db, tab)
 #  table `rquery_mat_17213185689570943899_0000000000` SQLiteConnection 
 #   nrow: 1 
 #  'data.frame':   1 obs. of  2 variables:
 #   $ AUC: num 0.6
 #   $ R2 : num 0.2

if(use_spark) {
  sparklyr::spark_disconnect(my_db)
} else {
  DBI::dbDisconnect(my_db)
}
```

------------------------------------------------------------------------

The `cdata` package is a demonstration of the ["coordinatized data" theory](http://winvector.github.io/FluidData/RowsAndColumns.html) and includes an implementation of the ["fluid data" methodology](http://winvector.github.io/FluidData/FluidData.html). The recommended tutorial is: [Fluid data reshaping with cdata](http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html). We also have a [short free cdata screencast](https://youtu.be/4cYbP3kbc0k) (and another example can be found [here](http://winvector.github.io/FluidData/DataWranglingAtScale.html)).

------------------------------------------------------------------------

Install via CRAN:

``` r
install.packages("cdata")
```

Or you can get the latest development version with

``` r
devtools::install_github("WinVector/cdata")
```

Note: `cdata` is targeted at data with "tame column names" (column names that are valid both in databases, and as `R` unquoted variable names) and basic types (column values that are simple `R` types such as `character`, `numeric`, `logical`, and so on).
