Layout Solution Conversion
================

The other day [I answered a question](http://www.win-vector.com/blog/2019/04/controlling-data-layout-with-cdata/) of the form:

What code turns records that look like this:

``` r
before <- wrapr::build_frame(
  "id"  , "xa", "xb", "ya", "yb" |
    1   , 1   , 3   , 6   , 8    |
    2   , 2   , 4   , 7   , 9    )

knitr::kable(before)
```

|   id|   xa|   xb|   ya|   yb|
|----:|----:|----:|----:|----:|
|    1|    1|    3|    6|    8|
|    2|    2|    4|    7|    9|

into records that look like this:

``` r
after <- wrapr::build_frame(
  "id"  , "t", "x", "y" |
    1   , "a", 1  , 6   |
    1   , "b", 3  , 8   |
    2   , "a", 2  , 7   |
    2   , "b", 4  , 9   )

knitr::kable(after)
```

|   id| t   |    x|    y|
|----:|:----|----:|----:|
|    1| a   |    1|    6|
|    1| b   |    3|    8|
|    2| a   |    2|    7|
|    2| b   |    4|    9|

[My solution](http://www.win-vector.com/blog/2019/04/controlling-data-layout-with-cdata/), using our own `cdata` package, was roughly the following.

``` r
# attach our package
library("cdata")

# specify what the body of a block
# record looks like
record_specification <- wrapr::qchar_frame(
  "t"  , "x", "y" |
    "a", xa , ya    |
    "b", xb , yb    )

# specify the record layout 
# transformation
converter <- rowrecs_to_blocks_spec(
  record_specification,
  recordKeys = "id")

# look that we have the right transformation
print(converter)
```

    ## {
    ##  row_record <- wrapr::qchar_frame(
    ##    "id"  , "xa", "xb", "ya", "yb" |
    ##      .   , xa  , xb  , ya  , yb   )
    ##  row_keys <- c('id')
    ## 
    ##  # becomes
    ## 
    ##  block_record <- wrapr::qchar_frame(
    ##    "id"  , "t", "x", "y" |
    ##      .   , "a", xa , ya  |
    ##      .   , "b", xb , yb  )
    ##  block_keys <- c('id', 't')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = TRUE, strict = FALSE)
    ## }

``` r
# apply the transformation
before %.>%
  converter %.>%
  knitr::kable(.)
```

|   id| t   |    x|    y|
|----:|:----|----:|----:|
|    1| a   |    1|    6|
|    1| b   |    3|    8|
|    2| a   |    2|    7|
|    2| b   |    4|    9|

Now the solution write-up as considerably slowed down to try and teach the concepts, and teach how to derive the transform specification for problems in general. Also important was the separation of the problem solving, inspection, and transform application phases.

Of course other solutions are possible, such as [a donated `tidyverse` solution](http://www.win-vector.com/blog/2019/04/controlling-data-layout-with-cdata/#comment-67188).

``` r
library(tidyverse)
```

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
before <- tibble::tribble(
  ~id , ~xa, ~xb, ~ya, ~yb ,
  1 , 1 , 3 , 6 , 8 ,
  2 , 2 , 4 , 7 , 9 )

knitr::kable(before)
```

|   id|   xa|   xb|   ya|   yb|
|----:|----:|----:|----:|----:|
|    1|    1|    3|    6|    8|
|    2|    2|    4|    7|    9|

``` r
tidy_pipeline <- . %>%
  gather(key, value, -id) %>% # wide to long
  separate(key, c("x", "t"), sep = 1) %>% # split column names
  spread(x, value) # long to wide

before %>%
  tidy_pipeline %>%
  knitr::kable()
```

|   id| t   |    x|    y|
|----:|:----|----:|----:|
|    1| a   |    1|    6|
|    1| b   |    3|    8|
|    2| a   |    2|    7|
|    2| b   |    4|    9|

Now if this is part of a "R has many good systems, including `tidyverse`" that is pretty nice (though our understanding is `tidyr` is moving away from the above string parsing methods, to more `cdata`-like concepts). If this is part of an implied "only `tidyverse` is allowed in R" push, then that would be something else.

Either way, it got me to thinking. How hard would it be to go the other way? How much work is it to adapt an existing `tidyverse` solution back into `cdata` tools? It turns out it is very easy: just run the `tidyverse` solution on itself, and capture that result as the transform specification. The only additional information needed is what set of columns are the per-record keys.

Lets take a look at doing that.

``` r
# attach packages
library("cdata")
library("wrapr")
```

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

    ## The following object is masked from 'package:tibble':
    ## 
    ##     view

``` r
# define a function that takes a function
# that maps rowrecs to blocks and captures
# the transform in cdata notation
build_cdata_transform <- function(before, f, recordKeys) {
  # build a row-record that has cell values
  # equal to column names
  row <- as.data.frame(
    as.list(
      colnames(before) := colnames(before)))

  # apply the f transform to the example row
  # record, and delete out the recordKey columns
  block <- f(row) %.>%
    select(., setdiff(colnames(.), recordKeys))

  # return the block result as the transform specification.
  rowrecs_to_blocks_spec(block, recordKeys = recordKeys)
}

# apply the capture function
recordKeys <- "id"
layout <- build_cdata_transform(before, 
                                tidy_pipeline, 
                                recordKeys)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
# confirm we got the right transform
print(layout)
```

    ## {
    ##  row_record <- wrapr::qchar_frame(
    ##    "id"  , "xa", "xb", "ya", "yb" |
    ##      .   , xa  , xb  , ya  , yb   )
    ##  row_keys <- c('id')
    ## 
    ##  # becomes
    ## 
    ##  block_record <- wrapr::qchar_frame(
    ##    "id"  , "t", "x", "y" |
    ##      .   , "a", xa , ya  |
    ##      .   , "b", xb , yb  )
    ##  block_keys <- c('id', 't')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = TRUE, strict = FALSE)
    ## }

``` r
# use it
before %.>%
  layout %.>%
  knitr::kable(.)
```

|   id| t   |    x|    y|
|----:|:----|----:|----:|
|    1| a   |    1|    6|
|    1| b   |    3|    8|
|    2| a   |    2|    7|
|    2| b   |    4|    9|
