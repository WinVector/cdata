Ordered Grouping Example
================

## Introduction

I’d like to share an example of data-wrangling/data-reshaping and how to
solve it in [`R`](https://www.r-project.org) using
[`rqdatatable`](https://github.com/WinVector/rqdatatable/)/[`cdata`](https://github.com/WinVector/cdata/)
(the `Python` version of this example can be foune
[here](https://github.com/WinVector/data_algebra/blob/master/Examples/cdata/ranking_pivot_example.md)).

In an RStudio Community note, user <code>hklovs</code> asked [how to
re-organize some
data](https://community.rstudio.com/t/tidying-data-reorganizing-tibble/48292).

The solution is:

  - Get a good definition of what is wanted
  - Re-process the data so any advisory column you wished you had is
    actually there
  - And finish the problem.

## The problem

In this example the ask was equivalent to:

How do I transform data from this format:

| ID | OP | DATE                |
| -: | :- | :------------------ |
|  1 | A  | 2001-01-02 00:00:00 |
|  1 | B  | 2015-04-25 00:00:00 |
|  2 | A  | 2000-04-01 00:00:00 |
|  3 | D  | 2014-04-07 00:00:00 |
|  4 | C  | 2012-12-01 00:00:00 |
|  4 | A  | 2005-06-16 00:00:00 |
|  4 | D  | 2009-01-20 00:00:00 |
|  4 | B  | 2009-01-20 00:00:00 |
|  5 | A  | 2010-10-10 00:00:00 |
|  5 | B  | 2003-11-09 00:00:00 |
|  6 | B  | 2004-01-09 00:00:00 |

Into this
format:

| ID | DATE1               | OP1 | DATE2               | OP2         | DATE3               | OP3 |
| -: | :------------------ | :-- | :------------------ | :---------- | :------------------ | :-- |
|  1 | 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B           | NA                  | NA  |
|  2 | 2000-04-01 00:00:00 | A   | NA                  | NA          | NA                  | NA  |
|  3 | 2014-04-07 00:00:00 | D   | NA                  | NA          | NA                  | NA  |
|  4 | 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | c(“B”, “D”) | 2012-12-01 00:00:00 | C   |
|  5 | 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A           | NA                  | NA  |
|  6 | 2004-01-09 00:00:00 | B   | NA                  | NA          | NA                  | NA  |

## The solution

What the ask translates to is: per `ID` pick the first three operations
ordered by date, merging operations with the same timestamp. Then write
these results into a single row for each `ID`.

The first step isn’t to worry about the data format, it is an
inessential or solvable difficulty. Instead make any extra descriptions
or controls you need explicit. In this case we need ranks. So let’s
first add those.

``` r
# bring in all of our packages
library(wrapr)
library(rquery)
library(rqdatatable)
library(cdata)

# some example data
d <- wrapr::build_frame(
  "ID"  , "OP", "DATE"                |
    1   , "A" , "2001-01-02 00:00:00" |
    1   , "B" , "2015-04-25 00:00:00" |
    2   , "A" , "2000-04-01 00:00:00" |
    3   , "D" , "2014-04-07 00:00:00" |
    4   , "C" , "2012-12-01 00:00:00" |
    4   , "A" , "2005-06-16 00:00:00" |
    4   , "D" , "2009-01-20 00:00:00" |
    4   , "B" , "2009-01-20 00:00:00" |
    5   , "A" , "2010-10-10 00:00:00" |
    5   , "B" , "2003-11-09 00:00:00" |
    6   , "B" , "2004-01-09 00:00:00" )

# specify the first few data processing steps
ops <- local_td(d) %.>%
  project(.,
          OP := list(sort(unique(OP))),
          groupby = c("ID", "DATE")) %.>%
  extend(.,
         rank %:=% row_number(),
         partitionby = "ID",
         orderby = "DATE")

# apply the steps to the data
d2 <- d %.>% ops

# show the intermediate results.
knitr::kable(d2)
```

| ID | DATE                | OP          | rank |
| -: | :------------------ | :---------- | ---: |
|  1 | 2001-01-02 00:00:00 | A           |    1 |
|  1 | 2015-04-25 00:00:00 | B           |    2 |
|  2 | 2000-04-01 00:00:00 | A           |    1 |
|  3 | 2014-04-07 00:00:00 | D           |    1 |
|  4 | 2005-06-16 00:00:00 | A           |    1 |
|  4 | 2009-01-20 00:00:00 | c(“B”, “D”) |    2 |
|  4 | 2012-12-01 00:00:00 | C           |    3 |
|  5 | 2003-11-09 00:00:00 | B           |    1 |
|  5 | 2010-10-10 00:00:00 | A           |    2 |
|  6 | 2004-01-09 00:00:00 | B           |    1 |

In the above code we used the `project()` operator to combine rows with
duplicates combined into vectors such as `c("B", "D")`. Then we added a
rank column. This gets us much closer to a complete solution. All we
have to do now is re-arrange the data.

First data re-arrangement we strongly encourage drawing out what one
wants it terms of one input record and one output record. With `cdata`
doing so essentially solves the problem.

So let’s look at what happens only to the rows with `ID == 1`. In this
case we expect input rows that look like this:

| ID | DATE                | OP | rank |
| -: | :------------------ | :- | ---: |
|  1 | 2001-01-02 00:00:00 | A  |    1 |
|  1 | 2015-04-25 00:00:00 | B  |    2 |

And we want this record transformed into
this:

| ID | DATE1               | OP1 | DATE2               | OP2 | DATE3 | OP3 |
| -: | :------------------ | :-- | :------------------ | :-- | :---- | :-- |
|  1 | 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B   | NA    | NA  |

The `cdata` data shaping rule is: draw a picture of any non-trivial
(more than one row) data records in their full generality. In our case
the interesting record is the following.

``` r
# draw a picture of the record format
diagram <- wrapr::qchar_frame(
  "rank", "DATE", "OP" |
    "1",   DATE1, OP1  |
    "2",   DATE2, OP2  |
    "3",   DATE3, OP3  )

knitr::kable(diagram)
```

| rank | DATE  | OP  |
| :--- | :---- | :-- |
| 1    | DATE1 | OP1 |
| 2    | DATE2 | OP2 |
| 3    | DATE3 | OP3 |

The column names `rank`, `DATE`, and `OP` are all column names of the
table we are starting with. The values `1`, `2`, and `3` are all values
we expect to see in the `rank` column of the working data frame. And the
symbols `DATE1`, `DATE2`, `DATE3`, `OP1`, `OP2`, and `OP3` are all
stand-in names for values we see in our data. These symbols will be the
column names of our new row-records.

With this diagram in hand we can specify the data reshaping step.

``` r
transform <- blocks_to_rowrecs_spec(
  controlTable = diagram,
  recordKeys = 'ID')
```

The transform specifies that records are found in the format shown in
diagram, and are to be converted to rows. We can confirm the intent by
printing the transform.

``` r
print(transform)
```

    ## {
    ##  block_record <- wrapr::qchar_frame(
    ##    "ID"  , "rank", "DATE", "OP" |
    ##      .   , "1"   , DATE1 , OP1  |
    ##      .   , "2"   , DATE2 , OP2  |
    ##      .   , "3"   , DATE3 , OP3  )
    ##  block_keys <- c('ID', 'rank')
    ## 
    ##  # becomes
    ## 
    ##  row_record <- wrapr::qchar_frame(
    ##    "ID"  , "DATE1", "OP1", "DATE2", "OP2", "DATE3", "OP3" |
    ##      .   , DATE1  , OP1  , DATE2  , OP2  , DATE3  , OP3   )
    ##  row_keys <- c('ID')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = TRUE, strict = FALSE, allow_rqdatatable = FALSE)
    ## }

We are now ready to put all of our operations together into one
composite pipeline

``` r
# specify the operations 
ops <- local_td(d) %.>%
  project(.,  # get a vector of ops for each ID, DATE pair
          OP := list(sort(unique(OP))),
          groupby = c("ID", "DATE")) %.>%
  extend(.,  # add a per-ID rank by DATE column
         rank %:=% row_number(),
         partitionby = "ID",
         orderby = "DATE") %.>%
  transform %.>%  # transform the record shape
  orderby(.,  # ensure presentation is ordered by ID
          'ID')

# apply the operations to data
res <- d %.>% ops

# present the results
knitr::kable(res)
```

| ID | DATE1               | OP1 | DATE2               | OP2         | DATE3               | OP3 |
| -: | :------------------ | :-- | :------------------ | :---------- | :------------------ | :-- |
|  1 | 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B           | NA                  | NA  |
|  2 | 2000-04-01 00:00:00 | A   | NA                  | NA          | NA                  | NA  |
|  3 | 2014-04-07 00:00:00 | D   | NA                  | NA          | NA                  | NA  |
|  4 | 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | c(“B”, “D”) | 2012-12-01 00:00:00 | C   |
|  5 | 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A           | NA                  | NA  |
|  6 | 2004-01-09 00:00:00 | B   | NA                  | NA          | NA                  | NA  |

And we are done.

## A variation

If the ask had not wanted same-timestamp `OP`s merged into a list the
solution would have looked like this:

``` r
# specify the operations 
ops <- local_td(d) %.>%
  extend(.,  # add a per-ID rank by DATE and OP columns
         rank %:=% row_number(),
         partitionby = "ID",
         orderby = c("DATE", "OP")) %.>%
  transform %.>%  # transform the record shape
  orderby(.,  # ensure presentation is ore-red by ID
          'ID')

# apply the operations to data
res <- d %.>% ops

# present the results
knitr::kable(res)
```

| ID | DATE1               | OP1 | DATE2               | OP2 | DATE3               | OP3 |
| -: | :------------------ | :-- | :------------------ | :-- | :------------------ | :-- |
|  1 | 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B   | NA                  | NA  |
|  2 | 2000-04-01 00:00:00 | A   | NA                  | NA  | NA                  | NA  |
|  3 | 2014-04-07 00:00:00 | D   | NA                  | NA  | NA                  | NA  |
|  4 | 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B   | 2009-01-20 00:00:00 | D   |
|  5 | 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A   | NA                  | NA  |
|  6 | 2004-01-09 00:00:00 | B   | NA                  | NA  | NA                  | NA  |

Currently in `rquery`/`R` all of the steps except the
`list(sort(unique()))` step are easy to translate into `SQL`. So this
variation (which does not include the problematic aggregation step) is
easy to translate into `SQL` for use in databases.

``` r
# get an example database connection
raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_connection)
db <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))

# copy data into database
rquery::rq_copy_to(db, table_name = 'd', d)
```

    ## [1] "mk_td(\"d\", c( \"ID\", \"OP\", \"DATE\"))"

``` r
# run the query and land the result in the database
# WITHOUT round-tripping the data through R
rquery::materialize(db, ops, table_name = 'res')
```

    ## [1] "mk_td(\"res\", c( \"ID\", \"DATE1\", \"DATE2\", \"DATE3\", \"OP1\", \"OP2\", \"OP3\"))"

``` r
# read the result so we can see it
res_db <- DBI::dbReadTable(raw_connection, 'res')

# close the DB connection
DBI::dbDisconnect(raw_connection)

# present the results
knitr::kable(res_db)
```

| ID | DATE1               | DATE2               | DATE3               | OP1 | OP2 | OP3 |
| -: | :------------------ | :------------------ | :------------------ | :-- | :-- | :-- |
|  1 | 2001-01-02 00:00:00 | 2015-04-25 00:00:00 | NA                  | A   | B   | NA  |
|  2 | 2000-04-01 00:00:00 | NA                  | NA                  | A   | NA  | NA  |
|  3 | 2014-04-07 00:00:00 | NA                  | NA                  | D   | NA  | NA  |
|  4 | 2005-06-16 00:00:00 | 2009-01-20 00:00:00 | 2009-01-20 00:00:00 | A   | B   | D   |
|  5 | 2003-11-09 00:00:00 | 2010-10-10 00:00:00 | NA                  | B   | A   | NA  |
|  6 | 2004-01-09 00:00:00 | NA                  | NA                  | B   | NA  | NA  |

Note: column order is not considered essential in `rquery` pipelines
(though the `select_columns()` can specify it). Also, the entire query
*can* be run in a database with the correct user-specified aggregation
function. We have a demonstration of this in our `Python` version of
this example.
