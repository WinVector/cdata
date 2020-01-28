Ordered Grouping Example
================

John Mount and Nina Zumel

Introduction
------------

This is an example of an interesting data-wrangling/data-reshaping problem and how to solve it in [`R`](https://www.r-project.org) using [`rqdatatable`](https://github.com/WinVector/rqdatatable/)/[`cdata`](https://github.com/WinVector/cdata/). The `Python` version of this example can be found [here](https://github.com/WinVector/data_algebra/blob/master/Examples/cdata/ranking_pivot_example.md)).

The problem
-----------

In an RStudio Community note, user <code>hklovs</code> asked [how to re-organize some data](https://community.rstudio.com/t/tidying-data-reorganizing-tibble/48292). The ask was essentially to transform data from this format:

|   ID| OP  | DATE                |
|----:|:----|:--------------------|
|    1| A   | 2001-01-02 00:00:00 |
|    1| B   | 2015-04-25 00:00:00 |
|    2| A   | 2000-04-01 00:00:00 |
|    3| D   | 2014-04-07 00:00:00 |
|    4| C   | 2012-12-01 00:00:00 |
|    4| A   | 2005-06-16 00:00:00 |
|    4| D   | 2009-01-20 00:00:00 |
|    4| B   | 2009-01-20 00:00:00 |
|    5| A   | 2010-10-10 00:00:00 |
|    5| B   | 2003-11-09 00:00:00 |
|    6| B   | 2004-01-09 00:00:00 |

Into this format:

|   ID| DATE1               | OP1 | DATE2               | OP2  | DATE3               | OP3 |
|----:|:--------------------|:----|:--------------------|:-----|:--------------------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B    | NA                  | NA  |
|    2| 2000-04-01 00:00:00 | A   | NA                  | NA   | NA                  | NA  |
|    3| 2014-04-07 00:00:00 | D   | NA                  | NA   | NA                  | NA  |
|    4| 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B, D | 2012-12-01 00:00:00 | C   |
|    5| 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A    | NA                  | NA  |
|    6| 2004-01-09 00:00:00 | B   | NA                  | NA   | NA                  | NA  |

That is: for each `ID` pick the first three operations ordered by date, merging operations with the same timestamp. Then write these results into a single row for each `ID`.

The solution
------------

A good way to solve any data-wrangling problem is to:

-   Get a good definition of what is wanted
-   Re-process the data so any advisory column you wished you had is actually there
-   And finish the problem.

Let's apply this process to our example problem.

### Adding an advisory rank column

The first step isn't to worry about the data format, as it is an inessential or solvable difficulty. Instead make any extra descriptions or controls you need explicit. In this case we need to date-rank and to merge the operations (per `ID`). So let's do that first.

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
    4   , "D" , "2009-01-20 00:00:00" |  # this and the next row
    4   , "B" , "2009-01-20 00:00:00" |  # are on the same date
    5   , "A" , "2010-10-10 00:00:00" |
    5   , "B" , "2003-11-09 00:00:00" |
    6   , "B" , "2004-01-09 00:00:00" )


# a function to paste a vector of strings together
concat_values = function(v) {
  paste(sort(unique(v)), collapse=", ")
}

# merge the operations to get one row per ID and DATE
# then rank the rows for each ID by DATE
ops <- local_td(d) %.>%
  project(.,  # fuse all the ops on same date/id into one string
          OP := concat_values(OP),  
          groupby = c("ID", "DATE")) %.>%
  extend(.,   # rank each ID group in order of date
         rank %:=% row_number(),
         partitionby = "ID",
         orderby = "DATE")

# apply the steps to the data
d2 <- d %.>% ops

# show the intermediate results.
knitr::kable(d2)
```

|   ID| DATE                | OP   |  rank|
|----:|:--------------------|:-----|-----:|
|    1| 2001-01-02 00:00:00 | A    |     1|
|    1| 2015-04-25 00:00:00 | B    |     2|
|    2| 2000-04-01 00:00:00 | A    |     1|
|    3| 2014-04-07 00:00:00 | D    |     1|
|    4| 2005-06-16 00:00:00 | A    |     1|
|    4| 2009-01-20 00:00:00 | B, D |     2|
|    4| 2012-12-01 00:00:00 | C    |     3|
|    5| 2003-11-09 00:00:00 | B    |     1|
|    5| 2010-10-10 00:00:00 | A    |     2|
|    6| 2004-01-09 00:00:00 | B    |     1|

In the above code we used the `project()` operator to merge rows with duplicate `ID` and `DATE` into a single string listing all the operations that occurred, for example "B, D". Then we added a rank column. This gives us all the information we need for a complete solution to the original problem. Now all we have to do is re-arrange the data.

### Reshaping the data

To reshape the data, we strongly encourage drawing out what one wants it terms of one input record and one output record. With `cdata` doing so essentially solves the problem.

So let's look at what happens only to the rows with `ID == 1`. In this case we expect input rows that look like this:

|   ID| DATE                | OP  |  rank|
|----:|:--------------------|:----|-----:|
|    1| 2001-01-02 00:00:00 | A   |     1|
|    1| 2015-04-25 00:00:00 | B   |     2|

And we want this record transformed into this:

|   ID| DATE1               | OP1 | DATE2               | OP2 | DATE3 | OP3 |
|----:|:--------------------|:----|:--------------------|:----|:------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B   | NA    | NA  |

We call the above record form a *row record*, because all the data for a given `ID` is in a single row. When the data for a given `ID` is not in a single row, we say it is in a *block*. In addition to having a per-record key (`ID` in our example), each row of a block is uniquely identified by an in-record structure key (in this case, `rank`).

`cdata` moves records from row shaped to block shaped, and vice-versa (It can also move data from one block shape to another, by going through a row).

To use `cdata`, draw a picture of any block record in its full generality. In our case the interesting record is the input shape, which looks like the following (with the record `ID` columns suppressed for conciseness).

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
|:-----|:------|:----|
| 1    | DATE1 | OP1 |
| 2    | DATE2 | OP2 |
| 3    | DATE3 | OP3 |

The column names `rank`, `DATE`, and `OP` are all column names of the table we are starting with. The values `1`, `2`, and `3` are all values we expect to see in the `rank` column of the working data frame. And the symbols `DATE1`, `DATE2`, `DATE3`, `OP1`, `OP2`, and `OP3` are all stand-in names for values we see in our data. These symbols will be the column names of our new row-records.

By default, the first column of a diagram is the in-record key (that is why we put `rank` first). However, any set of columns can be specified as the in-record keys through the package interfaces.

We have tutorials on how to build these diagrams [here](https://winvector.github.io/cdata/articles/design.html) and [here](https://winvector.github.io/cdata/articles/blocksrecs.html). Essentially we draw one record of the input and output and match column names to stand-in interior values of the other. The output record is a single row, so we don't have to explicitly pass it in. However it looks like the following.

``` r
row_record <- wrapr::qchar_frame(
  "DATE1", "OP1", "DATE2", "OP2", "DATE3", "OP3" |
   DATE1 ,  OP1 ,  DATE2 ,  OP2 ,  DATE3 ,  OP3  )

knitr::kable(row_record)
```

| DATE1 | OP1 | DATE2 | OP2 | DATE3 | OP3 |
|:------|:----|:------|:----|:------|:----|
| DATE1 | OP1 | DATE2 | OP2 | DATE3 | OP3 |

Notice the interior-data portions (the parts we wrote in the inputs as unquoted) of each table input are the cells that are matched from one record to the other. These are in fact just the earlier sample inputs and outputs with the values replaced with the placeholders `DATE1`, `DATE2`, `DATE3`, `OP1`, `OP2`, and `OP3`.

With the diagram in hand we can specify the data reshaping step. Since we are moving the data from blocks to row records, we use the function `blocks_to_rowrecs_spec` to create the reshaping transform.

``` r
transform <- blocks_to_rowrecs_spec(
  controlTable = diagram, # data frame describing the block
  recordKeys = 'ID')
```

The transform specifies that records are found in the format shown in `diagram`, and are to be converted to rows. We can confirm the intent by printing the transform.

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

If we apply this transform to the intermediate table `d2`, we have the data in the format we need (except possibly for the order of `ID`).

``` r
d2 %.>% # this MUST be the wrapr dot-pipe
  transform %.>%
  knitr::kable(.)
```

|   ID| DATE1               | OP1 | DATE2               | OP2  | DATE3               | OP3 |
|----:|:--------------------|:----|:--------------------|:-----|:--------------------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B    | NA                  | NA  |
|    2| 2000-04-01 00:00:00 | A   | NA                  | NA   | NA                  | NA  |
|    3| 2014-04-07 00:00:00 | D   | NA                  | NA   | NA                  | NA  |
|    4| 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B, D | 2012-12-01 00:00:00 | C   |
|    5| 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A    | NA                  | NA  |
|    6| 2004-01-09 00:00:00 | B   | NA                  | NA   | NA                  | NA  |

``` r
# if you prefer not to use a pipe:
# layout_by(transform, d2)
```

### The full transformation

We are now ready to put all of our operations together into one composite pipeline, starting from a specification of the original data `d`.

``` r
# specify the operations 
ops <- local_td(d) %.>%
  project(.,  # fuse all the ops on same date/id into one string
          OP := concat_values(OP),
          groupby = c("ID", "DATE")) %.>%
  extend(.,  # rank each ID group in order of date
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

|   ID| DATE1               | OP1 | DATE2               | OP2  | DATE3               | OP3 |
|----:|:--------------------|:----|:--------------------|:-----|:--------------------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B    | NA                  | NA  |
|    2| 2000-04-01 00:00:00 | A   | NA                  | NA   | NA                  | NA  |
|    3| 2014-04-07 00:00:00 | D   | NA                  | NA   | NA                  | NA  |
|    4| 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B, D | 2012-12-01 00:00:00 | C   |
|    5| 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A    | NA                  | NA  |
|    6| 2004-01-09 00:00:00 | B   | NA                  | NA   | NA                  | NA  |

And we are done.

A variation
-----------

If we had not wanted to merge ties, the solution would look like this:

``` r
# specify the operations 
ops <- local_td(d) %.>%
  extend(., # now we have to order by Date AND op
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

|   ID| DATE1               | OP1 | DATE2               | OP2 | DATE3               | OP3 |
|----:|:--------------------|:----|:--------------------|:----|:--------------------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B   | NA                  | NA  |
|    2| 2000-04-01 00:00:00 | A   | NA                  | NA  | NA                  | NA  |
|    3| 2014-04-07 00:00:00 | D   | NA                  | NA  | NA                  | NA  |
|    4| 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B   | 2009-01-20 00:00:00 | D   |
|    5| 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A   | NA                  | NA  |
|    6| 2004-01-09 00:00:00 | B   | NA                  | NA  | NA                  | NA  |

Currently in `rquery`/`R` all of the steps except the `concat_values` step are easy to translate into `SQL`. So this variation (which does not include the problematic aggregation step) is easy to translate into `SQL` for use in databases.

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

|   ID| DATE1               | DATE2               | DATE3               | OP1 | OP2 | OP3 |
|----:|:--------------------|:--------------------|:--------------------|:----|:----|:----|
|    1| 2001-01-02 00:00:00 | 2015-04-25 00:00:00 | NA                  | A   | B   | NA  |
|    2| 2000-04-01 00:00:00 | NA                  | NA                  | A   | NA  | NA  |
|    3| 2014-04-07 00:00:00 | NA                  | NA                  | D   | NA  | NA  |
|    4| 2005-06-16 00:00:00 | 2009-01-20 00:00:00 | 2009-01-20 00:00:00 | A   | B   | D   |
|    5| 2003-11-09 00:00:00 | 2010-10-10 00:00:00 | NA                  | B   | A   | NA  |
|    6| 2004-01-09 00:00:00 | NA                  | NA                  | B   | NA  | NA  |

Note: column order is not considered essential in `rquery` pipelines (though it is easy to fix once you are in R).

``` r
res_db <- res_db[qc(ID, DATE1, OP1, DATE2, OP2, DATE3, OP3)]

knitr::kable(res_db)
```

|   ID| DATE1               | OP1 | DATE2               | OP2 | DATE3               | OP3 |
|----:|:--------------------|:----|:--------------------|:----|:--------------------|:----|
|    1| 2001-01-02 00:00:00 | A   | 2015-04-25 00:00:00 | B   | NA                  | NA  |
|    2| 2000-04-01 00:00:00 | A   | NA                  | NA  | NA                  | NA  |
|    3| 2014-04-07 00:00:00 | D   | NA                  | NA  | NA                  | NA  |
|    4| 2005-06-16 00:00:00 | A   | 2009-01-20 00:00:00 | B   | 2009-01-20 00:00:00 | D   |
|    5| 2003-11-09 00:00:00 | B   | 2010-10-10 00:00:00 | A   | NA                  | NA  |
|    6| 2004-01-09 00:00:00 | B   | NA                  | NA  | NA                  | NA  |

Also, the entire query *can* be run in a database with the correct user-specified aggregation function. We have a demonstration of this in our `Python` version of this example.
