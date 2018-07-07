perf
================
Win-Vector LLC
4/12/2018

``` r
library("microbenchmark")
library("ggplot2")
library("tidyr")
library("cdata")
```

``` r
set.seed(3525)
mkData <- function(nrow, ncol) {
  dWide <- data.frame(id = seq_len(nrow))
  for(ci in seq_len(ncol)) {
    cn <- paste("col", ci, sep = "_")
    dWide[[cn]] <- runif(nrow)
  }
  dWide
}

for(nrow in c(100, 1000, 10000)) {
  for(ncol in c(10, 100)) {
    dWide <- mkData(nrow, ncol)
    # unpivot in cdata notation
    # actually rowrecs_to_blocks() is the preferend notation 
    # and unpivot_to_blocks() is a convenience
    dTall_cdata <- unpivot_to_blocks(dWide, 
                                     nameForNewKeyColumn = "col_key",
                                     nameForNewValueColumn = "col_val",
                                     columnsToTakeFrom <- setdiff(colnames(dWide), "id"))
    dTall_cdata <- dTall_cdata[order(dTall_cdata$id, dTall_cdata$col_key), , drop = FALSE]
    rownames(dTall_cdata) <- NULL
    dTall <- dTall_cdata
    
    # unpivot in tidyr notation
    dTall_tidyr <- gather(dWide, key = "col_key", value= "col_val", -id)
    dTall_tidyr <- dTall_tidyr[order(dTall_tidyr$id, dTall_tidyr$col_key), , drop = FALSE]
    rownames(dTall_tidyr) <- NULL
    
    # check
    testthat::expect_equal(dTall_cdata, dTall_tidyr)
    
    # time
    unpivot_cdata <- function() {
      nrow(unpivot_to_blocks(dWide, 
                             nameForNewKeyColumn = "col_key",
                             nameForNewValueColumn = "col_val",
                             columnsToTakeFrom <- setdiff(colnames(dWide), "id")))
    }
    unpivot_tidyr <- function() {
      nrow(gather(dWide, key = "col_key", value= "col_val", -id))
    }
    
    
    t_unpivot <- microbenchmark(unpivot_cdata(), unpivot_tidyr())
    #print(t_unpivot)
    #autoplot(t_unpivot)
    print(WVPlots::ScatterBoxPlotH(t_unpivot,  "time", "expr",
                                   paste0("un-pivot times (NS) (rows=",
                                          nrow, 
                                          ", cols=",
                                          ncol,
                                          ")")))
    
    # pivot cdata notation
    # actually blocks_to_rowrecs() is the preferend notation 
    # and pivot_to_rowrecs() is a convenience
    res_cdata <- pivot_to_rowrecs(dTall, 
                                  columnToTakeKeysFrom = "col_key",
                                  columnToTakeValuesFrom = "col_val",
                                  rowKeyColumns = "id")
    res_cdata <- res_cdata[order(res_cdata$id), , drop = FALSE]
    res_cdata <- res_cdata[, colnames(dWide), drop = FALSE]
    rownames(res_cdata) <- NULL
    testthat::expect_equal(dWide, res_cdata)
    
    # pivot tidyr notation
    res_tidyr <- spread(dTall, "col_key", -id)
    res_tidyr <- res_tidyr[order(res_tidyr$id), , drop = FALSE]
    res_tidyr <- res_tidyr[, colnames(dWide), drop = FALSE]
    rownames(res_tidyr) <- NULL
    testthat::expect_equal(dWide, res_tidyr)
    
    # time
    pivot_cdata <- function() {
      nrow(pivot_to_rowrecs(dTall, 
                            columnToTakeKeysFrom = "col_key",
                            columnToTakeValuesFrom = "col_val",
                            rowKeyColumns = "id"))
    }
    pivot_tidyr <- function() {
      nrow(spread(dTall, "col_key", -id))
    }
    
    t_pivot <- microbenchmark(pivot_cdata(), pivot_tidyr())
    #print(t_pivot)
    #autoplot(t_pivot)
    print(WVPlots::ScatterBoxPlotH(t_pivot,  "time", "expr", 
                                   paste0("pivot times (NS) (rows=",
                                          nrow, 
                                          ", cols=",
                                          ncol,
                                          ")")))
  }
}
```

![](perf_files/figure-markdown_github/exp-1.png)![](perf_files/figure-markdown_github/exp-2.png)![](perf_files/figure-markdown_github/exp-3.png)![](perf_files/figure-markdown_github/exp-4.png)![](perf_files/figure-markdown_github/exp-5.png)![](perf_files/figure-markdown_github/exp-6.png)![](perf_files/figure-markdown_github/exp-7.png)![](perf_files/figure-markdown_github/exp-8.png)![](perf_files/figure-markdown_github/exp-9.png)![](perf_files/figure-markdown_github/exp-10.png)![](perf_files/figure-markdown_github/exp-11.png)![](perf_files/figure-markdown_github/exp-12.png)
