check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "cdata"
packageVersion(package)
```

    ## [1] '1.2.0'

``` r
date()
```

    ## [1] "Fri Jun 11 15:40:02 2021"

``` r
parallelCluster <- NULL
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
#ncores <- parallel::detectCores()
#parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/cdata/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//Rtmph5ZHwc"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)

mk_fn <- function(package, directory) {
  force(package)
  force(directory)
  function(i) {
    library("prrd")
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## ## Reverse depends check of cdata 1.2.0 
    ## rmoo_0.1.6 started at 2021-06-11 15:40:04 failure at 2021-06-11 15:40:08 (0/0/1) 
    ## WVPlots_1.3.2 started at 2021-06-11 15:40:08 success at 2021-06-11 15:41:04 (1/0/1)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of cdata 1.2.0 had 1 successes, 1 failures, and 0 skipped packages. 
    ## Ran from 2021-06-11 15:40:04 to 2021-06-11 15:41:04 for 1 mins 
    ## Average of 30 secs relative to 30.001 secs using 1 runners
    ## 
    ## Failed packages:  rmoo 
    ## 
    ## Skipped packages:   
    ## 
    ## None still working
    ## 
    ## None still scheduled

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
