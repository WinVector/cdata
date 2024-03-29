check_reverse_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "cdata"
packageVersion(package)
```

    ## [1] '1.2.1'

``` r
date()
```

    ## [1] "Sat Aug 19 16:22:57 2023"

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

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//RtmpZWTbbQ"

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

    ## ## Reverse depends check of cdata 1.2.1 
    ## WVPlots_1.3.5 started at 2023-08-19 16:22:58 success at 2023-08-19 16:23:56 (1/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of cdata 1.2.1 had 1 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2023-08-19 16:22:58 to 2023-08-19 16:23:56 for 58 secs 
    ## Average of 58 secs relative to 57.286 secs using 1 runners
    ## 
    ## Failed packages:   
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
