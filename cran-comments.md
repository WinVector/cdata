

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.8.tar.gz 



### Windows

    devtools::build_win()
 
    
    rhub::check_for_cran()
 

### Linux

    rhub::check_for_cran()


## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md
    ## WVPlots_1.0.9 started at 2019-03-29 07:23:13 success at 2019-03-29 07:24:14 (1/0/0)
    ## Test of cdata had 1 successes, 0 failures, and 0 skipped packages. 

