

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.7.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    devtools::build_win()

### Linux

    R CMD check --as-cran cdata_1.0.6.tar.gz 



## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md
    ## WVPlots_1.0.9 started at 2019-03-23 09:18:51 success at 2019-03-23 09:19:51 (1/0/0)
    ## Test of cdata had 1 successes, 0 failures, and 0 skipped packages. 

