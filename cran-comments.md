

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.9.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.9’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
 


## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md
    ## WVPlots_1.1.0 started at 2019-04-15 16:42:44 success at 2019-04-15 16:43:39 (1/0/0)
    ## Test of cdata had 1 successes, 0 failures, and 0 skipped packages. 

# Notes

    "Zumel" and "coordinatized" are not misspelled.

