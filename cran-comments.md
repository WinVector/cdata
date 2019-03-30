

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.8.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    devtools::build_win()
    * using R version 3.5.3 (2019-03-11)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.8'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Possibly mis-spelled words in DESCRIPTION:
      coordinatized (15:36)
    Status: 1 NOTE
    "Zumel" and "coordinatized" are not misspelled.


    * using R version 3.6.0 alpha (2019-03-29 r76300)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.8'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Possibly mis-spelled words in DESCRIPTION:
      coordinatized (15:36)
    Status: 1 NOTE
    "Zumel" and "coordinatized" are not misspelled.


## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md
    ## WVPlots_1.0.9 started at 2019-03-29 07:23:13 success at 2019-03-29 07:24:14 (1/0/0)
    ## Test of cdata had 1 successes, 0 failures, and 0 skipped packages. 


# Notes

    "Zumel" and "coordinatized" are not misspelled.

