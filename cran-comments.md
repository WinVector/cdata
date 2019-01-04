

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.4.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ...
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    rhub::check_for_cran()
    447#> * using R Under development (unstable) (2018-12-26 r75909)
    448#> * using platform: x86_64-w64-mingw32 (64-bit)
    449#> * using session charset: ISO8859-1
    450#> * using option '--as-cran'
    451#> * checking for file 'cdata/DESCRIPTION' ... OK
    452#> * checking extension type ... Package
    453#> * this is package 'cdata' version '1.0.4'
    454#> * package encoding: UTF-8
    455#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    456#> Maintainer: 'John Mount '
    510#> Status: OK

    devtools::build_win()
    (down)
    
### Linux

    R CMD check --as-cran cdata_1.0.4.tar.gz 
    * using R version 3.5.1 (2018-07-02)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
    
