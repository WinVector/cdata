

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.5.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ...
     Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    rhub::check_for_cran()
    448#> * using R Under development (unstable) (2018-12-26 r75909)
    449#> * using platform: x86_64-w64-mingw32 (64-bit)
    450#> * using session charset: ISO8859-1
    451#> * using option '--as-cran'
    452#> * checking for file 'cdata/DESCRIPTION' ... OK
    453#> * checking extension type ... Package
    454#> * this is package 'cdata' version '1.0.5'
    455#> * package encoding: UTF-8
    456#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    457#> Maintainer: 'John Mount '
    511#> Status: OK
 
    devtools::build_win()
    * using R Under development (unstable) (2019-01-18 r75994)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.5'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

### Linux

    R CMD check --as-cran cdata_1.0.5.tar.gz 
    rhub::check_for_cran()
    1398#> * using R version 3.4.4 (2018-03-15)
    1399#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1400#> * using session charset: UTF-8
    1401#> * using option ‘--as-cran’
    1402#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1403#> * checking extension type ... Package
    1404#> * this is package ‘cdata’ version ‘1.0.5’
    1405#> * package encoding: UTF-8
    1406#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1407#> Maintainer: ‘John Mount ’
    1460#> Status: OK

## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes


