

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
    * using R Under development (unstable) (2019-03-22 r76262)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.7'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
    
    rhub::check_for_cran()
    458#> * using R Under development (unstable) (2019-03-09 r76216)
    459#> * using platform: x86_64-w64-mingw32 (64-bit)
    460#> * using session charset: ISO8859-1
    461#> * using option '--as-cran'
    462#> * checking for file 'cdata/DESCRIPTION' ... OK
    463#> * checking extension type ... Package
    464#> * this is package 'cdata' version '1.0.7'
    465#> * package encoding: UTF-8
    466#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    467#> Maintainer: 'John Mount '
    521#> Status: OK

### Linux

    rhub::check_for_cran()
    1163#> * using R version 3.5.3 (2019-03-11)
    1164#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1165#> * using session charset: UTF-8
    1166#> * using option ‘--as-cran’
    1167#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1168#> * checking extension type ... Package
    1169#> * this is package ‘cdata’ version ‘1.0.7’
    1170#> * package encoding: UTF-8
    1171#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1172#> Maintainer: ‘John Mount ’
    1226#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md
    ## WVPlots_1.0.9 started at 2019-03-23 09:18:51 success at 2019-03-23 09:19:51 (1/0/0)
    ## Test of cdata had 1 successes, 0 failures, and 0 skipped packages. 

