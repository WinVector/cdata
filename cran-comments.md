

Please consider this early update (7 updates in 6 months) for cdata. I found and fixed a serious but and would like users to not run into the issue.  I have added more tests to try and cut down on this sort of mistake.

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.3.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Number of updates in past 6 months: 7
    Status: 1 NOTE


### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-10-18 r75463)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Number of updates in past 6 months: 7
    Status: 1 NOTE
    
### Linux

    rhub::check_for_cran()
    1395#> * using R version 3.4.4 (2018-03-15)
    1396#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1397#> * using session charset: UTF-8
    1398#> * using option ‘--as-cran’
    1399#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1400#> * checking extension type ... Package
    1401#> * this is package ‘cdata’ version ‘1.0.3’
    1402#> * package encoding: UTF-8
    1403#> * checking CRAN incoming feasibility ... NOTE
    1404#> Maintainer: ‘John Mount ’
    1405#> Number of updates in past 6 months: 7
    1458#> Status: 1 NOTE

    rhub::check_for_cran()
    2322#> * using R Under development (unstable) (2018-10-14 r75439)
    2323#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2324#> * using session charset: UTF-8
    2325#> * using option ‘--as-cran’
    2326#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    2327#> * checking extension type ... Package
    2328#> * this is package ‘cdata’ version ‘1.0.3’
    2329#> * package encoding: UTF-8
    2330#> * checking CRAN incoming feasibility ... NOTE
    2331#> Maintainer: ‘John Mount ’
    2332#> Number of updates in past 6 months: 7
    2395#> Status: 1 WARNING, 1 NOTE

## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes

    