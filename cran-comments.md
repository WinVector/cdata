

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.2.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    415#> * using R Under development (unstable) (2018-09-27 r75377)
    416#> * using platform: x86_64-w64-mingw32 (64-bit)
    417#> * using session charset: ISO8859-1
    418#> * using option '--as-cran'
    419#> * checking for file 'cdata/DESCRIPTION' ... OK
    420#> * checking extension type ... Package
    421#> * this is package 'cdata' version '1.0.2'
    422#> * package encoding: UTF-8
    423#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    424#> Maintainer: 'John Mount '
    439#> * checking top-level files ... WARNING
    440#> Conversion of 'README.md' failed:
    441#> pandoc.exe: Could not fetch https://raw.githubusercontent.com/WinVector/cdata/master/tools/cdata.png
    442#> no store
    483#> Status: 1 WARNING
    URL is good, warning is spurious.

### Linux

    rhub::check_for_cran()
    1347#> * using R version 3.4.4 (2018-03-15)
    1348#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1349#> * using session charset: UTF-8
    1350#> * using option ‘--as-cran’
    1351#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1352#> * checking extension type ... Package
    1353#> * this is package ‘cdata’ version ‘1.0.2’
    1354#> * package encoding: UTF-8
    1355#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1356#> Maintainer: ‘John Mount ’
    1409#> Status: OK

    rhub::check_for_cran()
    2279#> * using R Under development (unstable) (2018-10-05 r75407)
    2280#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2281#> * using session charset: UTF-8
    2282#> * using option ‘--as-cran’
    2283#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    2284#> * checking extension type ... Package
    2285#> * this is package ‘cdata’ version ‘1.0.2’
    2286#> * package encoding: UTF-8
    2287#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    2288#> Maintainer: ‘John Mount ’
    2343#> Status: OK

## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes

    