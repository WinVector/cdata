

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.3.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2019-10-26 r77334)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.1.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

### Linux

    rhub::check_for_cran()
    1437#> * using R version 3.6.1 (2019-07-05)
    1438#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1439#> * using session charset: UTF-8
    1440#> * using option ‘--as-cran’
    1441#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1442#> * checking extension type ... Package
    1443#> * this is package ‘cdata’ version ‘1.1.3’
    1444#> * package encoding: UTF-8
    1445#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1446#> Maintainer: ‘John Mount ’
    1501#> Status: OK
    
    rhub::check_for_cran()
    1464#> * using R Under development (unstable) (2019-10-26 r77334)
    1465#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1466#> * using session charset: UTF-8
    1467#> * using option ‘--as-cran’
    1468#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1469#> * checking extension type ... Package
    1470#> * this is package ‘cdata’ version ‘1.1.3’
    1471#> * package encoding: UTF-8
    1472#> * checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
    1473#> NOTE
    1474#> Maintainer: ‘John Mount ’
    1475#> Possibly mis-spelled words in DESCRIPTION:
    1476#> coordinatized (15:36)
    1533#> Status: 1 NOTE
    "Zumel" and "coordinatized" are not misspelled.

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md


# Notes

    "Zumel" and "coordinatized" are not misspelled.

