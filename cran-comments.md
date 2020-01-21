

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.5.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2020-01-07 r77633)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.1.5'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

### Linux

    rhub::check_for_cran()
    1416#> * using R version 3.6.1 (2019-07-05)
    1417#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1418#> * using session charset: UTF-8
    1419#> * using option ‘--as-cran’
    1420#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1421#> * checking extension type ... Package
    1422#> * this is package ‘cdata’ version ‘1.1.5’
    1423#> * package encoding: UTF-8
    1424#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1425#> Maintainer: ‘John Mount ’
    1474#> OK
    
    rhub::check_for_cran()
    1596#> * using R Under development (unstable) (2020-01-18 r77674)
    1597#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1598#> * using session charset: UTF-8
    1599#> * using option ‘--as-cran’
    1600#> * checking for file ‘cdata/DESCRIPTION’ ... OK
    1601#> * checking extension type ... Package
    1602#> * this is package ‘cdata’ version ‘1.1.5’
    1603#> * package encoding: UTF-8
    1604#> * checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
    1605#> NOTE
    1606#> Maintainer: ‘John Mount ’
    1607#> Possibly mis-spelled words in DESCRIPTION:
    1608#> coordinatized (15:36)
    1665#> Status: 1 NOTE
    coordinatized is spelled correctly.

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md


# Notes

    "Zumel" and "coordinatized" are not misspelled.

