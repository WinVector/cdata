

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.8.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows


    devtools::check_win_devel()
    * using R Under development (unstable) (2020-08-18 r79041)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.1.8'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK
    
    rhub::check_for_cran()
    482#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    483#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    484#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    485#> setting R_REMOTES_STANDALONE to true
    486#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    487#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    488#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    489#> * using log directory 'C:/Users/USERzbcYMKkNet/cdata.Rcheck'
    490#> * using R Under development (unstable) (2020-07-05 r78784)
    491#> * using platform: x86_64-w64-mingw32 (64-bit)
    492#> * using session charset: ISO8859-1
    493#> * using option '--as-cran'
    494#> * checking for file 'cdata/DESCRIPTION' ... OK
    495#> * checking extension type ... Package
    496#> * this is package 'cdata' version '1.1.8'
    497#> * package encoding: UTF-8
    498#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    499#> Maintainer: 'John Mount '
    ...
    555#> Status: OK


## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md

# Notes

    "Zumel" and "coordinatized" are not misspelled.

