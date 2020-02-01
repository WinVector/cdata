

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.1.6.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.6’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

     rhub::check_for_cran()
     688#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     689#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     690#> setting R_REMOTES_STANDALONE to true
     691#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     692#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     693#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     694#> * using log directory 'C:/Users/USERFpTuGHdPvk/cdata.Rcheck'
     695#> * using R Under development (unstable) (2020-01-22 r77697)
     696#> * using platform: x86_64-w64-mingw32 (64-bit)
     697#> * using session charset: ISO8859-1
     698#> * using option '--as-cran'
     699#> * checking for file 'cdata/DESCRIPTION' ... OK
     700#> * checking extension type ... Package
     701#> * this is package 'cdata' version '1.1.6'
     702#> * package encoding: UTF-8
     703#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     704#> Maintainer: 'John Mount '
     760#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md


# Notes

    "Zumel" and "coordinatized" are not misspelled.

