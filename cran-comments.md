

## Test environments

### OSX
   

    R CMD check --as-cran cdata_1.1.9.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/cdata.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.1.9’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows


    devtools::check_win_devel()
    Error in curl::curl_fetch_memory(url, handle = h) : 
      Timeout was reached: [win-builder.r-project.org] FTP response timeout
    
    rhub::check_for_cran()
    778#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    779#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    780#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    781#> setting R_REMOTES_STANDALONE to true
    782#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    783#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    784#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    785#> * using log directory 'C:/Users/USERiBvfkuWdNX/cdata.Rcheck'
    786#> * using R Under development (unstable) (2020-10-09 r79317)
    787#> * using platform: x86_64-w64-mingw32 (64-bit)
    788#> * using session charset: ISO8859-1
    789#> * using option '--as-cran'
    790#> * checking for file 'cdata/DESCRIPTION' ... OK
    791#> * checking extension type ... Package
    792#> * this is package 'cdata' version '1.1.9'
    793#> * package encoding: UTF-8
    794#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    795#> Maintainer: 'John Mount '
    851#> Status: OK

## Downstream dependencies

    Checked all declared dependencies
    https://github.com/WinVector/cdata/blob/master/extras/check_reverse_dependencies.md

# Notes

    "Zumel" and "coordinatized" are not misspelled.

