

## Test environments

    * OSX
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * win-builder 
    * using R Under development (unstable) (2018-09-07 r75257)
    * using platform: x86_64-w64-mingw32 (64-bit)

## Test rsults

    R CMD check --as-cran cdata_1.0.0.tar.gz 
    
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Downstream dependencies

Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
