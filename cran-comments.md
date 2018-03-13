
Maintinance release.  Add DOI to documentation, use newer wrapr methods,
minor bug fixes.

## Test environments

  * OSX
  * using R version 3.4.3 (2017-11-30)
  * using platform: x86_64-apple-darwin15.6.0 (64-bit)

  * win-builder 
 

## R CMD check --as-cran cdata_0.6.0.tar.gz

  * using option ‘--as-cran’
  * checking for file ‘cdata/DESCRIPTION’ ... OK
  * checking extension type ... Package
  * this is package ‘cdata’ version ‘0.6.0’
  * package encoding: UTF-8
  * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’

Status: OK

## Downstream dependencies

Checked all declared dependencies:

  devtools::revdep_check()
   Checking 1 packages: WVPlots
   Checked WVPlots: 0 errors | 0 warnings | 0 notes

