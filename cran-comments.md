

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.1.tar.gz 

    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers

    Status: OK


### Windows

    rhub::check_for_cran()
    
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * using option '--as-cran'
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.1'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount '
    Conversion of 'README.md' failed:
    pandoc.exe: Could not fetch https://raw.githubusercontent.com/WinVector/cdata/master/tools/cdata.png
    Status: 1 WARNING
    
    Warning is spurious, URL is good.
 

    devtools::build_win()
    
    * using R Under development (unstable) (2018-09-20 r75339)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'cdata/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'cdata' version '1.0.1'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'

    Status: OK

    
### Linux

    rhub::check_for_cran()
    
    * using R version 3.4.4 (2018-03-15)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘cdata/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘cdata’ version ‘1.0.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount ’
    Status: OK


## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
    Checking 1 packages: WVPlots
    Checked WVPlots: 0 errors | 0 warnings | 0 notes
