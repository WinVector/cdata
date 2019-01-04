

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.4.tar.gz 
 


### Windows

    devtools::build_win()
 
    
### Linux

    R CMD check --as-cran cdata_1.0.4.tar.gz 


## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()

    