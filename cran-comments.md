

## Test environments

### OSX
   
    R CMD check --as-cran cdata_1.0.2.tar.gz 

  

### Windows

    rhub::check_for_cran()
    
 

    devtools::build_win()
    
  

    

## Downstream dependencies

    Checked all declared dependencies:

    devtools::revdep_check()
 