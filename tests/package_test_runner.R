
# package to check
pkg = "cdata"

# This file is distributed without license requirements, feel free to alter/copy.
if(requireNamespace("RUnit", quietly = TRUE)) {
  # library("RUnit") # uncomment this if you want RUnit attached during testing
  library(pkg, character.only = TRUE)
  cdata::run_cdata_tests(verbose = TRUE, require_RUnit_attached = FALSE)
}
