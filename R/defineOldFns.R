

#' Load old (0.5.1) functions into environment, to help code that has not yet adapted.
#'
#' @param env environment to define functions in
#' @return list of mappings performed
#'
#' @export
#'
define_old_cdata_functions <- function(env = parent.frame()) {
  mapping <- list(
    "buildUnPivotControlTable" = "build_unpivot_control",
    "moveValuesToRowsN" = "rowrecs_to_blocks_q",
    "moveValuesToRowsD" = "rowrecs_to_blocks",
    "buildPivotControlTableN" = "build_pivot_control_q",
    "buildPivotControlTableD" = "build_pivot_control",
    "moveValuesToColumnsN" = "blocks_to_rowrecs_q",
    "moveValuesToColumnsD" = "blocks_to_rowrecs",
    "unpivotValuesToRows" = "unpivot_to_blocks",
    "pivotValuesToColumns" = "pivot_to_rowrecs"
  )
  pkgenv <- as.environment("package:cdata")
  for(oi in names(mapping)) {
    ni <- mapping[[oi]]
    fi <- get(ni, envir = pkgenv)
    assign(oi, fi, envir = env)
  }
  mapping
}
