#' \code{cdata}: Fluid Data Transformations.
#'
#' Supplies implementations of higher order "fluid data" transforms.  These'
#' transforms move data between rows and columns, are controlled by a graphical
#' transformation specification, and have pivot and un-pivot as special cases.
#' Implementation is based on 'DBI' and 'RSQLite', so should be usable on
#' 'DBI' compliant data sources (include large systems such as 'PostgreSQL' and
#' 'Spark').  Convenience adapters are provided for in-memory 'data.frame's.
#' A theory of fluid data transforms can be found in the following articles:
#' \url{https://github.com/WinVector/cdata} and \url{https://winvector.github.io/FluidData/FluidData.html}.
#'
#'
#' @docType package
#' @name cdata
NULL

# make sure dot doesn't look like an unbound ref
. <- NULL

#' @importFrom wrapr grepdf
#' @export
wrapr::grepdf
