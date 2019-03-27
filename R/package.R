#' \code{cdata}: Fluid Data Transformations.
#'
#' Supplies implementations of higher order "fluid data" transforms.  These
#' transforms move data between rows and columns, are controlled by a graphical
#' transformation specification, and have pivot and un-pivot as special cases.
#' Large scale implementation is based on 'rquery', so should be usable on
#' 'SQL' compliant data sources (include large systems such as 'PostgreSQL' and
#' 'Spark').
#' This package introduces the idea of  control table specification of data transforms (later aslo adapted from 'cdata' by 'tidyr').
#' A theory of fluid data transforms can be found in the following articles:
#' \url{http://winvector.github.io/FluidData/FluidDataReshapingWithCdata.html},
#' \url{https://github.com/WinVector/cdata} and \url{https://winvector.github.io/FluidData/FluidData.html}.
#'
#'
#' @docType package
#' @name cdata
NULL

# make sure dot doesn't look like an unbound ref
. <- NULL

#' @importFrom wrapr mk_tmp_name_source
NULL
