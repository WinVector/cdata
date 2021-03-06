% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_keying.R
\name{rows_are_uniquely_keyed}
\alias{rows_are_uniquely_keyed}
\title{Check if table rows are uniquely keyed by keyset.}
\usage{
rows_are_uniquely_keyed(table_rep, key_columns, db)
}
\arguments{
\item{table_rep}{rquery op_tree}

\item{key_columns}{character vector names of key columns}

\item{db}{database handle}
}
\value{
TRUE if table rows are uniquely keyed by key columns
}
\description{
Return TRUE if table rows are uniquely keyed by key_columns.
}
\keyword{internal}
