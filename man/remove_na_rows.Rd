% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{remove_na_rows}
\alias{remove_na_rows}
\title{remove_na_rows}
\usage{
remove_na_rows(df)
}
\arguments{
\item{df}{the dataframe or datatable from which to remove columns}
}
\value{
a datatable with NA columns removed
}
\description{
remove columns from dataframe if they contain \emph{only} NA values
}
