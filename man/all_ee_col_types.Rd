% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_principle_datasets.R
\name{all_ee_col_types}
\alias{all_ee_col_types}
\title{all_ee_col_types}
\usage{
all_ee_col_types(date)
}
\arguments{
\item{date}{the date on which the All Employees report was run from Banner.
This is necessary because the columns were expanded on 12/15/2017}
}
\value{
a named list of vectors assigning each column to a class by column
name.
}
\description{
A named list of vectors of column names specifying their type as numeric or
character. Additional date formating is necessary after loading. This column
specification ensures that leading zeros are never dropped from certain
fields such as GID or Zip Code.
}
