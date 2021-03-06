% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{make_year_day_seq}
\alias{make_year_day_seq}
\title{make_year_day_seq}
\usage{
make_year_day_seq(year, by = "day")
}
\arguments{
\item{year}{a 4 digit integer specifying the year of character dates between
1900 and 2100}

\item{by}{a string containing one of the following: "day", "week", "month",
"quarter", "year" OR a number speficifying the day frequency of the date
sequence}
}
\value{
a character vector containg dates in the form YYYY-MM-DD
}
\description{
make_year_day_seq
}
\details{
create a character vector containing dates in the form "YYYY-MM-DD". Useful
for incrementing banner queries by fixed dates.
}
