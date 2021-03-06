% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_export.R
\name{check_fix_dupe_name}
\alias{check_fix_dupe_name}
\title{check_fix_dupe_name checks a string vector for the existence of a particular
string. If found, it modifies the query string so that it does not match an
existing string int he vector. It does this be appending an underscore and
numeric value.}
\usage{
check_fix_dupe_name(name, curr_names)
}
\arguments{
\item{name}{the single string name that will be searched for and modified if
necessary}

\item{curr_names}{the vector of strings that will be searched for the single
string 'name'.}
}
\value{
if necessary, a modified string that is not duplicated in the input
vector. otherwise, the input name parameter
}
\description{
check_fix_dupe_name checks a string vector for the existence of a particular
string. If found, it modifies the query string so that it does not match an
existing string int he vector. It does this be appending an underscore and
numeric value.
}
