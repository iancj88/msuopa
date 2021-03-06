% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_connections.R
\name{get_banner_conn}
\alias{get_banner_conn}
\title{get_banner_conn}
\usage{
get_banner_conn()
}
\value{
a connection object that can be used to exectute operations on the db
}
\description{
Connects to the MSU Production Oracle Banner Database.
Requires valid Banner username and password credentials.
Depends on ROracle to create Oracle database driver (aka OraDriver)
}
\author{
Ian C Johnson
}
