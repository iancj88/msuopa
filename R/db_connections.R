#' get_banner_conn
#'
#' @description Connects to the MSU Production Oracle Banner Database.
#' Requires valid Banner username and password credentials.
#' Depends on ROracle to create Oracle database driver (aka OraDriver)
#'
#' @return a connection object that can be used to exectute operations on the db
#' @export
#' @author Ian C Johnson
#'
get_banner_conn <- function() {

  username <- rstudioapi::showPrompt(title = "BANNER User Name",
                                     message = "Enter user name here: ",
                                     default = "IJOHNSON")
  pwd <- rstudioapi::askForPassword(prompt = "Enter BANNER password here: ")

  ora_conn_drvr <- ROracle::Oracle()
  db_conn <- ROracle::dbConnect(ora_conn_drvr,
                                username,
                                pwd,
                                dbname = "PROD")
  rm(pwd)
  return(db_conn)
}

#' get_access_conn
#'
#' get a connection to local or network stored access db. The connection allows
#' for operations to be made on the access database.
#'
#' @param db_file_path A full file path to the access database including the
#' file name.
#'
#' @return a connection object that can be used to exectute operations on the db
#' @export
get_access_conn <- function(db_file_path) {

  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)
  myconn <- DBI::dbConnect(odbc::odbc(),
                      .connection_string = db_connect_string)
  return(myconn)
}