#' source_folder_files
#'
#' Source all files in a given folder for the current R session. Any file
#' ending with *.R will be sourced.
#'
#' @param folder_path The folder path containing the R files. By default, uses
#' the ./R/ folder contained in teh working directory
#'
#' @return Success message will be printed to terminal
#' @export
source_folder_files <- function(folder_path = "./R/") {
  # Load up the functions stored in the ./R/ folder
  file.sources <- list.files(path = folder_path,
                             pattern = "\\.R$",
                             full.names = TRUE)

  if (!length(file.sources)) {
    stop(simpleError(sprintf('No R Source files found')))
  }

  src <- invisible(lapply(file.sources, source))
  message(sprintf('%s files successfully sourced.', length(src)))
}

# my_query <- paste(readLines('./snapsht_qry.sql'), collapse='\n')
# results <- sqlQuery(tst, my_query)
#
# my_query_mod <- gsub("AS_OF_DATE_HERE", "20171231", my_query)
#' make_year_day_seq
#'
#' @param year an integer specifying the year of character dates
#' @param by a string containing one of the following: "day", "week", "month",
#' "quarter", "year" OR a number speficifying the frequency of the date sequence
#'
#' @return a character vector containg dates in the form YYYY-MM-DD
#' @export
make_year_day_seq <- function(year, by = "day") {
  # basic parameter checks
  stopifnot(nchar(year) == 4,
            year > 1900,
            year < 2100,
            by %in% c("day",
                      "week",
                      "month",
                      "quarter",
                      "year",
                      "days",
                      "weeks",
                      "months",
                      "quarters",
                      "years") | is.numeric(by))

  start_date <- paste0(year, "-01-01")
  start_date <- as.Date(start_date)
  end_date <- paste0(year, "-12-31")
  end_date <- as.Date(end_date)

  dte_seq <- seq.Date(start_date, end_date, by = by)
  dte_seq <- dte_seq[dte_seq <= Sys.Date()]
  dte_seq <- as.character(dte_seq)

  return(dte_seq)
}


#' fix_native_org_names
#'
#' This function overwrites certain Org Hierarchy values for oddly formatted
#' departments in the all employees report.
#'
#' @param df a dataframe containing all employees data.
#'
#' @return the input dataframe with certain Org Hierarchy values overwritten/
#' fixed
fix_native_org_names <- function(df) {
  stopifnot(c("Org. Heirarchy", "Department") %in% names(df))

  # overwrite various 'inconsistent' names for orgs
  isProvost <- (df$`Org. Heirarchy` == "Other Provost")
  df[isProvost,"Org. Heirarchy"] <- "Provost"

  isNursing <- (df$Department %in% c("College of Nursing Billings",
                                     "College of Nursing Great Falls",
                                     "College of Nursing Missoula"))
  df[isNursing, "Org. Heirarchy"] <- "Nursing"

  isExtended <- (df$Department %in% c("Extended University NTEN",
                                      "Extended University",
                                      "Extended University Director"))
  df[isExtended, "Org. Heirarchy"] <- "Extended University"

  isAg <- (df$Department == "AES EARC")
  df[isAg, "Org. Heirarchy"] <- "Agriculture"

  isPres <- (df$Department == "Museum of the Rockies")
  df[isPres, "Org. Heirarchy"] <- "President"

  isBusiness <- (df$Department == "TS College of Business")
  df[isBusiness, "Org. Heirarchy"] <- "College of Business"

  return(df)
}


#' compute_fiscal_year
#'
#' computes the fiscal year of a vector of dates based on the Montana State
#' fiscal calendar (July 1 - June 30)
#'
#' @param date the vector of dates from which to compute fiscal year
#'
#' @return the vector of year integers
#' @export
#'
#' @examples
#' dte_1 <- as.Date("2018-01-01")
#' dte_2 <- as.Date("2018-08-01")
#' compute_fiscal_year(dte_1)
#' compute_fiscal_year(dte_2)
compute_fiscal_year <- function(date) {

  qrtr <- data.table::quarter(date)
  third_fourth_quarter <- (qrtr > 2)

  date <- data.table::year(date)
  date[third_fourth_quarter] <- date[third_fourth_quarter] + 1

  return(date)
}

#' rename_column
#'
#' Quickly rename a column based on it's current name rather than location. This
#' is helpful in certain instances when a name of a column cannot be determined
#' in advance.
#'
#' @param df the dataframe containing columns to be renamed
#' @param old_name a string containing the name of the column to be renamed
#' @param new_name a string containing the new name.
#'
#' @return the same dataset with a renamed column
#' @export
rename_column <- function(df, old_name, new_name) {
  colnames(df)[which(names(df) == old_name)] <- new_name
  return(df)
}


#' load_sql_qry
#'
#' load a properly formatted sql query to be sent to an oracle db via RORacle or
#' an access db via DBI
#'
#' @param file_path the full folder path containing the file
#' @param file_name the full name of the file with extension
#'
#' @return a string containing the sql query
#' @export
load_sql_qry <- function(file_path, file_name) {
  full_path <- paste0(file_path, file_name)
  qry <- paste(readLines(full_path), collapse = '\n')
  return(qry)
}


#' format_banner_date
#'
#' reformat a banner date column to one interpretable with the base
#' \code{as.Date()} functiWon. Banner stores dates in the format YYYY-MM-DD
#' HH:MM:SS. For our purposes, the timestamp is irrelevant and subsequently
#' dropped.
#'
#' @param df_col a single column vector containing the banner date/timestamps
#'
#' @return the same column vector with the timestamps dropped and remaining
#' values loaded as date classes.
format_banner_date <- function(df_col) {
  # only select the date portion
  date_out <- stringr::str_sub(df_col, start = 1L, end = 10)
  date_out <- as.Date(date_out)
  return(date_out)
}

#' pad_gid
#'
#' add dropped zeros to gid values
#'
#' @param gid_vec the vector of gids to add gids
#'
#' @return
#' @export
#'
#' @examples
pad_gid <- function(gid_vec) {
  gid_length <- 9

  if (sum(nchar(gid_vec) > gid_length,  na.rm = TRUE) > 0) {
    return(gid_vec)
  }

  gid_numbers <- as.numeric(gid_vec)
  gid_vec <- sprintf("%09d", gid_numbers)
  return(gid_vec)
}

