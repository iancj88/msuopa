#' get_banner_snapshots
#'
#'
#' @param year year overwhich snapshots will be pulled. For one date only, see
#' \code{debug_date}
#' @param sql_file_name optional file name and path if an alternative sql query
#' is needed. Defaults to OPA snapshot sql query.
#' @param frequency a character vector containing one of "day", "week", "month",
#' "quarter", or "year". Alternatively, may be a numeric integer. Used to
#' determine the date sequence from which the sql queries are built.
#' @param debug_date an optional date used to specify a single date to run for
#' a snapshot. Will supersede any values used in \code{year} or \code{by}
#' parameters
#'
#' @return a list of snapshot query returns. The names of the list items specify
#' the date on which query is set.
#' @export
get_banner_snapshots <- function(year,
                                 sql_file_name,
                                 by = "week",
                                 debug_date) {

  if (!missing(sql_file_name)) {
    # load the sql query file into a readable format to be passed to
    # the database
    sql_query <- paste(readLines(sql_file_name), collapse = '\n')
  } else {
    sql_query <- msuopa::snapshot_sql_query
  }

  good_frequencies <- c("day", "week", "month", "quarter", "year")
  if (!by %in% good_frequencies & !is.numeric(by)) {
    by <- "month"
    warning("'by' argument not acceptable.\n",
            '--Defaulting to "month".\n',
            "--Acceptable values include:", good_frequencies)
  }

  # create a list of sequential dates contained in the given year starting with
  # January first (YYYY-01-01) to December 31st (YYYY-12-31)
  if (!missing(debug_date)) {
    year_char_seq <- debug_date
  } else {
    year_char_seq <- make_year_day_seq(year, by)
  }

  # create a sequence of queries for each date.
  day_qries <- mapply(gsub,
                      replacement = year_char_seq,
                      MoreArgs = list(pattern = "AS_OF_DATE_HERE",
                                      x = sql_query))

  # make a banner connection
  banner_con <- get_banner_conn()

  # get a list of dataframes corresponding to the dates fed in as query
  # variables
  list_results <- mapply(ROracle::dbGetQuery,
                         statement = day_qries,
                         MoreArgs = list(conn = banner_con),
                         SIMPLIFY = FALSE)
  names(list_results) <- year_char_seq

  return(list_results)
}

#' get_all_ee
#'
#' Load one or more All Employee reports from csv source. Optionally, save the
#' compiled RDS file for faster loading in the future.
#'
#' @param folderpath the folderpath containing the csv files.
#' @param most_recent_only Boolean. Simplest way to specify loading the most recent csv
#' file only for performance reasons.
#' @param opt_start_date An optional start date if only certain files should be
#' loaded. If not specified and \code{most_recent_only == TRUE}, then will load
#' all files found in folderpath.
#' @param opt_end_date An optional end date if only certain files should be
#' loaded. If not specified and \code{most_recent_only == TRUE}, then will load
#' all files found in folderpath.
#' @param supplement Boolean value indicating if the dataframes should have
#' additional derived columns added. Examples of columns include EMR Job Type,
#' Fiscal Year, and Longevity Bonus.
#'
#' @return a single dataframe containing one or more all employees reports.
#' report data can be distinguished by the added 'date' column.
#' @export
get_all_ee <- function(folderpath,
                       most_recent_only = TRUE,
                       opt_start_date,
                       opt_end_date,
                       supplement = TRUE) {

  # check that the input filepath leads to a valid directory
  if (missing(folderpath)) {
    folderpath <- "X:/Employees/All EEs Reports/csv_src/"
  }
  stopifnot(dir.exists(folderpath))

  # check that the directory is not empty
  stopifnot(length(list.files(path = folderpath)) > 0)

  # check that the directory contains a .txt file for parsing
  file_names_only <- list.files(folderpath, full.names = FALSE)
  stopifnot(sum(grepl(".txt", file_names_only)) > 0)

  # get the full paths to the files in the directory so that each can be
  # fed into the csv reader one a time.
  file_names_paths <- list.files(folderpath, full.names = TRUE)

  # only keep those files that are .txt
  file_names_paths <- file_names_paths[grepl(".txt",
                                             file_names_only)]
  file_names_only <- file_names_only[grepl(".txt",
                                           file_names_only)]

  # if only the most recent all ee file is to be loaded (for performance
  # considerations), select the final file in the list. This will correspond
  # to the most 'recent' file because of the file name containing the date.
  if (most_recent_only == TRUE) {
    last_file_indx <- length(file_names_only)
    file_names_paths <- file_names_paths[last_file_indx]
    file_names_only <- file_names_only[last_file_indx]
    # otherwise filter the text file list to include those that fall
    # in the desired date range
  } else if ((most_recent_only == FALSE) &
             (!missing(opt_start_date)) &
             (!missing(opt_end_date))) {
    file_dates <- allee_dates_from_fnames(file_names_only)
    file_names_paths <- file_names_paths[((file_dates >= opt_start_date) &
                                            (file_dates <= opt_end_date))]

    file_names_only <- file_names_only[((file_dates >= opt_start_date) &
                                          (file_dates <= opt_end_date))]
  }

  # use data table fread (because it is fast) to load the csvs
  loaded_data <- mapply(read_allee_csv,
                        path = file_names_paths,
                        name = file_names_only,
                        SIMPLIFY = FALSE)

  # combine the loaded data and change to dataframe
  loaded_data <- dplyr::bind_rows(loaded_data)

  # save the RDS file a snapshot if the user requests it
  save_ques_dialog <- "Caution.\n\nContains sensitive information.\n\nSave to secure locations only"
  user_wants_to_save <- rstudioapi::showQuestion("Save All EE RDS File",
                                                 save_ques_dialog,
                                                 ok = "Save",
                                                 cancel = "Cancel")
  if (user_wants_to_save) {
    dir_select_capt <- "Select Location to Save All EE RDS File."
    user_selected_dir <- rstudioapi::selectDirectory(caption = dir_select_capt,
                                                     label = "Select",
                                                     path = "./")

    max_date <- max(loaded_data$date)

    rds_name <- paste0(user_selected_dir,
                       "/all_ee_compiled-",
                       max_date,
                       ".RDS")

    saveRDS(loaded_data, rds_name)
  }


  # Format the date columns out of their screwy dd-MMM-yy format
  loaded_data <- format_allEE_dates(loaded_data)
  loaded_data <- fix_native_org_names(loaded_data)
  if (supplement == TRUE) {
    loaded_data <- supplement_all_ee(loaded_data)
  }


  # if only a subset of the data is needed filter by the date
  # if ((most_recent_only == FALSE) &
  #     (!missing(opt_start_date)) &
  #     (!missing(opt_end_date))) {
  #   loaded_data <- filter(loaded_data,
  #                         date >= opt_start_date,
  #                         date <= opt_end_date)
  # }

  return(loaded_data)
}

#' allee_dates_from_fnames
#'
#' Extracts and formats the dates contained in the csv file names. The csv files
#' must be in the form "YYYYMMDD All Employees.txt".
#'
#' @param file_list a character vector containing the names of all the txt
#' files in the folder containing csv
#'
#' @return list of POSIXct dates contained in the input filenames
#'
#' @examples
allee_dates_from_fnames <- function(file_list) {
  date_from_file_name <- gsub(" All Employees.txt", "", file_list)
  date_from_file_name <-  as.POSIXct(date_from_file_name, format = "%Y%m%d")
  return(date_from_file_name)
}

#' read_allee_csv
#'
#' given a csv file containing the all employees report data, load it into a
#' dataframe. Uses data.table's \code{fread} function for performance reasons.
#' Column types are specified using \code{all_ee_col_types} function.
#'
#' @param path the full path the csv file
#' @param name the full name of the csv file
#'
#' @return an unnamed dataframe containing the all employees data
#'
#' @seealso allee_dates_from_fnames, all_ee_col_types, get_all_ee
read_allee_csv <- function(path, name) {
  # compute the date of the file to determine the column types
  # contained in it. This date will be placed into it's own column
  # after it is read into a data.table
  fname_date <- allee_dates_from_fnames(name)

  col_fread_types <- all_ee_col_types(date = fname_date)

  dt <- data.table::fread(path,
                          header = TRUE,
                          sep = ";",
                          colClasses = col_fread_types,
                          skip = 12)


  df <- data.table::setDF(dt)
  # place the date into it's own column. The date will be added as a name of the
  # df after being placed in a list of other All Employee report dataframes.
  df$date <- fname_date

  return(df)
}

# x -----------------------------------------------------------------------
#' all_ee_col_types
#'
#' A named list of vectors of column names specifying their type as numeric or
#' character. Additional date formating is necessary after loading. This column
#' specification ensures that leading zeros are never dropped from certain
#' fields such as GID or Zip Code.
#'
#' @param date the date on which the All Employees report was run from Banner.
#' This is necessary because the columns were expanded on 12/15/2017
#'
#' @return a named list of vectors assigning each column to a class by column
#' name.
all_ee_col_types <- function(date) {
  # Current column names in all ee as of 2017/12/27:

  # [1] "GID"                    "Last Name"
  # [3] "First Name"             "Home Street 1"
  # [5] "Home Street 2"          "Home Street 3"
  # [7] "City"                   "State"
  # [9] "Zip"                    "Campus"
  # [11] "Pict Code"              "Department"
  # [13] "Home Orgn Number"       "Budget Org."
  # [15] "Budget Org. Long Desc." "Org. Heirarchy"
  # [17] "Job Title"              "Status"
  # [19] "PEAEMPL ECLS"           "ECLS Description"
  # [21] "MUS"                    "Position Number"
  # [23] "Suffix"                 "Position Title"
  # [25] "FTE"                    "Job Type"
  # [27] "Pays"                   "Current Hire Date"
  # [29] "Campus Orig. Hire"      "Longevity Date"
  # [31] "Annual Lv Accrual"      "Anniversary Date"
  # [33] "Last Work Date"         "Job Begin Date"
  # [35] "Employee Group"         "Hourly Rate"
  # [37] "Annual Salary"          "Assgn Salary"
  # [39] "Retirement"             "Union"
  # [41] "Union Deduction"        "BCAT"
  # [43] "Leave Category"         "Sex"
  # [45] "Race 1"                 "Birth Date"
  # [47] "SOC Code"               "SOC Description"
  # [49] "Email"                  "Phone"
  # [51] "Index"                  "Fund"
  # [53] "Orgn"                   "Account"
  # [55] "Program"                "Percent"
  # [57] "date"                   "CUPA Code"
  # [59] "CUPA Desc."             "FED SOC Code"
  # [61] "FED SOC Code Desc."     "MUS SOC Code"
  # [63] "MUS SOC Code Desc."     "JCAT Code"
  # [65] "JCAT Desc."

  # column names in all ee as prior to 2017/12/15:

  # [1] "GID"                    "Last Name"
  # [3] "First Name"             "Home Street 1"
  # [5] "Home Street 2"          "Home Street 3"
  # [7] "City"                   "State"
  # [9] "Zip"                    "Campus"
  # [11] "Pict Code"              "Department"
  # [13] "Home Orgn Number"       "Budget Org."
  # [15] "Budget Org. Long Desc." "Org. Heirarchy"
  # [17] "Job Title"              "Status"
  # [19] "PEAEMPL ECLS"           "ECLS Description"
  # [21] "MUS"                    "Position Number"
  # [23] "Suffix"                 "Position Title"
  # [25] "FTE"                    "Job Type"
  # [27] "Pays"                   "Current Hire Date"
  # [29] "Campus Orig. Hire"      "Longevity Date"
  # [31] "Annual Lv Accrual"      "Anniversary Date"
  # [33] "Last Work Date"         "Job Begin Date"
  # [35] "Employee Group"         "Hourly Rate"
  # [37] "Annual Salary"          "Assgn Salary"
  # [39] "Retirement"             "Union"
  # [41] "Union Deduction"        "BCAT"
  # [43] "Leave Category"         "Sex"
  # [45] "Race 1"                 "Birth Date"
  # [47] "SOC Code"               "SOC Description"
  # [49] "Email"                  "Phone"
  # [51] "Index"                  "Fund"
  # [53] "Orgn"                   "Account"
  # [55] "Program"                "Percent"
  # [57] "date"

  all_ee_v2_date <- as.POSIXct("2017/12/15")
  if (date < all_ee_v2_date) { # This is the older version
    col_types <- list(character = c("GID",
                                    "Last Name",
                                    "First Name",
                                    "Home Street 1",
                                    "Home Street 2",
                                    "Home Street 3",
                                    "City",
                                    "State",
                                    "Zip",
                                    "Campus",
                                    "Pict Code",
                                    "Department",
                                    "Home Orgn Number",
                                    "Budget Org.",
                                    "Budget Org. Long Desc.",
                                    "Org. Heirarchy",
                                    "Job Title",
                                    "Status",
                                    "PEAEMPL ECLS",
                                    "ECLS Description",
                                    "MUS",
                                    "Position Number",
                                    "Suffix",
                                    "Position Title",
                                    "Job Type",
                                    "Current Hire Date",
                                    "Campus Orig. Hire",
                                    "Longevity Date",
                                    "Annual Lv Accrual",
                                    "Anniversary Date",
                                    "Last Work Date",
                                    "Job Begin Date",
                                    "Employee Group",
                                    "Retirement",
                                    "Union",
                                    "Union Deduction",
                                    "BCAT",
                                    "Leave Category",
                                    "Sex",
                                    "Race 1",
                                    "Birth Date",
                                    "SOC Code",
                                    "SOC Description",
                                    "Email",
                                    "Phone",
                                    "Index",
                                    "Fund",
                                    "Orgn",
                                    "Account",
                                    "Program"),
                      numeric = c("FTE",
                                  "Pays",
                                  "Hourly Rate",
                                  "Annual Salary",
                                  "Assgn Salary",
                                  "Percent"))
  } else {
    # This is the newer all ee version with removed SOC Code,
    # SOC Description columns and added CUPA, JCAT, SOC FED, and SOC
    # MUS columns specified
    col_types <- list(character = c("GID",
                                    "Last Name",
                                    "First Name",
                                    "Home Street 1",
                                    "Home Street 2",
                                    "Home Street 3",
                                    "City",
                                    "State",
                                    "Zip",
                                    "Campus",
                                    "Pict Code",
                                    "Department",
                                    "Home Orgn Number",
                                    "Budget Org.",
                                    "Budget Org. Long Desc.",
                                    "Org. Heirarchy",
                                    "Job Title",
                                    "Status",
                                    "PEAEMPL ECLS",
                                    "ECLS Description",
                                    "MUS",
                                    "Position Number",
                                    "Suffix",
                                    "Position Title",
                                    "Job Type",
                                    "Current Hire Date",
                                    "Campus Orig. Hire",
                                    "Longevity Date",
                                    "Annual Lv Accrual",
                                    "Anniversary Date",
                                    "Last Work Date",
                                    "Job Begin Date",
                                    "Employee Group",
                                    "Retirement",
                                    "Union",
                                    "Union Deduction",
                                    "BCAT",
                                    "Leave Category",
                                    "Sex",
                                    "Race 1",
                                    "Birth Date",
                                    "Email",
                                    "Phone",
                                    "Index",
                                    "Fund",
                                    "Orgn",
                                    "Account",
                                    "Program",
                                    "CUPA Code",
                                    "CUPA Desc.",
                                    "FED SOC Code",
                                    "FED SOC Code Desc.",
                                    "MUS SOC Code",
                                    "MUS SOC Code Desc.",
                                    "JCAT Code",
                                    "JCAT Desc."),
                      numeric = c("FTE",
                                  "Pays",
                                  "Hourly Rate",
                                  "Annual Salary",
                                  "Assgn Salary",
                                  "Percent"))
  }

  return(col_types)
}

#' format_allEE_dates
#'
#' Properly format dates using the ISO YYYY-MM-DD standard. All Employees report
#' formats them as character type in the DD-MMM-YYYY format.
#'
#' @param df dataframe containing the all employees data
#'
#' @return the input dataframe with revised date formats
#'
#' @examples
format_allEE_dates <- function(df) {

  date_cols <- c("Current Hire Date",
                 "Campus Orig. Hire",
                 "Longevity Date",
                 "Anniversary Date",
                 "Last Work Date",
                 "Job Begin Date",
                 "Anniversary Date",
                 "Birth Date")

  date_cols_indx <- which(names(df) %in% date_cols)

  for (col in date_cols_indx) {
    df[,col] <- lubridate::parse_date_time2(df[,col], "%d-%b-%y")

    # the year is stored as a two digit number making it difficult to parse
    # properly. '80' may be interpreted as 1980 or 2080. if the date was
    # parsed as the future, subtract 100 from it.
    misread_years <- which(lubridate::year(df[,col]) > lubridate::year(Sys.Date()))
    if (length(misread_years) > 0) {
      lubridate::year(df[misread_years, col]) <- lubridate::year(df[misread_years, col]) - 100
      misread_years <- NULL
    }
  }

  return(df)
}



#' supplement_all_ee
#'
#' A wrapper for several functions that add additional columns to the all
#' employees report. Adds EMR Job Type, EMR Org, Longevity Bonus, Full Name,
#' Job Key, Job Date Key, and fiscal year
#'
#' @param df the all employees report with unaltered column header names.
#'
#' @return the original input dataframe with the additional columns
#' @seealso add_emr_job_type, add_emr_orgs, add_longevity_bonus
#' @export
supplement_all_ee <- function(df) {
  df_out <- add_emr_job_type(df,
                             position_number_col_name = "Position Number",
                             suffix_col_name = "Suffix",
                             mus_col_name = "MUS")
  df_out <- add_emr_orgs(df_out,
                         dept_number_col_name = "Budget Org.")
  df_out <- add_longevity_bonus(df_out,
                                longevity_date_col = "Longevity Date",
                                hr_rate_col = "Hourly Rate",
                                assgn_rate_col = "Assgn Salary",
                                annual_rate_col = "Annual Salary")

  df_out$FullName <- paste0(df_out$`Last Name`,
                        ", ",
                        df_out$`First Name`)

  df_out$job_key <- paste0(df_out$GID,
                           df_out$`Position Number`,
                           df_out$Suffix)
  df_out$job_date_key <- paste0(df_out$GID,
                                df_out$`Position Number`,
                                df_out$Suffix,
                                df_out$date)

  df_out$FY <- compute_fiscal_year(date = df_out$date)

  return(df_out)
}

#' get_ftvorgn_data
#'
#' ftvorgn data is used to calculate organization hierarchy via the logical
#' structure of the organization codes. This validation table is maintained by
#' the finance team and regularly updated with new organization codes and org.
#' titles. For this reason, it is recommended to use this function to pull the
#' most up-to-date ftvorgn table data directly from Banner. Requires Banner
#' logon credentials.
#'
#' @return a dataframe containing all FTVORGN table variables. out of date rows
#' are removed leaving only the most recent record if more than one record
#' exists for a single ftvorgn organization code
#' @export

get_ftvorgn_data <- function() {

  banner_con  <- get_banner_conn()
  sql_qry <- "select * from FTVORGN"
  ftvorgn_data <- ROracle::dbGetQuery(banner_con,
                                      statement = sql_qry)
  ROracle::dbDisconnect(banner_con)

  # format the effective date column to an r date type
  # This will be used to only select the most recent organization code
  # assignment.
  ftvorgn_data$FTVORGN_EFF_DATE <- format_banner_date(ftvorgn_data$FTVORGN_EFF_DATE)
  ftvorgn_data$FTVORGN_TERM_DATE <- format_banner_date(ftvorgn_data$FTVORGN_TERM_DATE)

  # get the max assignment date for each organization code. These will be used
  # to filter out the old depreciated rows.
  orgn_max_dates <- dplyr::group_by(ftvorgn_data,
                                    FTVORGN_ORGN_CODE)
  orgn_max_dates <- dplyr::summarize(orgn_max_dates,
                                     max_date = max(FTVORGN_EFF_DATE))

  orgn_max_dates_keys <- paste0(orgn_max_dates$FTVORGN_ORGN_CODE,
                                orgn_max_dates$max_date)

  ftvorgn_data$key <- paste0(ftvorgn_data$FTVORGN_ORGN_CODE,
                             ftvorgn_data$FTVORGN_EFF_DATE)

  # do the filtering for the final dataset
  ftvorgn_data_out <- dplyr::filter(ftvorgn_data, key %in% orgn_max_dates_keys,
                                    FTVORGN_STATUS_IND == "A",
                                    is.na(ftvorgn_data$FTVORGN_TERM_DATE) |
                                      ftvorgn_data$FTVORGN_TERM_DATE > Sys.Date())
  return(ftvorgn_data_out)
}

#' @title Organization hierarchy based on job funding sources.
#'
#' Determine organization(s) providing the highest percent of a job's funding.
#' Using this as the primary 'Department', crosswalk appropriate 'College' and
#' 'Division' organizations
#'
#' @seealso \code{get_alt_org_hierarchy}, \code{opa_org_xwalk}
#'
#' @param df the dataframe to which the budget org hierarchy will be appended
#' @param lbr_dist_org_col_name the name of the column containing the
#' organization code. defaults to value used in All Employees report.
#' @param lbr_dist_prcnt_col_name the name of the column containing the
#' percent funding contributed by the organization code. defaults to value used
#' in All Employees report.
#'
#' @return a dataframe with a job-key and funding_dept, funding_college,
#' funding_division
#' @export
#'
#' @examples
get_budget_org_hierarchy <- function(df,
                                     lbr_dist_org_col_name,
                                     lbr_dist_prcnt_col_name) {
  library(dplyr)

  # the default values align with the column names found in the default all
  # employees report. These variables are used in dplyr filter and select
  # methods, hence the addition of the quo variables.
  if (missing(lbr_dist_org_col_name)) {
    lbr_dist_org_col_name <- "Orgn"
  }
  if (missing(lbr_dist_prcnt_col_name)) {
    lbr_dist_prcnt_col_name <- "Percent"
    quo_lbr_dist_prcnt_col_name <- dplyr::quo(Percent)
  }
  # ensure that the df has the necessary columns to determine the org number
  # by whichever has the greatest budget funding percentage.
  if (!lbr_dist_org_col_name %in% names(df) |
      !lbr_dist_prcnt_col_name %in% names(df)) {
    stop("Error in get_correct_budget_orgs\nCould not find ",
         lbr_dist_org_col_name,
         " or ",
         lbr_dist_prcnt_col_name)
  }

  # there may need to be additional work done to appropriatley quosure the
  # column names if those are user specified. The previous if missing statements
  # handle the default cases appropriately.
  #
  #quo_lbr_dist_org_col_name <- quo(lbr_dist_org_col_name)
  #quo_lbr_dist_prcnt_col_name <- enquo(lbr_dist_prcnt_col_name)

  # get the maximum percentage from any single labor distribtion source for each
  # job.
  # this max percent is joined to the core dataframe. Only rows which have the
  # respective 'max' percent for each job will be kept. This will leave multiple
  # rows per job in case of a tie max percent (i.e. 4 budget orgs each paying
  # 25% or two orgs each paying 50% etc)
  df$job_org_key <- paste0(df$job_key, df[,lbr_dist_org_col_name])

  df_max_percents <- dplyr::group_by(df, job_key)
  df_max_percents <- dplyr::summarise(df_max_percents,
                                      max_perc = max(!!quo_lbr_dist_prcnt_col_name))

  df_out <- dplyr::left_join(df,
                             df_max_percents,
                             by = "job_key")

  df_out$is_max_perc_row <- df_out[,lbr_dist_prcnt_col_name] == df_out$max_perc
  df_out <-  df_out[df_out$is_max_perc_row == TRUE,]

  #get the orgn names to connect to the max budget orgn codes
  orgn_data <- get_ftvorgn_data()
  orgn_data <- dplyr::select(orgn_data,
                             "orgn_code" = FTVORGN_ORGN_CODE,
                             "budget_orgn_desc" = FTVORGN_TITLE)

  org_hierarchy <- get_alt_org_hierarchy()
  df_out <- dplyr::left_join(df_out,
                             orgn_data,
                             by = c("Orgn" = "orgn_code"))
  df_out <- dplyr::left_join(df_out,
                             org_hierarchy,
                             by = c("Orgn" = "orgn_code"))



  # only keep the job key and new org info
  df_out <- dplyr::select(df_out,
                          job_key,
                          "funding_percent" = Percent,
                          "funding_org_code" = Orgn,
                          "funding_department" = budget_orgn_desc,
                          "funding_college" = College,
                          "funding_division" = Division)
  df_out <- dplyr::distinct(df_out)

  # for each unique job in the dataframe (df),
  #   figure out which orgn provides the most funcding based on the value
  #   stored in the column specified by lbr_dist_prcnt_col_name

  return(df_out)
}


#' get_alt_org_hierarchy
#'
#' loads the opa alternative organization hierarchy specifying the department,
#' college, and division rollups for each organization code. Update referenced
#' excel document as necessary.
#'
#' @return the dataframe specified by \code{opa_org_xwalk}
get_alt_org_hierarchy <- function() {
  fpath <- "X:/Employees/All EEs Reports/r_package/AllEmployees/"
  fname <- "AltOrgHierarchy.xlsx"

  name_path <- paste0(fpath, fname)

  alt_org_hierarchy <- readxl::read_excel(name_path)
  alt_org_hierarchy <- dplyr::select(alt_org_hierarchy,
                                     "orgn_code" = Orgn,
                                     "Department" = `Orgn Title`,
                                     "College" = `Level3 Orgn Title`,
                                     "Division" = `Level2 Orgn Title`)

  return(alt_org_hierarchy)
}


#' get_race_data
#'
#' Get Bob's IPED's Access race table. The path is optional incase the location
#' changes. The database is currently located at
#' "X:/IRCommon/RACE/IPEDS_Race.accdb". This table uses PIDM as it's unique key.
#' If it is necessary to filer or join by GID, the \code{get_pidm_gid_lu}
#' function is used to join GID to the dataset.
#'

#' @param unique_pidms a character or numeric vector used to filter the returned
#' dataset.
#' @param unique_gids a character or numeric vector used to filter the returned
#' dataset. Requires a Banner database connection.
#' @param optional_race_file_path A filepath to the access database containing
#' Bob's Race table
#' @param optional_return_all_cols A boolean parameter specifying whether to
#' return all columns or just the calculated IPEDS value.
#'
#' @return race data for all or a subset population
#' @export
#'
get_race_data <- function(unique_pidms,
                          unique_gids,
                          optional_race_file_path,
                          optional_return_all_cols = F) {
  if (missing(optional_race_file_path)) {
    race_file_path <- "X:/IRCommon/RACE/IPEDS_Race.accdb"
  } else {
    race_file_path <- optional_race_file_path
  }
  start_time <- Sys.time()
  race_conn <- get_access_conn(race_file_path)
  race_tbl <- dplyr::tbl(race_conn, "IPEDS")
  if (!optional_return_all_cols) {
    race_tbl <- dplyr::select(race_tbl, PIDM, IPEDS_Code)
  }
  race_tbl <- dplyr::collect(race_tbl)
  if (!missing(unique_pidms)) {
    race_tbl <- dplyr::filter(race_tbl, PIDM %in% unique_pidms)
  } else if (!missing(unique_gids)) {
    # if gid is all that is available, crosswalk them into the dataset from
    # the gid values. warning this can take quite a bit of time to complete
    # if the db is under heavy load.
    pidm_gid_lu <- get_pidm_gid_lu()
    race_tbl <- dplyr::left_join(race_tbl,
                                 pidm_gid_lu,
                                 by = "PIDM")
    race_tbl <- dplyr::filter(race_tbl,
                              GID %in% unique_gids)
  } else {
    # just return all the records, don't filter by either gid or pidm
  }
  end_time <- Sys.time()

  print(paste0("Race data collected in ", end_time - start_time))

  odbc::dbDisconnect(race_conn)

  return(race_tbl)
}

#' get_pidm_gid_lu
#'
#' get a dataset containing gids and their corresponding pidms. This is pulled
#' directly from banner. If a banner connection is not possible, see OPA's
#' employee snapshot files. This dataset is comprehensive for every student and
#' employee that has ever worked on campus while banner has been implemented
#'
#' @return a two column dataframe containing gids and corresponding pidms
#' @export
#'
get_pidm_gid_lu <- function() {
  pidm_id_qry <- "SELECT spriden_pidm \"PIDM\",spriden_id \"ID\" FROM spriden"
  bnr_conn <- get_banner_conn()
  df <- ROracle::dbGetQuery(conn = bnr_conn,
                            statement = pidm_id_qry)
  df <- rename_column(df,
                      old_name = "ID",
                      new_name = "GID")
  return(df)
}
