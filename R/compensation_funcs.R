#' add_longevity_bonus
#'
#' Add a longevity raise amount column depending on the years of service and
#' emr job type. If the EMRJobType column is not in the dataset, the function will
#' attempt to add it.
#'
#'
#' @param df the dataframe to which the new columns will be appended
#' @param longevity_date_col The name of the column containing the longevity
#' date data for each person.
#' @param hr_rate_col The name of the column containing the unadjusted hourly
#' rate of the job
#' @param assgn_rate_col The name of the column containing the unadjusted
#' monthly rate of the job
#' @param annual_rate_col The name of the column containing the unadjusted
#' annual salary of the job
#' @param opt_as_of_date An optional date on which to caluclate longevity bonus
#' rates. Defaults to value found in the \code{date} column if unsupplied.
#'
#' @return The original dataframe with four new columns:
#' \describe{
#' \item{years_of_service}{The total years of service as-of the last day of the month of the as-of date}
#' \item{PercentToBase}{The percent bonus as a double i.e. no bonus corresponds with a PercentToBase == 1. Derived from the LongevityBonus Lookup table}
#' \item{BaseAndLongHourly}{The Hourly pay rate with longevity bonus}
#' \item{BaseAndLongAssgn}{The monthly pay rate with longevity bonus}
#' \item{BaseAndLongAnnual}{The annual salary with longevity bonus}
#' }
#' @export
add_longevity_bonus <- function(df,
                                longevity_date_col,
                                hr_rate_col,
                                assgn_rate_col,
                                annual_rate_col,
                                opt_as_of_date) {
  # Add a longevity raise amount column depending on the years of service
  longevity_date <- df[,longevity_date_col]

  # calculate the years of service from the 1st. of the month,
  # that's when the longevity increase goes into effect in payroll
  lubridate::day(longevity_date) <- 01

  # get an interval class object between the two measurement dates,
  # this interval is converted into the years unit by the duration (dyears)
  # function, i.e. what is the duration of the interval in years?
  if (missing(opt_as_of_date)) {
    opt_as_of_date <- df$date
  } else {
    opt_as_of_date <- rep(opt_as_of_date,
                          length.out = length(longevity_date))
  }
  yos_interval <- lubridate::interval(longevity_date,
                                      opt_as_of_date)

  years_of_service <- yos_interval / lubridate::dyears(x = 1)

  # use the whole integer for the lookup value in the longevity multiplier
  # percent table
  years_of_service <- floor(years_of_service)
  df$`years_of_service` <- years_of_service

  # longevity rates df stored internally to package. see above RDS.

  df <- dplyr::left_join(x = df,
                         y = msuopa::longevity_rates,
                         by = c("years_of_service" = "YearsOfService"))
  df$PercentToBase <- df$PercentToBase + 1 # PercentToBase comes from longevity_rate
  #   join.

  # only classifieds get the bonus so default in 100% for rest of rows
  if (!"EMRJobType" %in% names(df)) {
    warning(paste0("Adding Longevity rates",
                   "EMRJobType column does not exist. ",
                   "Attempting to add with default column names"))
    df <- add_emr_job_type(df,
                           position_number_col_name = ,
                           suffix_col_name = ,
                           mus_col_name = )
  }
  df[which(!df$EMRJobType == "Classified"), "PercentToBase"] <- 1

  # Now add the meat, potates and gravy
  df$BaseAndLongHourly <- df$PercentToBase * df[,hr_rate_col]
  df$BaseAndLongAssgn <- df$PercentToBase * df[,assgn_rate_col]
  df$BaseAndLongAnnual <- df$PercentToBase * df[,annual_rate_col]

  return(df)
}


#' Add a job's FLSA exemption status to a dataframe
#'
#' Using a choosen Ecls column, map elcs to FLSA overtime exemption status.
#'
#' @param df the dataframe containing the ecls column and to which the new
#' column will be added.
#' @param ecls_col_name The string vector containing the name of the eclass
#' column to which the FLSA status will be mapped.
#'
#' @return The original dataframe with a new column named \code{FLSA OT Exempt}.
#' @export
#'
add_flsa_exmpt_status <- function(df, ecls_col_name) {
  stopifnot(ecls_col_name %in% names(df))
  df_out <- df
  df_out$ThisIsAnAbsurdColumnNamePlaceHolder123459 <- df_out[,ecls_col_name]

  df_out <- dplyr::left_join(df_out,
                             msuopa::ecls_flsa_exmpt_tbl,
                             by = c("ThisIsAnAbsurdColumnNamePlaceHolder123459" = "Ecls Code"))
  df_out <- dplyr::select(df_out,
                          -ThisIsAnAbsurdColumnNamePlaceHolder123459)

  return(df_out)
}

