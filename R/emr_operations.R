#' add_emr_job_type
#'
#' EMR Job Types are aggregated groups of employee classes. These are commonly
#' used as grouping variables for analyses. Job type derived from Position #,
#' Suffix, and MUS Contract indicator.
#'
#' @param df the dataframe containing the necessary columns and to which the new
#' column will be appended.
#' @param position_number_col_name the string containing the name of the
#' Position Number column
#' @param suffix_col_name the string containing the name of the
#' Suffix column
#' @param mus_col_name the string containing the name of the
#' MUS Contract Indicator column
#'
#' @return the original dataframe with a newly appended \code{EMRJobType} column
#' @export

add_emr_job_type <- function(df,
                             position_number_col_name = "Position Number",
                             suffix_col_name = "Suffix",
                             mus_col_name = "MUS") {

  one_char_posn <- stringr::str_sub(unlist(df[,position_number_col_name]), 1, 1)
  two_char_posn <- stringr::str_sub(unlist(df[,position_number_col_name]), 1, 2)
  third_char_posn <- stringr::str_sub(unlist(df[,position_number_col_name]), 3, 3)

  # Apply the Classified job label
  is_classified <- (two_char_posn %in% c("4M","4N"))
  df[is_classified,"EMRJobType"] <- "Classified"

  #Apply the Fixed-Term job label
  is_fixed_term <- (two_char_posn %in% c("4M","4N") & third_char_posn %in% c("2", "3"))
  df[is_fixed_term,"EMRJobType"] <- "Fixed-Term"

  #Apply the executive job label
  is_executive <- (two_char_posn == "4E")
  df[is_executive,"EMRJobType"] <- "Executive"

  #Apply the faculty labels
  is_faculty_tt <- (two_char_posn %in% c("4A", "4B", "4X") & df[,mus_col_name] == "Y")
  is_faculty_ntt <- (two_char_posn %in% c("4A", "4B", "4X") & df[,mus_col_name] == "N")
  df[is_faculty_tt,"EMRJobType"] <- "Faculty TT/T"
  df[is_faculty_ntt,"EMRJobType"] <- "Faculty NTT"

  # Apply the Student and Grad Student labels
  is_student <- (two_char_posn == "4S")
  is_grad <- (two_char_posn == "4D")
  df[is_student, "EMRJobType"] <- "Student"
  df[is_grad, "EMRJobType"] <- "Grad Asst."

  # Apply the Temp job label
  is_temp <- two_char_posn %in% c("4T", "4K", "4J", "4L", "4P")
  df[is_temp, "EMRJobType"] <- "Temporary"

  # Apply the Retiree job label
  is_retiree <- (two_char_posn == "4R")
  df[is_retiree, "EMRJobType"] <- "Retiree"

  # Apply the Professional job label
  is_professional <- (two_char_posn %in% c("4C", "4H"))
  df[is_professional, "EMRJobType"] <- "Professional"

  # Apply the ad hoc hourly label
  is_adhoc <- (two_char_posn %in% c("4F", "4V"))
  df[is_adhoc, "EMRJobType"] <- "Ad-Hoc Hourly"

  # Apply the Summer session label
  is_summer_session <- (two_char_posn == "4X")
  df[is_summer_session, "EMRJobType"] <- "Summer Session"

  # Apply the non-Job payment label
  non_job_suff <- c("SD", "GP", "CR", "OT", "OL", "TF", "TM",
                    "LW", "TL", "TR", "RF", "OC", "L3", "GS", "SE")
  non_job_position_numbers <-  c("4ADCMP",
                                 "4ONEPY",
                                 "4TERMS",
                                 "4OEHHD")
  is_non_job_payment <- (df[,suffix_col_name] %in% non_job_suff
                         | df[,position_number_col_name] %in% non_job_position_numbers)

  df[is_non_job_payment,"EMRJobType"] <- "Non-Job Payment"
  rm(is_non_job_payment)

  is_non_bz_job <- (!one_char_posn == "4")
  df[is_non_bz_job, "EMRJobType"] <- "Non-BZ Job"

  # Apply the additional comp payment label
  #is_add_comp <- (df$Suffix %in% )
  #
  # TODO:
  #   differentiate non-job-payment for additional compensation payment

  return(df)
}



#' add_emr_orgs
#'
#' Function to add a column of data to a dataframe corresponding to
#   the EMR Organization of the data
#'
#'
#' @param df the dataframe to which the data will be added
#' @param dept_number_col_name the string containing the name of the dept number
#' column
#' @param opt_lu_fpath supply if an alternative lookup dataset is needed.
#' Defaults to use the internally stored \code{emr_org_xwalk} dataframe.
#'
#' @return the original dataframe with an additioanl emr_org column
#' @export
add_emr_orgs <- function(df,
                         dept_number_col_name,
                         opt_lu_fpath) {


  if (!missing(opt_lu_fpath)) {
    stopifnot(file.exists(opt_lu_fpath))
    emr_xwalk <- readxl::read_xlsx(path = opt_lu_fpath,
                                   skip = 1)
    emr_xwalk <- dplyr::select(emr_xwalk, `Dept Number`, EMROrg, VPOrg)
  } else {
    emr_xwalk <- dplyr::select(msuopa::emr_org_xwalk,
                               -`Dept Name`)
  }

  df <- rename_column(df,
                     old_name = dept_number_col_name,
                     new_name = "Dept Number")

  df <- dplyr::left_join(df,
                         emr_xwalk,
                         by = c("Dept Number"))

  df <- rename_column(df,
                     new_name = dept_number_col_name,
                     old_name = "Dept Number")
  return(df)
}
