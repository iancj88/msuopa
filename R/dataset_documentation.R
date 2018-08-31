#' The OPA Employee snapshot sql query in a form to be executed on the Banner
#' database.
#'
#' For readable formatting, replace /n with actual new lines characters and
#' then remove all instances of escape characters ('/'). Requires
#' 'AS_OF_DATE_HERE' to be replaced with a valid character representation of
#' a date in the form YYYY-MM-DD. Also requires JOB_STATUSES_HERE string to be
#' replaced with a character vector comprised of atleast one value job status
#' abbreviations i.e. 'A', 'B', 'L'.
#'
#' @source OPA Employee Access database
#"snapshot_sql_query"

#' The EMR Organization Lookup table contains the crosswalk between organization
#' number, name, and higher-level organization known as 'EMROrg' and 'VPOrg.
#'
#' Requires regular updates as dept numbers/names change. Numbers and names are
#' defined/governed by the Finance team.
#'
#' @format A dataframe with 2487 rows and 4 variables:
#' \describe{
#'   \item{Dept Name}{The Name of the dept as defined in FTVORGN table}
#'   \item{Dept Number}{The six digit alpha-numeric dept. number as defined in
#'   the FTVORGN table}
#'   \item{EMROrg}{The EMR Org which the dept rolls up to. Originally created by
#'   Glen Nethercut, 2014. Owned by OPA and maintained by Ian C Johnson}
#'   \item{VPOrg}{The VP Org which is a slightly more aggregated rollup compared
#'   to the EMROrg}
#' }
"emr_org_xwalk"

#' The Longevity rate lookup file contains a sequence of integers from -1 to 100
#' corresponding to the years of service and associated longevity percent bonus.
#'
#' Each year of service has a Percent column which specifies the amount of bonus given to the employee. The bonus
#' ranges from 0 (no bonus) to .15 (15% bonus). Has not changed since at least 2005.
#'
#' @format A dataframe with 102 rows and 2 variables:
#' \describe{
#'   \item{YearsOfService}{An integer between -1 and 100}
#'   \item{PercentToBase}{A double between 0 and .15}
#' }
"longevity_rates"

#' The OPA Organization lookup table contains crosswalks from organization codes
#' to Department, College, and Division names. College and Division are roughly
#' equivalent to the EMROrg and VPOrg found in the emr_org_xwalk dataset.
#' Updated as needed (addition of new actively used organization codes). See
#' \code{get_alt_org_hierarchy} internal function in the msuopa package.
#'
#' @format A dataframe with 485 rows and 4 variables:
#' \describe{
#'   \item{orgn_code}{The six digit alpha-numeric organization code defined in
#'   the FTVORGN validation table}
#'   \item{Department}{The text description/name associated with the given
#'   \code{orgn_code} in the FTVORGN banner table}
#'   \item{College}{The college or unit that is roughly equivalent to the level
#'   3 organization level}
#'   \item{Division}{The highest level rollup hierarchy approximately equivalent
#'   to the level 2 organizations as defined by the business office}}
#'
"opa_org_xwalk"

#' FLSA Overtime exemption status is applied via nbajobs eclass setting. This
#' setting enables or disables the ability to enter extra overtime hours into
#' the web time entry timesheet. The dataset maps the ~40 eclasses to their FLSA
#' OT exemption status. 'FLSA OT Exempt' is one of three values: T, F, or NA. NA
#' is typically applied to corner cases such as the 'MG' or Additional
#' Compensation Eclass. This dataset is principally used in the
#' \code{add_flsa_exmpt_status}. The individual exempt non-exempt designations
#' should be reviewed on an annual basis with the HR team to ensure that the
#' Banner implementation has not changed.
#'
#' @format A dataframe with 43 rows and 2 variables:
#' \describe{
#'   \item{Ecls Code}{The two digit abbreviated eclass code. Only the eclass
#'   applied to NBAJOBS record affects FLSA status}
#'   \item{FLSA OT Exempt}{A boolean value indicating whether the eclass allows
#'   overtime hours on webtime entry. Acceptable values include T, F, and NA.}}
#'
"ecls_flsa_exmpt_tbl"


#' The termination reason codes, description and boolean indicator if the
#' termination was voluntary by the employee
#'
#' @format A dataframe with 16 rows and three variables: \describe{
#'   \item{TermReas}{The two digit termination code applied to an individual.
#'   NOT a job term code reason}
#'   \item{Desc}{A textual description of the term code}
#'   \item{IsVoluntary}{Boolean value indicating if the reason is employee
#'   voluntary}}
#'
"term_code_lu"


#' Number of working hours per month and per year for all fiscal years from
#' 2000-2033 and for the primary employee types: Classified, Professional,
#' Faculty (9 month), and Faculty (10 month)
#' @source Max Thompson's email dated 2/22/17
#' @format A dataframe with 34 rows and 15 variables: \describe{
#'   \item{begin_date}{The first day of the fiscal year}
#'   \item{end_date}{The last day of the fiscal year}
#'   \item{FY}{the fiscal year}
#'   \item{days_cls, days_prof, days_fac9, days_fac10}{The number of working
#'   days for the type of employee in the given fiscal year}
#'   \item{hours_cls, hours_prof, hours_fac9, hours_fac10}{The working hours per
#'   pay period for the type of employee}
#'   \item{hours_per_month_cls, hours_per_month_prof, hours_per_month_fac9,
#'   hours_per_month_fac10}{The number of hours in each pay period for the
#'   type of employee}}
#'
"FY_days_hours"