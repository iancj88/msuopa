#' write_report
#'
#' Export a dataframe to an excel and csv file. Typically used to share
#' aggregated raw data.
#'
#' @param df the dataframe to be output
#' @param output_name_path the full name of the folder and file name to be
#' exported
#' @param sheetName The string name of the excel sheet
#' @param header_title The string to be placed in the header title when printing
#' the sheet
#'
#' @return NULL
#' @export
write_report <- function(df_list,
                         output_name_path,
                         sheetName,
                         opt_header_title) {

  if (missing(output_name_path)) {
    rstudioapi::selectDirectory(caption = "Select Folder for output",
                                label = "Select",
                                path = "./")
  }
  if (missing(opt_header_title)) {
    header_title <- sheetName
  } else {
    header_title <- opt_header_title
  }

  # this writes the csv file ....
  data.table::fwrite(x = df_list,
                     file = paste0(output_name_path, ".csv"),
                     append = FALSE,
                     sep = ",",
                     col.names = TRUE)

  # write.xlsx(x = df,
  #            file = paste0(output_name_path, ".xlsx"),
  #            creator = "Montana State University - Bozeman",
  #            zoom = 90,
  #            startRow = 2,
  #            colNames = TRUE,
  #            rowNames = FALSE,
  #            firstActiveRow = 3,
  #            firstActiveCol = 3)

  # setup appropriate classes for column formating
  #class(df$`Date Entered Job Title`) <- "Date"
  #class(df$`Hire Date`) <- "Date/POSIXt"


  wb_active <- openxlsx::createWorkbook()
  #options("openxlsx.borderColour" = "#4F80BD")
  #options("openxlsx.borderStyle" = "thin")
  #options("openxlsx.dateFormat" = "mm/dd/yyyy")

  options("openxlsx.datetimeFormat" = "mm/dd/yyyy")
  openxlsx::modifyBaseFont(wb_active,
                           fontSize = 10,
                           fontName = "Segoe UI")

  # add names to the dataframes in the list if there are none already assigned
  # give a warning to the user that the names were automatically added. these
  # names are used to determine sheetnames and table names
  if (is.null(names(df_list))) {
    n_items <- length(df_list)
    names(df_list) <- letters[1:n_items]
  }
  names(df_list) <- format_sht_names(names(df_list))

  # loop through the dataframes and write them into the file
  if (length(df_list) > 1) {
    invisible(mapply(df_list, create_sheets, )
  }

  openxlsx::saveWorkbook(wb_active,
                         file = paste0(output_name_path, ".xlsx"),
                         overwrite = TRUE)
}

#' format_sht_names ensures that the name to be used for an execl table or
#' worksheet is properly formatted
#'
#' Requries the length to be less than or equal to 31 (sheet max) and replaces
#' all spaces with underscores (table requirement)
#'
#' @param name_vec a string to be used as a worksheet name and/or table name
#'
#' @return the properly formatted string. if no formatting is needed then return
#' the input parameter string.
format_sht_names <- function(name_vec) {
  # the names will be used for the datatables within the workbook. as such, they
  # cannot contain spaces
  modified_names <- gsub("\\s", "_", name_vec)

  #they can also not be greater than 31 characters due to excel limitations
  #if any are the case, throw a warning messsage and truncate that result

  name_lengths <- nchar(modified_names)
  too_long_names_indx <- name_lengths > 31
  if (sum(too_long_names_indx) > 0) {
    warning("Worksheet Name(s) too long. ",
            "Values will be truncated to 31 characters. ",
            "Bad name(s):  \\n", modified_names[too_long_names_indx])
  }

  modified_names[too_long_names_indx] <- substr(modified_names[too_long_names_indx],
                                              start = 1,
                                              stop = 31)
  return(modified_names)
}

#' check_fix_dupe_name checks a string vector for the existence of a particular
#' string. If found, it modifies the query string so that it does not match an
#' existing string int he vector. It does this be appending an underscore and
#' numeric value.
#'
#' @param name the single string name that will be searched for and modified if
#' necessary
#' @param curr_names the vector of strings that will be searched for the single
#' string 'name'.
#'
#' @return if necessary, a modified string that is not duplicated in the input
#' vector. otherwise, the input name parameter
#' @export
check_fix_dupe_name <- function(name, curr_names) {
  # check to make sure that the dataframe name isn't already being used as the
  # name of a worksheet. If it is, append a '_x' where x is a number to name.
  # Recheck the length and shorten if necessary.
  while (name %in% curr_names) {
    number_suffix <- 1
    while (paste0(name, "_", number_suffix) %in% curr_names) {
      number_suffix <- number_suffix + 1
    }
    name <- paste0(name, "_", number_suffix)
    if (nchar(name) > 31) {
      suffix_len <- nchar(number_suffix) + 1
      name <- substr(name, start = 1, stop = 31 - suffix_len)
      name <- paste0(name, "_", number_suffix)
    }
  }

  return(name)
}

create_sheets <- function(wb_active, df, df_name, opt_header_row) {
  if (missing(opt_header_row)) {
    header_row <- 2
  } else {
    header_row <- opt_header_row
  }

  curr_sht_names <- openxlsx::names(wb_active)
  curr_tbl_names <- mapply(openxlsx::getTables,
                           sht = curr_sht_names,
                           MoreArgs = list(wb = wb_active),
                           SIMPLIFIY = TRUE)
  format_sht_names(df_name)
  df_name <- check_fix_dupe_name(df_name, curr_sht_names)
  df_name <- check_fix_dupe_name(df_name, curr_tbl_names)


  openxlsx::addWorksheet(wb_active,
                         sheetName = df_name,
                         zoom = 85,
                         header = c("Montana State University - Bozeman - ",
                                    paste0("Compiled on ", Sys.Date())),
                         orientation = "landscape")

  openxlsx::freezePane(wb_active,
                       sheet = df_name,
                       firstActiveRow = header_row + 1,
                       firstActiveCol = 2)

  openxlsx::writeDataTable(wb_active,
                           sheet = df_name,
                           x = df,
                           tableName = df_name,
                           colNames = TRUE,
                           rowNames = FALSE,
                           tableStyle = "TableStyleLight1",
                           startRow = header_row,
                           withFilter = TRUE,
                           bandedRows = TRUE)

  # format the header
  header_style <- openxlsx::createStyle(textDecoration = "bold",
                                        wrapText = TRUE,
                                        border = "bottom")
  openxlsx::setRowHeights(wb_active,
                          sheet = data_sht,
                          rows = header_row,
                          heights = 12.75 * 3)
  openxlsx::setColWidths(wb_active,
                         sheet = data_sht,
                         cols = 1:ncol(df),
                         widths = 16)
  openxlsx::addStyle(wb_active,
                     sheet = data_sht,
                     style = header_style,
                     rows = header_row,
                     cols = 1:ncol(df))
  openxlsx::setColWidths(wb_active,
                         sheet = data_sht,
                         cols = 1:ncol(df),
                         widths = "auto")
}