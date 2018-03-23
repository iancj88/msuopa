#' write_report
#'
#' Export a dataframe to an excel and csv file. Typically used to share
#' aggregated raw data.
#'
#' @param df the dataframe to be output
#' @param output_name_path the full name if
#' @param sheetName
#' @param header_title
#'
#' @return NULL
#' @export
#'
#' @examples
write_report <- function(df,
                         output_name_path,
                         sheetName,
                         header_title) {
  require(openxlsx)
  require(data.table)

  if (missing(output_name_path)) {
    rstudioapi::selectDirectory(caption = "Select Folder for output",
                                label = "Select",
                                path = "./")
  }
  # this writes the csv file obviously
  data.table::fwrite(x = df,
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

  header_row <- 2
  data_sht <- 1
  wb_active <- openxlsx::createWorkbook()

  #options("openxlsx.borderColour" = "#4F80BD")
  #options("openxlsx.borderStyle" = "thin")
  #options("openxlsx.dateFormat" = "mm/dd/yyyy")

  options("openxlsx.datetimeFormat" = "mm/dd/yyyy")
  openxlsx::modifyBaseFont(wb_active,
                           fontSize = 10,
                           fontName = "Segoe UI")
  openxlsx::addWorksheet(wb_active,
                         sheetName = sheetName,
                         zoom = 85,
                         header = c("Montana State University - Bozeman",
                                    header_title,
                                    paste0("Compiled on ", Sys.Date())),
                         orientation = "landscape")

  openxlsx::freezePane(wb_active,
                       sheet = data_sht,
                       firstActiveRow = header_row + 1,
                       firstActiveCol = 2)

  openxlsx::writeDataTable(wb_active,
                           sheet = data_sht,
                           x = df,
                           tableName = sheetName,
                           colNames = TRUE,
                           rowNames = FALSE,
                           tableStyle = "TableStyleLight1",
                           startRow = header_row,
                           withFilter = TRUE,
                           bandedRows = TRUE)

  # format the header
  openxlsx::header_style <- createStyle(textDecoration = "bold",
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
  openxlsx::saveWorkbook(wb_active,
                         file = paste0(output_name_path, ".xlsx"),
                         overwrite = TRUE)
}
