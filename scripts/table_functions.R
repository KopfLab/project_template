#' Export data frame to excel
#'
#' Export one or multiple data frame to an Excel file. For more fine-control, call createWorkbook and add_excel_sheet directly.
#'
#' @param ... data frames to export, name arguments to get tabe names --> export_to_excel(`tab a` = df_a, `tab b` = df_b, ...)
#' @param file file path where to save the excel file
#' @param dbl_digits how many digits to show for numbers (NOTE: numbers are exported at full float precision, this is just for the excel styling of numbers) 
#' @return returns the data frame invisibly for use in pipes
export_to_excel <- function(..., file, dbl_digits = 2) {
  # make excel workbook
  wb <- openxlsx::createWorkbook()
  
  # add excel shet
  sheets <- list(...)
  stopifnot(length(sheets) > 0)
  if (is.null(names(sheets)) || any(nchar(names(sheets)) == 0)) {
    names(sheets) <- paste("Sheet", seq_len(length(sheets)))
  }
  purrr::walk2(
    names(sheets),
    sheets,
    ~add_excel_sheet(wb, sheet_name = .x, .y, dbl_digits = dbl_digits)
  )
  
  # save workbook
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  
  # return invisible
  return(invisible(df))
}


#' Add an excel sheet to a workbook
#' @param ... the data frames
#' @inheritParams export_to_excel
#' @param col_max_width maximum column width
add_excel_sheet <- function(wb, sheet_name, ..., dbl_digits = 2, col_max_width = 75) {
  
  # sheet
  openxlsx::addWorksheet(wb, sheet_name)
  hs <- openxlsx::createStyle(textDecoration = "bold") # header style
  
  # data
  sheet_data_sets <- list(...)
  start_row <- 1L
  for (sheet_data in sheet_data_sets) {
    sheet_data <- dplyr::ungroup(sheet_data)
    if (ncol(sheet_data) > 0) {
      openxlsx::writeData(wb, sheet_name, sheet_data, startRow = start_row, headerStyle = hs)
      int_cols <- which(purrr::map_lgl(sheet_data, is.integer))
      dbl_cols <- setdiff(which(purrr::map_lgl(sheet_data, is.numeric)), int_cols)
      if (dbl_digits < 1) {
        int_cols <- c(int_cols, dbl_cols)
        dbl_cols <- integer()
      }
      # integer column formatting
      if (length(int_cols) > 0) {
        openxlsx::addStyle(
          wb, sheet_name, style = openxlsx::createStyle(numFmt = "0"),
          rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
          cols = int_cols, gridExpand = TRUE)
      }
      # double column formatting
      if (length(dbl_cols) > 0) {
        dbl_format <- paste0("0.", paste(rep("0", dbl_digits), collapse = ""))
        openxlsx::addStyle(
          wb, sheet_name, style = openxlsx::createStyle(numFmt = dbl_format),
          rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
          cols = dbl_cols, gridExpand = TRUE)
      }
      # new start row
      start_row <- start_row + nrow(sheet_data) + 2L
    }
  }
  
  # calculate header widths
  header_widths <- 
    sheet_data_sets %>% 
    # account for bold width
    purrr::map(~nchar(names(.x)))
  max_n_cols <- purrr::map_int(header_widths, length) %>% max()
  
  # calculate data widths
  if (max_n_cols > 0) {
    calculate_data_width <- function(x) {
      if (is.integer(x)) x <- sprintf("%d", x)
      else if (is.numeric(x)) x <- sprintf(paste0("%.", dbl_digits, "f"), x)
      else x <- as.character(x)
      return(max(c(0, nchar(x)), na.rm = TRUE))
    }
    data_widths <-
      sheet_data_sets %>% 
      purrr::map(
        ~dplyr::summarise_all(.x, list(calculate_data_width)) %>% 
          unlist(use.names = FALSE)
      )
    max_widths <- purrr::map2(header_widths, data_widths , ~{
      widths <- if (is.null(.y)) .x else pmax(.x, .y, 0)
      widths <- pmin(col_max_width, widths)
      c(widths, rep(0L, times = max_n_cols - length(widths)))
    })
    col_widths <- do.call(pmax, args = max_widths)
    openxlsx::setColWidths(wb, sheet_name, cols = 1:length(col_widths), widths = col_widths)
  }
  
}