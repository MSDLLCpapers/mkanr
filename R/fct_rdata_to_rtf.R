#' Create RTF table from .RData file using r2rtf package
#'
#' Creates an RTF table with title and footer from an RData output using r2rtf package.
#'
#' @section Specification:
#' \if{latex}{ This function creates an RTF plot with title and footer from an RData output using r2rtf package.
#'
#'  Logical Specifications:
#'
#'   \enumerate{
#'   \item Check input parameters
#'   \item If csv_output equals TRUE, use write.csv to generate un-formatted csv file in outdata folder
#'   \item Assemble the title, footnote and .RData output into an RTF table output in outtable folder
#'   }
#'   
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param rename_output Name for output files
#' @param rtf_title Table title(s) for rtf output (Default c("Title 1","(Population)"))
#' @param rtf_footnote Table footnote(s) for rtf output (Default c("{^a} Footnote 1","{^b} Footnote 2"))
#' @param rel_col_widths Relative columns widths (Default c(2.5,rep(1,3)))
#' @param col_just Columns justification (Default c("l", rep("c", 3)))
#' @param col_format Number formatting as per format parameter of formatC function, use NA for string or no formatting (Default rep(NA,4))
#' @param col_digits The desired number of digits after the decimal point as per digits parameter of formatC function, use NA for no formatting (Default rep(NA,4))
#' @param header_label Columns custom label (Default "")
#' @param csv_output Boolean (TRUE/FALSE) to indicate if csv output should be produced (Default TRUE)
#' @param orientation Orientation in 'portrait' or 'landscape' (Default "portrait")
#' @param outdata_path Directory for the .RData table (Default path$outdata)
#' @param outtable_path Directory for the .rtf table (Default path$outtable)
#'
#' @return A text containing the path to the created RTF table.
#'
#' @import dplyr
#' @import r2rtf
#' @import stringr
#' @importFrom tibble as_tibble
#' @importFrom rlang is_missing
#' @importFrom  purrr pmap
#'
rdata_to_rtf <- function(rename_output,
                          rtf_title = c("Title 1", "(Population)"),
                          rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
                          rel_col_widths = c(2.5, rep(1, 3)),
                          col_just = c("l", rep("c", 3)),
                          col_format = rep(NA, 4),
                          col_digits = rep(NA, 4),
                          header_label = "",
                          csv_output = TRUE,
                          orientation = "portrait",
                          outdata_path = path$outdata,
                          outtable_path = path$outtable) {
  # Bulletproofing ----
  bullet_proof(rename_output = rename_output,
               rtf_title = rtf_title,
               rtf_footnote = rtf_footnote,
               rel_col_widths = rel_col_widths,
               col_just = col_just,
               header_label = header_label,
               csv_output = csv_output,
               orientation = orientation,
               outdata_path = outdata_path,
               outtable_path = outtable_path)
               
  
  # input table
  filename <-
    file.path(outdata_path, paste0(rename_output, ".RData"))
  if (!file.exists(filename))
    stop(
      filename,
      " (outdata_path/rename_output.RData) file does not exist, please enter valid information"
    )

  # utility function to loadRData
  load_RData <- function(fileName) {
    # loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  # 	Load table in environment ----
  table_01 <- load_RData(filename)
  
  # check table
  if (!is.data.frame(table_01))
    stop(filename,
         " (outdata_path/rename_output.RData) file should be a dataframe")
  # check col_just
  if (!(dim(table_01)[2] == length(col_just)))
    stop(
      "col_just should be the same length as number of columns in table stored in outdata_path/rename_output.RData"
    )
  # check rel_col_widths
  if (!(dim(table_01)[2] == length(rel_col_widths)))
    stop(
      "rel_col_widths should be the same length as number of columns in table stored in outdata_path/rename_output.RData"
    )
  # check header_label
  if (header_label != "" &
      !(dim(table_01)[2] == length(
        stringr::str_split(header_label, stringr::fixed("|"), simplify = TRUE)
      )))
    stop(
      header_label,
      " (header_label) should be the same number of string (separated by |) as number of columns in table stored in outdata_path/rename_output.RData"
    )
  # check col_format
  if (rlang::is_missing(col_format) |
      rlang::is_missing(col_digits))
    stop("col_format and col_digits should not be missing")
  if (!(is.character(ifelse(is.na(col_format), " ", col_format)) &
        is.numeric(ifelse(is.na(col_digits), 999, col_digits))))
    stop(
      "non NA col_format should be of type character and non NA col_digits should be of type numeric"
    )
  if (!(dim(table_01)[2] == length(col_format) &
        dim(table_01)[2] == length(col_digits)))
    stop(
      "col_format and col_digits should be the same length as number of columns in table stored in outdata_path/rename_output.RData"
    )
  if (!identical(unlist(purrr::pmap(list(x = col_format, y = col_digits),
                                    function(x, y) {
                                      ifelse((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y)),
                                             TRUE,
                                             FALSE)
                                    })), rep(TRUE, length(col_format)))) {
    stop("NA should be present at the same indices in both col_format and col_digits")
  }
  
  # Format metadata ----
  table_02 <- table_01 %>%
    as.data.frame() %>%
    tibble::as_tibble()
  
  table_03 <- do.call(dplyr::bind_cols, list(purrr::pmap(list(
    x = 1:length(table_02),
    y = col_digits,
    z = col_format
  ),
  function(x, y, z) {
    ifelse(!is.na(y),
           tibble::as_tibble(formatC(
             x = table_02 %>% dplyr::pull(x),
             digits = y,
             format = z
           )),
           table_02[, x]) %>% setNames(colnames(table_02[, x]))
  })))
  
  tbl_base <- table_03 %>%
    dplyr::mutate(dplyr::across(everything(),
                                ~ stringr::str_replace_all(.x, intToUtf8(160), " ")))
  
  names(tbl_base) <-
    stringr::str_replace_all(names(tbl_base), intToUtf8(160), " ")
  
  colheader1 <-
    ifelse(header_label == "", paste(names(tbl_base), collapse = "|"), header_label)
  
  #Save csv version
  if (isTRUE(csv_output)) {
    csv_out <- table_02
    names(csv_out) <-
      unlist(strsplit(colheader1, split = "|", fixed = TRUE))
    write.csv(csv_out, file = file.path(outdata_path, paste0(rename_output, ".csv")))
  }
  
  # Generate RTF table and save to outtable ----
  tbl_base %>%
    r2rtf::rtf_title(rtf_title) %>%
    r2rtf::rtf_footnote(rtf_footnote) %>%
    r2rtf::rtf_colheader(colheader1,
                         col_rel_width = rel_col_widths) %>%
    r2rtf::rtf_body(
      col_rel_width = rel_col_widths,
      text_justification = col_just,
      text_indent_first = -240,
      text_indent_left = 180
    ) %>%
    r2rtf::rtf_page(orientation = orientation) %>%
    r2rtf::rtf_encode() %>%
    r2rtf::write_rtf(file.path(outtable_path, paste0(rename_output, ".rtf")))
  
  # Return path to table ----
  return(file.path(outtable_path, paste0(rename_output, ".rtf")))
}
