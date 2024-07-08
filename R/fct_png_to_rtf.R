#' Create RTF plot from .png file
#'
#' Creates an RTF plot with title and footer from a .png output.
#'
#' @section Specification:
#' \if{latex}{ This function creates an RTF plot with title and footer from a .png output.
#'
#' Logical Specifications:
#'
#'   \enumerate{
#'   \item Check input parameters.
#'   \item Assemble the title, footnote and png format plot into an RTF output.
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param outgraph_path Directory for the .png output (Default path$outgraph)
#' @param rename_output Name of output files
#' @param rtf_title Figure title(s) for rtf output (Default c("Title 1","(Population)"))
#' @param rtf_footnote Figure footnote(s) for rtf output (Default c("{^a} Footnote 1","{^b} Footnote 2"))
#' @param fig_height Height (inch) of the figure in rtf output (default 4)
#' @param fig_width Width (inch) of the figure in rtf output (default 6)
#' @param orientation Orientation in 'portrait' or 'landscape' (Default "portrait")
#'
#' @return A text containing the path to the created RTF plot.
#' 
#' @import r2rtf
#' @importFrom rlang is_missing
#'
png_to_rtf <- function(outgraph_path,
                       rename_output,
                       rtf_title = c("Title 1", "(Population)"),
                       rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
                       fig_height = 4,
                       fig_width = 6,
                       orientation = "portrait") {
  # Bulletproofing ----
  bullet_proof(outgraph_path = outgraph_path,
                rename_output = rename_output,
                rtf_title = rtf_title,
                rtf_footnote = rtf_footnote,
                fig_height = fig_height,
                fig_width = fig_width,
                orientation = orientation)
               
  # filename
  filename <- file.path(outgraph_path, paste0(rename_output, ".png"))
  if (!file.exists(filename)) stop(filename, " (outgraph_path/rename_output.png) file does not exist, please enter valid information")

  # Generate and format RTF figure and save to outgraph_path ----
  filename %>%
    r2rtf::rtf_read_figure() %>% # read PNG files from the file path
    r2rtf::rtf_page(orientation = orientation) %>% # set page orientation
    r2rtf::rtf_title(rtf_title) %>% # add title or subtitle
    r2rtf::rtf_footnote(rtf_footnote, text_justification = "l") %>% # add footnote
    r2rtf::rtf_figure(
      fig_height = fig_height, # set figure height
      fig_width = fig_width
      # set figure width individually.
    ) %>%
    r2rtf::rtf_encode(doc_type = "figure") %>% # encode rtf as figure
    r2rtf::write_rtf(file.path(outgraph_path, paste0(rename_output, ".rtf")))

  # Return path to table ----
  return(file.path(outgraph_path, paste0(rename_output, ".rtf")))
}
