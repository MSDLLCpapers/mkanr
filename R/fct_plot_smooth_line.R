#' Create line plot
#'
#' Creates a line plot and saves it to .RData and .png format.
#'
#' @section Specification:
#' \if{latex}{ This function creates a line plot and saves it to .RData and .png format.
#'
#' Logical Specifications:
#'  
#'   \enumerate{
#'   \item Check validity of input parameters
#'   \item Create the plot
#'   \item Save plot and associated data in outdata and .png in outgraph
#'   \item Return plot
#'   }
#'
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param data Data for the line plot
#' @param xvar Independent variable 
#' @param yvar Dependent variable 
#' @param xlab X-axis label 
#' @param ylab Y-axis label 
#' @param levelvar Category variable 
#' @param facetvar Strata variable (for ggplot2::facet_grid)
#' @param break_point X-axis value to indicate break point value for piece-wise analysis
#' @param fig_res Resolution (Pixels per inch) of the figure 
#' @param fig_height Height (inch) of the figure 
#' @param fig_width Width (inch) of the figure 
#' @param fig_scale Scale factor of the figure 
#' @param fig_line_size Line width in mm 
#' @param fig_grid_x Logical. Show x axis grid lines (vertical) 
#' @param fig_grid_y Logical. Show y axis grid lines (horizontal) 
#' @param fig_scale_x_continuous Customize scale of x-axis (see ggplot2::scale_x_continuous())
#' @param fig_scale_y_continuous Customize scale of y-axis (see ggplot2::scale_y_continuous())
#' @param palette Palette to use for plotting 
#' @param facetvar_legend Title of the legend for facetvar (see ggplot2::labeller())
#' @param levelvar_legend Title of the legend for levelvar
#' @param legend_textsize Text size of the legend 
#' @param legend_position Position of the legend 
#'        Possible values are ("none", "left", "right", "bottom", "top")
#' @param rename_output Name of output files
#' @param outdata_path Directory for the .RData output 
#' @param outgraph_path Directory for the .png output 
#'
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @importFrom rlang is_missing
#' @importFrom stringr str_detect
#'
plot_smooth_line <- function(data,
                             xvar="x_primary",
                             yvar="y_primary",
                             levelvar="group_primary",
                             facetvar=NULL,
                             break_point=NULL,
                             rename_output="test",
                             xlab = "X label",
                             ylab = "Y label",
                             fig_res = 300,
                             fig_height = 4,
                             fig_width = 6.6,
                             fig_scale = 1.3,
                             fig_line_size = 0.7,
                             fig_grid_x = TRUE,
                             fig_grid_y = TRUE,
                             fig_scale_x_continuous=NULL,
                             fig_scale_y_continuous=NULL,
                             palette=c("#00857C", "#6ECEB2", "#BFED33", "#FFF063", "#0C2340", "#5450E4", 
                                                "#688CE8", "#69B8F7", "#799A0E", "#BFED33", "#66203A", "#DB8DAA", 
                                                "#F68D2E", "#FABB82", "#00857C", "#6ECEB2", "#BFED33", "#FFF063", 
                                                "#0C2340", "#5450E4", "#688CE8", "#69B8F7", "#799A0E", "#BFED33"
                             ),
                             legend_position = "bottom",
                             facetvar_legend = ggplot2::labeller(.cols = base::identity),
                             levelvar_legend = "Type",
                             legend_textsize = 10,
                             outgraph_path = tempdir(),
                             outdata_path = tempdir()) {

  # Bulletproofing ----
  bullet_proof(data = data,
               xvar = xvar,
               yvar = yvar,
               levelvar = levelvar,
               facetvar = facetvar,
               break_point = break_point,
               rename_output = rename_output,
               xlab = xlab,
               ylab = ylab,
               fig_res = fig_res,
               fig_height = fig_height,
               fig_width = fig_width,
               fig_scale = fig_scale,
               fig_line_size = fig_line_size,
               fig_grid_x = fig_grid_x,
               fig_grid_y = fig_grid_y,
               palette = palette,
               legend_position = legend_position,
               levelvar_legend = levelvar_legend,
               legend_textsize = legend_textsize,
               outgraph_path = outgraph_path,
               outdata_path = outdata_path)
               
               
               
  # check facetvar in data
  if(!is.null(facetvar) && facetvar %in% colnames(data)){
    stop("facetvar should exist in input data")
  }
  # check var in data
  if (!identical(c(xvar, yvar, levelvar) %in% colnames(data), rep(TRUE, 3))) stop(xvar, " (xvar), ", yvar, " (yvar) and ", levelvar, " (levelvar) should exist in data")
  # check level is factor
  if (!(is.factor(data |> dplyr::pull(get(levelvar))))) stop(levelvar, " (levelvar) should be a factor in data")

  # Calculate number of columns dynamically ----
  max_char_level <- max(strwidth(levels(data |> dplyr::pull(get(levelvar))),font=legend_textsize, units='in'))
  n_col_legend <- floor(fig_width/max_char_level)
  
  # Generate plot  ----
  n_dists <- nlevels(data |> dplyr::pull(get(levelvar))) - 1

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = get(xvar), y = get(yvar), colour = get(levelvar)))
  
  if ("Kaplan-Meier" %in% unique(data |> dplyr::pull(get(levelvar)))) {
    plot <- plot +
      ggplot2::geom_step(data = data |> dplyr::filter(get(levelvar) == "Kaplan-Meier"), size = fig_line_size)
  }

  plot <- plot +
    ggplot2::geom_line(data = data |> dplyr::filter(get(levelvar) != "Kaplan-Meier"), size = fig_line_size) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::scale_colour_manual(
      name = levelvar_legend,
      values = c("black", palette),
      breaks = levels(data |> dplyr::pull(get(levelvar))),
      guide = guide_legend(ncol=n_col_legend, byrow=TRUE)
    )  
  
  if(!is.null(facetvar) && is.character(facetvar) && length(facetvar)==1 && facetvar %in% colnames(data)){
  plot <- plot +
    ggplot2::facet_grid(cols=ggplot2::vars(get(facetvar)),labeller=facetvar_legend)
  }
  
  if(!is.null(break_point) && is.numeric(break_point) && length(break_point)==1){
    plot <- plot + 
      ggplot2::geom_vline(xintercept=break_point,col="red")+
      ggplot2::annotate("text",x=break_point,y=-Inf,hjust=-0.1,vjust=1.2,angle=90,label=paste0("break point = ",break_point),color="red", size=2.5)
  }
  
  plot<- plot +
    ggplot2::theme(
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = legend_textsize),
      panel.spacing.x = grid::unit(fig_width/25, "inches"),
      legend.key.size = grid::unit(fig_width/50, "inches"),
      legend.key.width = grid::unit(fig_width/50, "inches"),
      legend.spacing.x = grid::unit(fig_width/25, "inches"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = -0.2, unit = "in")
    )
  
  if(!is.null(fig_scale_x_continuous)){
    plot <- plot + 
      eval(parse(text=(fig_scale_x_continuous)))
  }
  
  if(!is.null(fig_scale_y_continuous)){
    plot <- plot + 
      eval(parse(text=(fig_scale_y_continuous)))
  }

  # Store result data and graph ----
  ggplot2::ggsave(plot = plot, file = file.path(outgraph_path, paste0(rename_output, ".png")), dpi = fig_res, height = fig_height, width = fig_width, scale = fig_scale)

  rdata <- list(plot = plot, data = data)
  assign(rename_output, rdata)
  save(list = rename_output, file = file.path(outdata_path, paste0(rename_output, ".RData")))

  # Return plot ----
  plot
}
