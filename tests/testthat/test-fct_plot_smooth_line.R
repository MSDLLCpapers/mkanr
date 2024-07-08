# path to dummy data
data(adsl)
data(adtte)

# dummy directory for outputs
tmp_dir <- tempdir()
tmp_dir_all = file.path(tmp_dir, "all")
if (dir.exists(tmp_dir_all))
  unlink(tmp_dir_all, recursive = TRUE)
dir.create(tmp_dir_all, recursive = TRUE)


test_that("plot_smooth_line produces expected plot and data", {
  
  # use function
  plot_dtest01 <- plot_smooth_line(
    data = Orange,
    xvar = "age",
    yvar = "circumference",
    levelvar = "Tree",
    rename_output = "dtest01",
    xlab = "Age",
    ylab = "Circumference",
    fig_res = 300,
    fig_height = 4,
    fig_width = 6.6,
    fig_scale = 1.3,
    legend_position = "bottom",
    levelvar_legend = "Tree",
    legend_textsize = 10,
    outgraph_path = tmp_dir,
    outdata_path = tmp_dir
  )
  
  # Expect png saved in outgraph_path
  expect_snapshot_file(file.path(tmp_dir, "dtest01.png"))
  
  # Expect identical plot saved and returned by function
  load(file.path(tmp_dir, "dtest01.RData"))
  
  # expect data used for plot and Rdata saved identical
  expect_equal(Orange, dtest01$data)
  
  
  # double programming
  plot_dblprg <- Orange %>%
    ggplot2::ggplot(ggplot2::aes(x = age, y = circumference, col = Tree, linetype = Tree)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Age") +
    ggplot2::ylab("Circumference") +
    ggplot2::scale_colour_manual(
      name = "Tree",
      values = c("black", grDevices::rainbow(3),
                 guide = guide_legend(ncol=2, byrow=TRUE))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Tree",
      values = c(1, rep_len(2:10, 3))
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10)
    )
  
  # expect plot data to be the same
  expect_equal(plot_dblprg$data, plot_dtest01$data)
  
})

test_that("plot_smooth_line correctly resizes the ledend width to span across ful figure length", {
  
  # long-term survival curves: short labels
  hta_flexsurv(population_from = mkanr::adsl,
               population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
               observation_from = mkanr::adtte,
               observation_where = (PARAMCD == "OS"),
               therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
               time = "AVAL",
               time_unit = "W",
               parametric_list = list(`Exponential`="exp",
                                      `Weibull`="weibull",
                                      `Log-normal`="lognormal", 
                                      `Log-logistic`="llogis",
                                      `Gompertz`="gompertz", 
                                      `Gamma`="gamma", 
                                      `Generalized Gamma`="gengamma"),
               spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                  `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                  `Spline on normal - 1 knots`=list(scale="normal",k=1)),
               predict_time = 0:1040,
               id_analysis=c('e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
               rtf_prefix = "dtest10",
               rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                       rtf_title_pop=c("Subtitle 1", "(Population)"),
                                       rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                       param="Overall Survival",
                                       therapy_arm=levels(tte[[therapy_data_var$name]]),
                                       output_suffix_update = list(),
                                       rtf_param = list(),
                                       models_title="Standard Parametric and Flexible Parametric Spline",
                                       modified_value=list()),
               outdata_path = tempdir(),
               outgraph_path = tempdir(),
               outtable_path = tempdir()
  )
  
  expect_snapshot_file(file.path(tmp_dir,paste0("dtest10", "e0stdflex0fig0timelong",".rtf")))
  expect_snapshot_file(file.path(tmp_dir,paste0("dtest10", "e0stdflex0fig0timeshort",".rtf")))

  
  # long-term survival curves: long labels
  hta_flexsurv(population_from = mkanr::adsl,
               population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
               observation_from = mkanr::adtte,
               observation_where = (PARAMCD == "OS"),
               therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
               time = "AVAL",
               time_unit = "W",
               parametric_list = list(`Exponential`="exp",
                                      `Weibull`="weibull",
                                      `Log-normal`="lognormal", 
                                      `Log-logistic`="llogis",
                                      `Gompertz`="gompertz", 
                                      `Gamma`="gamma", 
                                      `Generalized Gamma`="gengamma"),
               spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                  `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                  `Spline on normal - 1 knots`=list(scale="normal",k=1)),
               predict_time = 0:1040,
               id_analysis=c('e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
               rtf_prefix = "dtest110",
               rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                       rtf_title_pop=c("Subtitle 1", "(Population)"),
                                       rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                       param="Overall Survival",
                                       therapy_arm=levels(tte[[therapy_data_var$name]]),
                                       output_suffix_update = list('e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                                       rtf_param = list('custom_model_label', 'custom_model_label'),
                                       models_title="Standard Parametric and Flexible Parametric Spline",
                                       modified_value=list(TRUE,TRUE)),
               outdata_path = tempdir(),
               outgraph_path = tempdir(),
               outtable_path = tempdir()
  )
  
  expect_snapshot_file(file.path(tmp_dir,paste0("dtest110", "e0stdflex0fig0timelong",".rtf")))
  expect_snapshot_file(file.path(tmp_dir,paste0("dtest110", "e0stdflex0fig0timeshort",".rtf")))

})


test_that("plot_smooth_line correctly plots the figure with not NULL values", {
  # use function
  plot_dtest01 <- plot_smooth_line(
    data = Orange,
    xvar = "age",
    yvar = "circumference",
    levelvar = "Tree",
    break_point = 1000,
    rename_output = "dtest03",
    xlab = "Age",
    ylab = "Circumference",
    fig_res = 300,
    fig_height = 4,
    fig_width = 6.6,
    fig_scale = 1.3,
    legend_position = "bottom",
    levelvar_legend = "Tree",
    legend_textsize = 10,
    outgraph_path = tmp_dir,
    outdata_path = tmp_dir
  )
  
  # Expect png saved in outgraph_path
  expect_snapshot_file(file.path(tmp_dir, "dtest03.png"))
})