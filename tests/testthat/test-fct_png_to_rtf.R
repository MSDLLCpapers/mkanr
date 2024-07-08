#temporary directory
tmp_dir <- tempdir()

# path to dummy data
data(adsl)
data(adtte)

# preprocess tte
tte <- preprocess_data(
  population_from = adsl,
  population_where = ,
  observation_from = adtte,
  observation_where = AVAL>7,
  analysis_variable = list(name="AVAL2", derivation="AVAL/7-1", label='Analysis Variable 2'),
  analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
  therapy_data_var=list(name="TRT01P",levels=NULL,labels=NULL,name_label="Planned Treatment for Period 01")
)

test_formula <- stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P)

# fits list
## flexsurvspline
spline_fits<-run_models_flexsurvspline(
  preprocessed_data = tte,
  surv_formula = test_formula,
  scalexk_list = list(hazard_1k=list(scale="hazard",k=1))
)

## flexsurvreg
flexsurv_fits<-run_models_flexsurvreg(
  preprocessed_data = tte,
  surv_formula = test_formula,
  dist_list = list(`Exponential`="exp",
                   weibull="weibull", 
                   gompertz="gompertz", 
                   gamma="gamma", 
                   lognormal="lognormal", 
                   llogis="llogis", 
                   `Generalized Gamma`="gengamma")
)


# extract est
extrap_est<-extract_est(fits=append(flexsurv_fits,spline_fits),
                        t=0:70,
                        ci=FALSE,
                        se=FALSE,
                        B=1000,
                        cl = 0.95,
                        na.action = na.omit)

ready_to_plot<-format_est(est=extrap_est,
                          preprocessed_data=tte,
                          preprocessed_data_km=tte,
                          surv_formula=test_formula,
                          type="fleming",
                          rename_output="test0pngtortf",
                          outdata_path=tmp_dir)

test0pngtortf <- plot_smooth_line(
  data = ready_to_plot,
  xlab = "Time in Weeks", 
  ylab = "Survival",
  xvar = "x_primary",
  yvar = "y_primary",
  levelvar = "group_primary",
  break_point= NULL,
  rename_output = "test0pngtortf",
  outgraph_path = tmp_dir,
  outdata_path = tmp_dir,
  fig_scale_x_continuous="scale_x_continuous(expand=c(0,0))",
  fig_scale_y_continuous="scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2), limits=c(0,1))",
  fig_res = 300,
  fig_height = 4,
  fig_width = 6.6,
  fig_scale = 1.3,
  levelvar_legend = "Type",
  legend_textsize = 10,
  legend_position = "bottom"
)

test_that("png_to_rtf works", {
  path_fig <- png_to_rtf(
    outgraph_path = tmp_dir,
    rename_output = "test0pngtortf",
    rtf_title = "This is a test\n (ITT Population)",
    rtf_footnote = "Database Lock Date: test",
    fig_height = 4,
    fig_width = 6,
    orientation = "portrait"
  )
  
  testthat::expect_snapshot_file(path_fig)
})