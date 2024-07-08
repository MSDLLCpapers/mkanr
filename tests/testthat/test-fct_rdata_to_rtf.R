# path to dummy data
data(adsl)
data(adtte)

# dummy directory for outputs
tmp_dir <- tempdir()

# test data
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

# extract gof
  gof_dtest<-extract_gof(fits=append(flexsurv_fits,spline_fits),
                         preprocessed_data = tte) 
  test_output <- format_gof(gof_dtest,
                                  outdata_path = tmp_dir,
                                  rename_output="test0outputtortf")
  test_output2 <- format_gof(gof_dtest,
                            outdata_path = tmp_dir,
                            rename_output="test0outputtortf2")
  
test_that("rdata_to_rtf data and parameter checks", {
  expect_error(rdata_to_rtf(
    rename_output = "test0outputtortf2",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 6)),
    col_format = NA,
    col_digits = NA,
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = FALSE
  ))
  
  expect_error(rdata_to_rtf(
    rename_output = "test0outputtortf2",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 6)),
    col_format = c(NA, rep("f", 3),rep("d", 3)),
    col_digits = c(NA, rep("1", 3), rep("0", 3)),
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = FALSE
  ))
  
  expect_error(rdata_to_rtf(
    rename_output = "test0outputtortf2",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 6)),
    col_format = c(NA, rep("f", 3),rep("d", 3)),
    col_digits = c(NA, rep(1, 3), rep(0, 3)),
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = FALSE
  ))

  expect_error(rdata_to_rtf(
    rename_output = "test0outputtortf2",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 5)),
    col_format = c(NA, rep("f", 3),rep("d", 3)),
    col_digits = c(NA, rep(1, 3), rep(0, 3)),
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = FALSE
  ))
  
  
})

test_that("rdata_to_rtf csv not produced when csv_output=FALSE", {
  # use function
  path_to_rtf2 <- rdata_to_rtf(
    rename_output = "test0outputtortf2",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 6)),
    col_format = c(NA, rep("f", 3),rep("d", 3)),
    col_digits = c(NA, rep(1, 3), rep(0, 3)),
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = FALSE
  )

  # Expect file saved at location
  expect_snapshot_file(path_to_rtf2)
  
  # Expect csv data identical to outdata data
  expect_warning(expect_error(read.csv(file.path(tmp_dir,"test0outputtortf2.csv"))))
  
})


test_that("rdata_to_rtf csv produced when csv_output=TRUE", {
  # use function
  path_to_rtf <- rdata_to_rtf(
    rename_output = "test0outputtortf",
    rtf_title = c("Title 1", "(Population)"),
    rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
    header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
    rel_col_widths = c(4, rep(1, 6)),
    col_just = c("l", rep("r", 6)),
    col_format = c(NA, rep("f", 3),rep("d", 3)),
    col_digits = c(NA, rep(1, 3), rep(0, 3)),
    orientation = "portrait",
    outdata_path = tmp_dir,
    outtable_path = tmp_dir,
    csv_output = TRUE
  )
  
  # Expect file saved at location
  expect_snapshot_file(path_to_rtf)
  
  # Expect csv data identical to outdata data
  expect_snapshot_file(file.path(tmp_dir,"test0outputtortf.csv"))
  
})

