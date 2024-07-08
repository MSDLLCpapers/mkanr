#temporary directory
tmp_dir <- tempdir()
tmp_dir_all = file.path(tmp_dir, "all")
if (dir.exists(tmp_dir_all))
  unlink(tmp_dir_all, recursive = TRUE)
dir.create(tmp_dir_all, recursive = TRUE)

# path to dummy data
data(adsl)
data(adtte)

#dummy directory for outputs
tmp_dir = tempdir()


#load dummy data
tte <- preprocess_data(
  population_from = adsl,
  population_where = ,
  observation_from = adtte,
  observation_where = AVAL>7,
  analysis_variable = list(name="AVAL2", derivation="AVAL/7-1", label='Analysis Variable 2'),
  analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
  therapy_data_var=list(name="TRT01P",levels=NULL,labels=NULL,name_label="Planned Treatment for Period 01")
)

tte_km <- preprocess_data(
  population_from = adsl,
  population_where = ,
  observation_from = adtte,
  observation_where =,
  analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
  analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
  therapy_data_var=list(name="TRT01P",levels=NULL,labels=NULL,name_label="Planned Treatment for Period 01")
)

test_formula <- stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P)

#fits list

#flexsurvspline
spline_fits<-run_models_flexsurvspline(
  preprocessed_data = tte,
  surv_formula = test_formula,
  scalexk_list = list(hazard_1k=list(scale="hazard",k=1))
)

spline_fits2<-run_models_flexsurvspline(
  preprocessed_data = tte,
  surv_formula = test_formula,
  scalexk_list = list(
    hazard_1k = list(scale = "hazard", k = 1),
    hazard_2k = list(scale = "hazard", k = 2),
    hazard_3k = list(scale = "hazard", k = 3))
)

#flexsurvreg
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


test_that("format_gof - formats goodness of fit as expected", {
  
  #extract gof
  gof_dtest<-extract_gof(fits=append(flexsurv_fits,spline_fits),
                         preprocessed_data = tte) 
  gof_dtest_formatted<-format_gof(gof_dtest)
  
  #check number of combine models is correct
  expect_equal(dim(gof_dtest_formatted)[1],
               length(append(flexsurv_fits,spline_fits)))
  
  #check output format is consistent with documentation
  expect_true(is.data.frame(gof_dtest_formatted))
  expect_equal(colnames(gof_dtest_formatted),
               c("Survival Model","AIC","BIC","Mean AIC BIC","Rank AIC", "Rank BIC", "Rank Mean AIC BIC"))
  
  
  #check gof_arrange arranges data as expected
  expect_equal(gof_dtest_formatted %>% dplyr::arrange(`Rank AIC`),
               format_gof(gof_dtest,
                          gof_arrange = "Rank AIC"))
  
  #double programming of derived variables
  expect_equal(gof_dtest_formatted$`Mean AIC BIC`,
               sapply(gof_dtest,function(x) (x$AIC+x$BIC)/2),
               ignore_attr=TRUE)
  
  expect_equal(gof_dtest_formatted %>% dplyr::arrange(`Rank AIC`) %>% dplyr::pull(AIC),
               sort(sapply(gof_dtest,function(x) x$AIC)),
               ignore_attr=TRUE)
  
  expect_equal(gof_dtest_formatted %>% dplyr::arrange(`Rank BIC`) %>% dplyr::pull(BIC),
               sort(sapply(gof_dtest,function(x) x$BIC)),
               ignore_attr=TRUE)
  
  expect_equal(gof_dtest_formatted %>% dplyr::arrange(`Rank Mean AIC BIC`) %>% dplyr::pull(`Mean AIC BIC`),
               sort(sapply(gof_dtest,function(x) (x$AIC+x$BIC)/2)),
               ignore_attr=TRUE)
  
  #check identical output (RData format) is saved in outdata_path with rename_output name
  gof_dtest_formatted_save<-format_gof(gof_dtest,
                                       outdata_path = tmp_dir,
                                       rename_output = "gof0dtest0save")
  
  expect_silent(load(file.path(tmp_dir,"gof0dtest0save.RData")))
  
  expect_equal(gof_dtest_formatted_save,gof0dtest0save)
  
})


test_that("format_est - formats survival estimates as expected - case 1: default parameters with treatment as covariate", {
  
  #extract estimates from fit
  est_dtest<-extract_est(fits=flexsurv_fits)
  est_dtest_formatted<-format_est(est_dtest)
  
  #run survfit model with default parameters
  survfit_model<-survival::survfit(attributes(est_dtest)$surv_formula,
                                   attributes(est_dtest)$preprocessed_data,
                                   type="fleming")
  
  survfit_est<-summary(survfit_model,
                       time=0:ceiling(max(attributes(est_dtest)$preprocessed_data[as.character(attributes(est_dtest)$surv_formula[[2]])[2]])))
  
  #check number of rows is as expected (all models combines plus suvfit estimates)
  expect_equal(dim(dplyr::bind_rows(do.call(dplyr::bind_rows,est_dtest),tibble::tibble(time=survfit_est$time,
                                                                                       est=survfit_est$surv)))[1],
               dim(est_dtest_formatted)[1])
  
  #check output format is consistent with documentation
  expect_true(is.data.frame(est_dtest_formatted))
  expect_equal(colnames(est_dtest_formatted),
               c("x_primary","y_primary","param_dist","group_primary",
                 as.character(attributes(flexsurv_fits)$surv_formula[[3]])))
  
  #double program est 
  est_vector=c(survfit_est$surv,sapply(est_dtest,function(x) x$est))
  expect_equal(est_vector,est_dtest_formatted$y_primary)
  
  #check identical output (RData format) is saved in outdata_path with rename_output name
  est_dtest_formatted_save<-format_est(est_dtest,
                                       outdata_path = tmp_dir,
                                       rename_output = "est0dtest0save")
  
  expect_silent(load(file.path(tmp_dir,"est0dtest0save.RData")))
  
  expect_equal(est_dtest_formatted_save,est0dtest0save)
  
})


test_that("format_est - formats survival estimates as expected - case 2: default parameters with no covariate", {
  
  #define formula
  nocov_formula <- stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1)
  
  #run fits
  spline_fits<-run_models_flexsurvspline(
    preprocessed_data = tte,
    surv_formula = nocov_formula,
    scalexk_list = list(hazard_1k=list(scale="hazard",k=1))
  )
  
  #extract estimates from fit
  est_dtest<-extract_est(fits=spline_fits)
  est_dtest_formatted<-format_est(est_dtest)
  
  #run survfit model with default parameters
  survfit_model<-survival::survfit(attributes(est_dtest)$surv_formula,
                                   attributes(est_dtest)$preprocessed_data,
                                   type="fleming")
  
  survfit_est<-summary(survfit_model,
                       time=0:ceiling(max(attributes(est_dtest)$preprocessed_data[as.character(attributes(est_dtest)$surv_formula[[2]])[2]])))
  
  #check number of rows is as expected (all models combines plus suvfit estimates)
  expect_equal(dim(dplyr::bind_rows(do.call(dplyr::bind_rows,est_dtest),tibble::tibble(time=survfit_est$time,
                                                                                       est=survfit_est$surv)))[1],
               dim(est_dtest_formatted)[1])
  
  #check output format is consistent with documentation
  expect_true(is.data.frame(est_dtest_formatted))
  expect_equal(colnames(est_dtest_formatted),
               c("x_primary","y_primary","param_dist","group_primary"))
  
  #double program est 
  est_vector=c(survfit_est$surv,sapply(est_dtest,function(x) x$est))
  expect_equal(est_vector,est_dtest_formatted$y_primary)
  
  #check identical output (RData format) is saved in outdata_path with rename_output name
  est_dtest_formatted_save<-format_est(est_dtest,
                                       outdata_path = tmp_dir,
                                       rename_output = "est0dtest0save2")
  
  expect_silent(load(file.path(tmp_dir,"est0dtest0save2.RData")))
  
  expect_equal(est_dtest_formatted_save,est0dtest0save2)
  
})
