# path to dummy data
data(adsl)
data(adtte)

# preprocess tte
preprocessed_data <- preprocess_data(
  population_from = adsl,
  population_where = (ITTFL == "Y"),
  observation_from = adtte,
  observation_where = (PARAMCD == "OS"),
  analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
  analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
)

#flexsurvreg ----
test_that("run_models_flexsurvreg - double programming - produces fit list as expected", {
  
  #case 1: one fit, treatment as a covariate
  wrapper_flexsurvreg<-run_models_flexsurvreg(
    preprocessed_data = preprocessed_data ,
    surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
    dist_list = list(weibull="weibull")
  )
  
  function_flexsurvreg<-flexsurv::flexsurvreg(data=preprocessed_data ,
                                              formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
                                              dist = "weibull")
  
  expect_equal(attributes(wrapper_flexsurvreg$weibull),attributes(function_flexsurvreg))
  test_dummy<-lapply(attributes(wrapper_flexsurvreg$weibull)$names,
                     function(x) if(x!="call"){expect_equal(wrapper_flexsurvreg$weibull[x],function_flexsurvreg[x])})
  
  #case 2: multiple fits, no covariates
  wrapper_flexsurvreg_2<-run_models_flexsurvreg(
    preprocessed_data = preprocessed_data ,
    surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
    dist_list = list(weibull="weibull",
                     exp="exp")
  )
  
  function_flexsurvreg_2<-list(weibull=flexsurv::flexsurvreg(data=preprocessed_data ,
                                                             formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                                             dist = "weibull"),
                               exp=flexsurv::flexsurvreg(data=preprocessed_data ,
                                                         formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                                         dist = "exp"))
  test_dummy<-purrr::pmap(list(a=wrapper_flexsurvreg_2,b=function_flexsurvreg_2),
                          function(a,b) {
                            expect_equal(attributes(a),attributes(b));
                            lapply(attributes(a)$names,
                                   function(x) if(x!="call"){expect_equal(a[x],b[x])})
                          }
  )
})

test_that("run_models_flexsurvreg - partial convergence issues", {
  
  #check only data of converged models is present in output
  surv_pred_func <- run_models_flexsurvreg(
    preprocessed_data = preprocessed_data %>% dplyr::filter(row_number()<18),
    surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
    dist_list = list(exponential="exp",
                     weibull="weibull", 
                     gompertz="gompertz", 
                     gamma="gamma", 
                     lognormal="lognormal", 
                     loglogistic="llogis", 
                     gengamma="gengamma")
  )
  
  expect_equal(length(surv_pred_func),6)
  
})

#flexsurvspline ----
test_that("run_models_flexsurvspline - double programming - produces fit list as expected", {
  
  #case 1: one fit, treatment as a covariate
  wrapper_flexsurvspline<-run_models_flexsurvspline(
    preprocessed_data = preprocessed_data ,
    surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
    scalexk_list = list(hazard_1k=list(scale="hazard",k=1))
  )
  
  function_flexsurvspline<-flexsurv::flexsurvspline(data=preprocessed_data ,
                                                    formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
                                                    scale = "hazard",
                                                    k=1)
  
  expect_equal(attributes(wrapper_flexsurvspline$hazard_1k),attributes(function_flexsurvspline))
  test_dummy<-lapply(attributes(wrapper_flexsurvspline$weibull)$names,
                     function(x) if(x!="call"){expect_equal(wrapper_flexsurvspline$weibull[x],function_flexsurvspline[x])})
  
  #case 2: multiple fits, no covariates
  wrapper_flexsurvspline_2<-run_models_flexsurvspline(
    preprocessed_data = preprocessed_data ,
    surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
    scalexk_list = list(hazard_1k=list(scale="hazard",k=1),
                        normal_knot_10=list(scale="normal",knots=log(c(10))))
  )
  
  function_flexsurvspline_2<-list(hazard_1k=flexsurv::flexsurvspline(data=preprocessed_data ,
                                                                     formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                                                     scale = "hazard",
                                                                     k=1,
                                                                     method="Nelder-Mead"),
                                  normal_knot_10=flexsurv::flexsurvspline(data=preprocessed_data ,
                                                                          formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                                                          scale = "normal",
                                                                          knots=log(c(10)),
                                                                          method="Nelder-Mead"))
  
  test_dummy<-purrr::pmap(list(a=wrapper_flexsurvspline_2,b=function_flexsurvspline_2),
                          function(a,b) {
                            expect_equal(attributes(a),attributes(b));
                            lapply(attributes(a)$names,
                                   function(x) if(x!="call"){expect_equal(a[x],b[x])})
                          }
  )
})


test_that("run_models_flexsurvpline - partial convergence issues", {
  #check error if only some model converges
  surv_spline_dtest <- run_models_flexsurvspline(preprocessed_data=preprocessed_data %>% dplyr::filter(TRT01P=="B: Placebo" & row_number()<30),
                                                 surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                                 scalexk_list = list(hazard_1k=list(scale="hazard",k=1),
                                                                     hazard_2k=list(scale="hazard",k=2),
                                                                     hazard_3k=list(scale="hazard",k=3),
                                                                     odds_1k=list(scale="odds",k=1),
                                                                     odds_2k=list(scale="odds",k=2),
                                                                     odds_3k=list(scale="odds",k=3),
                                                                     normal_1k=list(scale="normal",k=1),
                                                                     normal_2k=list(scale="normal",k=2),
                                                                     normal_3k=list(scale="normal",k=3)),
                                                 seed_value = 7902,
                                                 na.action=na.exclude,
                                                 method = "Nelder-Mead")
  
  expect_equal(length(surv_spline_dtest),8)
  
})
