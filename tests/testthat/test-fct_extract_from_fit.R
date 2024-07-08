#temporary directory
tmp_dir <- tempdir()

# path to dummy data
data(adsl)
data(adtte)

#load dummy data
tte <- preprocess_data(
  population_from = adsl,
  population_where = ITTFL=='Y',
  observation_from = adtte,
  observation_where = PARAMCD=="PFS",
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

#flexsurvreg
flexsurv_fits<-run_models_flexsurvreg(
  preprocessed_data = tte,
  surv_formula = test_formula,
  dist_list = list(exp="exp",
                   weibull="weibull", 
                   gompertz="gompertz", 
                   gamma="gamma", 
                   lognormal="lognormal", 
                   llogis="llogis", 
                   gengamma="gengamma")
)


test_that("extract_gof - double programming - produces gof as expected", {
  
  #flexsurvreg
  flexreg_manual=flexsurv::flexsurvreg(formula= test_formula,data = tte, dist="gompertz")
  #res
  expect_equal(data.frame(distr_desc="gomp",
                          AIC=flexreg_manual$AIC,
                          BIC=glance(flexreg_manual)$BIC),
               extract_gof(fits=list(gomp=flexsurv_fits$gompertz),
                           preprocessed_data = tte)$gomp,
               ignore_attr = TRUE)
  
  #flexsurvspline
  #res
  set.seed=7902
  spline_manual<-flexsurv::flexsurvspline(formula= test_formula,data = tte, scale="hazard",k=1,na.action=na.omit,
                                          method = "Nelder-Mead")
  expect_equal(data.frame(distr_desc=add_info_fit_desc(fits=list(spline=spline_manual)),
                          AIC=spline_manual$AIC,
                          BIC=glance(spline_manual)$BIC),
               extract_gof(fits=list(spline=spline_fits$hazard_1k),
                           preprocessed_data = tte)$spline,
               ignore_attr = TRUE)
  

  #combination of 2
  expect_equal(list(spline=data.frame(distr_desc=add_info_fit_desc(fits=list(spline=spline_manual)),
                                      AIC=spline_manual$AIC,
                                      BIC=glance(spline_manual)$BIC),
                    flexreg=data.frame(distr_desc="gomp",
                                       AIC=flexreg_manual$AIC,
                                       BIC=glance(flexreg_manual)$BIC)),
               extract_gof(fits=list(spline=spline_fits$hazard_1k,
                                     gomp=flexsurv_fits$gompertz),
                           preprocessed_data = tte),
               ignore_attr = TRUE)
  
  #check attributes match for spline knots feature
  expect_equal(extract_gof(spline_fits)$hazard_1k$distrib_desc,
               add_info_fit_desc(spline_fits, exp=TRUE, digits=2),
               ignore_attr=TRUE)
  
  expect_equal(extract_gof(spline_fits,exp=FALSE,digits=3)$hazard_1k$distrib_desc,
               add_info_fit_desc(spline_fits, exp=FALSE, digits=3),
               ignore_attr=TRUE)
  
  expect_equal(extract_gof(spline_fits, custom_model_label = FALSE)$hazard_1k$distrib_desc,
               add_info_fit_desc(spline_fits, exp=TRUE, digits=2, custom_model_label = FALSE),
               ignore_attr=TRUE)
  
  
})


test_that("extract_est - double programming - produces survival times/hazard estimates as expected", {
  
  #flexsurvreg
  flexreg_manual=flexsurv::flexsurvreg(formula= test_formula,data = tte, dist="gompertz")
  flexreg_est=cbind(summary(flexreg_manual,type="survival",tidy=TRUE,ci=FALSE,t=0:10),
                    param_dist=rep("gomp",33),
                    dist_desc=rep("gomp",33),
                    est_hazard=summary(flexreg_manual,type="hazard",tidy=TRUE,ci=FALSE,t=0:10)$est)
  
  #res
  expect_equal(flexreg_est,
               extract_est(fits=list(gomp=flexsurv_fits$gompertz), t=0:10)$gomp,
               ignore_attr = TRUE)
  
  
  #flexsurvspline
  #res
  set.seed=7902
  spline_manual<-flexsurv::flexsurvspline(formula= test_formula,data = tte, scale="hazard",k=1,na.action=na.omit,
                                          method = "Nelder-Mead")
  spline_est=cbind(summary(spline_manual,type="survival",tidy=TRUE,ci=FALSE,t=0:10),
                   param_dist=rep("hazard_1k",33),
                   dist_desc=rep(add_info_fit_desc(fits=list(hazard_1k=spline_manual)),33),
                   est_hazard=summary(spline_manual,type="hazard",tidy=TRUE,ci=FALSE,t=0:10)$est)
  
  #res
  expect_equal(spline_est,
               extract_est(fits=list(hazard_1k=spline_fits$hazard_1k), t=0:10)$hazard_1k,
               ignore_attr = TRUE)
  
  
  #combination of 2
  expect_equal(list(hazard_1k=spline_est,
                    gomp=flexreg_est),
               extract_est(fits=list(hazard_1k=spline_fits$hazard_1k,
                                     gomp=flexsurv_fits$gompertz),
                           t=0:10),
               ignore_attr = TRUE)
  
  #run with ci and se
  expect_silent(test_lcl<-extract_est(fits=flexsurv_fits,
                                      t=c(0,100,200,1000),
                                      ci=TRUE,
                                      se=TRUE,
                                      B=1000,
                                      cl = 0.95,
                                      na.action = na.pass))
  set.seed(1234)
  flexreg_lcl=cbind(summary(flexreg_manual,type="survival",tidy=TRUE,ci=TRUE,se=TRUE,t=c(0,100,200,1000)),
                    param_dist=rep("gompertz",12),
                    dist_desc=rep("gompertz",12))
  
  set.seed(1234)
  tmp_hazard=summary(flexreg_manual,type="hazard",tidy=TRUE,ci=TRUE,se=TRUE,t=c(0,100,200,1000))
  
  flexreg_lcl_01=cbind(flexreg_lcl,
                       est_hazard=tmp_hazard$est,
                       lcl_hazard=tmp_hazard$lcl,
                       ucl_hazard=tmp_hazard$ucl,
                       se_hazard=tmp_hazard$se)
  
  expect_equal(flexreg_lcl_01,
               test_lcl$gomp,
               ignore_attr = TRUE)
  
  #check attributes match for spline knots feature
  expect_equal(unique(extract_est(spline_fits)$hazard_1k$distr_desc),
               add_info_fit_desc(spline_fits, exp=TRUE, digits=2),
               ignore_attr=TRUE)
  
  expect_equal(unique(extract_est(spline_fits,exp=FALSE,digits=3)$hazard_1k$distr_desc),
               add_info_fit_desc(spline_fits, exp=FALSE, digits=3),
               ignore_attr=TRUE)
  
  expect_equal(unique(extract_est(spline_fits, custom_model_label = FALSE)$hazard_1k$distr_desc),
               add_info_fit_desc(spline_fits, exp=TRUE, digits=2, custom_model_label = FALSE),
               ignore_attr=TRUE)
  
  
})


test_that("extract_est - double programming - produces survival times/hazard estimates as expected", {
  
  #flexsurvreg
  flexreg_manual=flexsurv::flexsurvreg(formula= test_formula,data = tte, dist="gompertz")
  flexreg_est=cbind(summary(flexreg_manual,type="survival",tidy=TRUE,ci=FALSE,t=0:10),
                    param_dist=rep("gomp",33),
                    dist_desc=rep("gomp",33),
                    est_hazard=summary(flexreg_manual,type="hazard",tidy=TRUE,ci=FALSE,t=0:10)$est)
  
  #res
  expect_equal(flexreg_est,
               extract_est(fits=list(gomp=flexsurv_fits$gompertz), t=0:10)$gomp,
               ignore_attr = TRUE)
  
  
  #flexsurvspline
  #res
  set.seed=7902
  spline_manual<-flexsurv::flexsurvspline(formula= test_formula,data = tte, scale="hazard",k=1,na.action=na.omit,
                                          method = "Nelder-Mead")
  spline_est=cbind(summary(spline_manual,type="survival",tidy=TRUE,ci=FALSE,t=0:10),
                   param_dist=rep("hazard_1k",33),
                   dist_desc=rep(add_info_fit_desc(fits=list(hazard_1k=spline_manual)),33),
                   est_hazard=summary(spline_manual,type="hazard",tidy=TRUE,ci=FALSE,t=0:10)$est)
  
  #res
  expect_equal(spline_est,
               extract_est(fits=list(hazard_1k=spline_fits$hazard_1k), t=0:10)$hazard_1k,
               ignore_attr = TRUE)
  
  
  #combination of 2
  expect_equal(list(hazard_1k=spline_est,
                    gomp=flexreg_est),
               extract_est(fits=list(hazard_1k=spline_fits$hazard_1k,
                                     gomp=flexsurv_fits$gompertz),
                           t=0:10),
               ignore_attr = TRUE)
  
  #run with ci and se
  expect_silent(extract_est(fits=flexsurv_fits,
                            t=c(0,100,200,1000),
                            ci=TRUE,
                            se=TRUE,
                            B=1000,
                            cl = 0.95,
                            na.action = na.pass))
  
})


test_that("add_info_fit_desc - double programming - produces updated names as expected", {
  
  #Compare with default values
  expect_equal(add_info_fit_desc(fits=spline_fits),
               glue::glue("{names(spline_fits)} ({glue::glue_collapse(round(head(exp(spline_fits$hazard_1k$knots[-1]),-1),digits={2}),sep=', ')})"),
               ignore_attr=TRUE)
  
  #Compare with custom rounding and not exponential
  expect_equal(add_info_fit_desc(fits=spline_fits,exp=FALSE,digits=3),
               glue::glue("{names(spline_fits)} ({glue::glue_collapse(round(head(spline_fits$hazard_1k$knots[-1],-1),digits={3}),sep=', ')})"),
               ignore_attr=TRUE)
  
  #Compare with spline knots locations removed (custom_model_label = FALSE)
  expect_equal(add_info_fit_desc(fits=spline_fits,exp=FALSE,digits=3,custom_model_label=FALSE),
               glue::glue("{names(spline_fits)}"),
               ignore_attr=TRUE)
  
})


test_that("add_log_rest_row - double programming - produces updated names as expected", {
  
  data_compare<-merge(tibble::as_tibble(flexsurv_fits$gompertz$res,rownames = "key"),
                      tibble::as_tibble(flexsurv_fits$gompertz$res.t,rownames = "key"),by="key")
  
  data_compare$estisequal=data_compare$est.x==data_compare$est.y
  
  data_compare$newkey=ifelse(data_compare$estisequal,data_compare$key,glue::glue("log({data_compare$key})"))
  
  expect_equal(sort(add_log_rest_row(fit=flexsurv_fits$gompertz)),
               sort(data_compare$newkey),
               ignore_attr=TRUE)
  
  
})