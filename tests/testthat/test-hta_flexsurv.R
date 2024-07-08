# path to dummy data
data(adsl)
data(adtte)

#dummy directory for outputs
tmp_dir = tempdir()
tmp_dir_all = file.path(tmp_dir, "all")
if (dir.exists(tmp_dir_all))
  unlink(tmp_dir_all, recursive = TRUE)
dir.create(tmp_dir_all, recursive = TRUE)

test_that("hta_flexsurv specific bulletproofing works", {
  
  #more than 1 arm
  expect_error(
    hta_flexsurv(population_from = mkanr::adsl,
                 population_where = (ITTFL == "Y"),
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
                 id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                 rtf_prefix = "survce0os0arm10",
                 rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                         rtf_title_pop=c("Subtitle 1", "(Population)"),
                                         rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                         param="Overall Survival",
                                         therapy_arm=levels(tte[[therapy_data_var$name]]),
                                         output_suffix_update = list(),
                                         rtf_param=list(),
                                         models_title="Standard Parametric and Flexible Parametric Spline",
                                         modified_value=list()),
                 outdata_path = tempdir(),
                 outgraph_path = tempdir(),
                 outtable_path = tempdir()
    ),"hta_flexsurv : analysis requires only one arm for therapy_data_var in data created by the combination of population_from, population_where, observation_from and observation_where"
  )

})

test_that("hta_flexsurv - double programming - expected output are reproduceable withtout wrapper ", {
  
  dev_results<-hta_flexsurv(population_from = mkanr::adsl,
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
                            spline_list = list(),
                            predict_time = 0:1040,
                            id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                            rtf_prefix = "dtest0",
                            rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                                    rtf_title_pop=c("Subtitle 1", "(Population)"),
                                                    rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                                    param="Overall Survival",
                                                    therapy_arm=levels(tte[[therapy_data_var$name]]),
                                                    output_suffix_update = list(),
                                                    rtf_param=list(),
                                                    models_title="Standard Parametric and Flexible Parametric Spline",
                                                    modified_value=list()),
                            outdata_path = tempdir(),
                            outgraph_path = tempdir(),
                            outtable_path = tempdir()
  )
  
  #load dummy data
  tte <- preprocess_data(
    population_from = adsl,
    population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
    observation_from = adtte,
    observation_where = (PARAMCD == "OS"),
    analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
    analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
    therapy_data_var=list(name="TRT01P",levels=NULL,labels=NULL,name_label="Planned Treatment for Period 01")
  )
  
  tte_km <- preprocess_data(
    population_from = adsl,
    population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
    observation_from = adtte,
    observation_where =(PARAMCD == "OS"),
    analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
    analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
    therapy_data_var=list(name="TRT01P",levels=NULL,labels=NULL,name_label="Planned Treatment for Period 01")
  )
  
  test_formula <- stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1)
  
  #fits list
  flexsurv_fits<-run_models_flexsurvreg(
    preprocessed_data = tte,
    surv_formula = test_formula,
    dist_list = list(`Exponential`="exp",
                     `Weibull`="weibull",
                     `Log-normal`="lognormal", 
                     `Log-logistic`="llogis",
                     `Gompertz`="gompertz", 
                     `Gamma`="gamma", 
                     `Generalized Gamma`="gengamma")
  )
  
  #table
  gof_dtest<- flexsurv_fits %>%
    extract_gof() %>%
    format_gof()
  
  ## load result from wrapper use
  load(file.path(tmp_dir, "dtest0e0stdflex0gof.RData"))

    
  ## function result and wrapper saved Rdata expected equal
  expect_equal(dtest0e0stdflex0gof, gof_dtest)

  #plots data
  short_dtest<-flexsurv_fits %>%
    extract_est(t=0:max(tte["AVAL2"])) %>%
    format_est(preprocessed_data_km=tte)
  
  long_dtest<-flexsurv_fits %>%
    extract_est(t=0:1040) %>%
    format_est(preprocessed_data_km=tte)
  
  ## load result from wrapper use
  load(file.path(tmp_dir, "dtest0e0stdflex0fig0timeshort.RData"))
  load(file.path(tmp_dir, "dtest0e0stdflex0fig0timelong.RData"))
  
  ## function result and wrapper saved Rdata expected equal
  expect_equal(dtest0e0stdflex0fig0timeshort$data, short_dtest)
  expect_equal(dtest0e0stdflex0fig0timelong$data, long_dtest)
  
})

test_that("hta_flexsurv - output produced as expected", {
  
  dev_results<-hta_flexsurv(population_from = mkanr::adsl,
                            population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
                            observation_from = mkanr::adtte,
                            observation_where = (PARAMCD == "OS"),
                            therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
                            time = "AVAL",
                            time_unit = "W",
                            parametric_list = list(),
                            spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                               `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                               `Spline on normal - 1 knots`=list(scale="normal",k=1)),
                            predict_time = 0:1040,
                            id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                            rtf_prefix = "survce0onearm0",
                            rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                                    rtf_title_pop=c("Subtitle 1", "(Population)"),
                                                    rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                                    param="Overall Survival",
                                                    therapy_arm=levels(tte[[therapy_data_var$name]]),
                                                    output_suffix_update = list(),
                                                    rtf_param=list(),
                                                    models_title="Standard Parametric and Flexible Parametric Spline",
                                                    modified_value=list()),
                            outdata_path = tempdir(),
                            outgraph_path = tempdir(),
                            outtable_path = tempdir()
  )
  
  expect_equal(length(dev_results),3)
  
  ## expect rtf
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0gof",".rtf")))
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0fig0timelong",".rtf")))
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0fig0timeshort",".rtf")))

  ## expect png
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0fig0timelong",".png")))
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0fig0timeshort",".png")))

  #expect csv
  expect_snapshot_file(file.path(tmp_dir,paste0("survce0onearm0", "e0stdflex0gof",".csv")))

})


test_that("hta_flexsurv - check that label customisation work with custom_model_label parameter ", {
  # without knot locations in label (default)
  dev_results_lab<-hta_flexsurv(population_from = mkanr::adsl,
                                population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
                                observation_from = mkanr::adtte,
                                observation_where = (PARAMCD == "OS"),
                                therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
                                time = "AVAL",
                                time_unit = "W",
                                parametric_list = list(`Exponential`="exp"),
                                spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                                   `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                                   `Spline on normal - 1 knots`=list(scale="normal",k=1)),
                                predict_time = 0:1040,
                                id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                                rtf_prefix = "dtest20",
                                rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                                        rtf_title_pop=c("Subtitle 1", "(Population)"),
                                                        rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                                        param="Overall Survival",
                                                        therapy_arm=levels(tte[[therapy_data_var$name]]),
                                                        output_suffix_update = list(),
                                                        rtf_param=list(),
                                                        models_title="Standard Parametric and Flexible Parametric Spline",
                                                        modified_value=list()),
                                outdata_path = tempdir(),
                                outgraph_path = tempdir(),
                                outtable_path = tempdir()
  )

  ## load result from wrapper use
  load(file.path(tmp_dir, "dtest20e0stdflex0gof.RData"))
  load(file.path(tmp_dir, "dtest20e0stdflex0fig0timeshort.RData"))
  load(file.path(tmp_dir, "dtest20e0stdflex0fig0timelong.RData"))
  
  
  ## expect equal results
  expect_equal(dtest20e0stdflex0gof$`Survival Model`, 
               c("Exponential", "Spline on hazard - 1 knot", "Spline on odds - 2 knots", "Spline on normal - 1 knots"))
  
  expect_equal(levels(dtest20e0stdflex0fig0timeshort$data$group_primary), 
               c("Kaplan-Meier", "Exponential", "Spline on hazard - 1 knot", "Spline on odds - 2 knots", "Spline on normal - 1 knots"))
  
  expect_equal(levels(dtest20e0stdflex0fig0timelong$data$group_primary), 
               c("Kaplan-Meier", "Exponential", "Spline on hazard - 1 knot", "Spline on odds - 2 knots", "Spline on normal - 1 knots"))
  
  
  # with knot locations in label (default)
  dev_results_lab<-hta_flexsurv(population_from = mkanr::adsl,
                                population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
                                observation_from = mkanr::adtte,
                                observation_where = (PARAMCD == "OS"),
                                therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
                                time = "AVAL",
                                time_unit = "W",
                                parametric_list = list(`Exponential`="exp"),
                                spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                                   `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                                   `Spline on normal - 1 knots`=list(scale="normal",k=1)),
                                predict_time = 0:1040,
                                id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                                rtf_prefix = "dtest30",
                                rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                                        rtf_title_pop=c("Subtitle 1", "(Population)"),
                                                        rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                                        param="Overall Survival",
                                                        therapy_arm=levels(tte[[therapy_data_var$name]]),
                                                        output_suffix_update = list('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                                                        rtf_param=list('custom_model_label', 'custom_model_label', 'custom_model_label'),
                                                        models_title="Standard Parametric and Flexible Parametric Spline",
                                                        modified_value=list(TRUE,TRUE,TRUE)),
                                outdata_path = tempdir(),
                                outgraph_path = tempdir(),
                                outtable_path = tempdir()
  )
  
  ## load result from wrapper use
  load(file.path(tmp_dir, "dtest30e0stdflex0gof.RData"))
  load(file.path(tmp_dir, "dtest30e0stdflex0fig0timeshort.RData"))
  load(file.path(tmp_dir, "dtest30e0stdflex0fig0timelong.RData"))
  
  
  ## expect equal results
  expect_equal(dtest30e0stdflex0gof$`Survival Model`, 
               c("Exponential", "Spline on hazard - 1 knot (20.63)", "Spline on odds - 2 knots (17.35, 26.82)", "Spline on normal - 1 knots (20.63)"))
  
  expect_equal(levels(dtest30e0stdflex0fig0timeshort$data$group_primary), 
               c("Kaplan-Meier","Exponential","Spline on hazard - 1 knot (20.63)","Spline on odds - 2 knots (17.35, 26.82)", "Spline on normal - 1 knots (20.63)"))
  
  expect_equal(levels(dtest30e0stdflex0fig0timelong$data$group_primary), 
               c("Kaplan-Meier","Exponential","Spline on hazard - 1 knot (20.63)","Spline on odds - 2 knots (17.35, 26.82)", "Spline on normal - 1 knots (20.63)"))

})