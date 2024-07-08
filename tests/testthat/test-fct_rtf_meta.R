# path to dummy data
data(adsl)
data(adtte)

# dummy directory for outputs
tmp_dir <- tempdir()
tmp_dir_all <- file.path(tmp_dir, "all")
if (dir.exists(tmp_dir_all)) unlink(tmp_dir_all, recursive = TRUE)
dir.create(tmp_dir_all, recursive = TRUE)


test_that("fct_rtf_meta works", {
  # check that function extracts formatting for all outputs when output_suffix="all"
  # expect 3 (sub-)lists
  devt1 <- fct_rtf_meta(output_suffix = list("all"),
                          rtf_title_pop = c("Subtitle 1", "(Population)"),
                          rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                          param = "Overall Survival",
                          therapy_arm = "Chemotherapy 0mg",
                          output_suffix_update = list(),
                          rtf_param=list(),
                          modified_value=list())
  expect_equal(length(devt1), 3) 
  
  # check that function extracts formatting for selected output as per output_suffix
  # expect list of 9 elements
  devt2 <- fct_rtf_meta(output_suffix = list("e0stdflex0gof"),
                          rtf_title_pop = c("Subtitle 1", "(Population)"),
                          rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                          param = "Overall Survival",
                          therapy_arm = "B: Placebo",
                          output_suffix_update = list(),
                          rtf_param=list(),
                          modified_value=list())
  expect_equal(length(devt2), 1) 
  grepl("Goodness", devt2$e0stdflex0gof$rtf_title[1], fixed=TRUE)
  expect_match(devt2$e0stdflex0gof$rtf_title[1],"Goodness")
  
  # expect 2 lists of 10 elements
  devt2a <- fct_rtf_meta(output_suffix = list("e0stdflex0gof","e0stdflex0fig0timeshort"),
                           rtf_title_pop = c("Subtitle 1", "(Population)"),
                           rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                           param = "Overall Survival",
                           therapy_arm = "B: Placebo",
                           output_suffix_update = list(),
                           rtf_param=list(),
                           modified_value=list())
  expect_equal(length(devt2), 1) 
  grepl("Goodness", devt2$e0stdflex0gof$rtf_title[1], fixed=TRUE)
  expect_match(devt2$e0stdflex0gof$rtf_title[1],"Goodness")
  
  
  # check that function modifies formatting for selected output as per output_suffix
  # expect list of 10 elements
  devt3 <- fct_rtf_meta(output_suffix=list("e0stdflex0gof"),
                        rtf_title_pop=c("Subtitle 1", "(Population)"),
                        rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                        param="Overall Survival",
                        time_horizon=c("20 Years"),
                        therapy_data_var = list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
                        models_title="Standard Parametric",
                        therapy_arm = c("B: Placebo", "A: Drug X"),
                        output_suffix_update = list("e0stdflex0gof", "e0stdflex0gof", "e0stdflex0gof"),
                        rtf_param=list("rtf_title[1]","header_label", "csv_output"),
                        modified_value=list("Goodness of Fit Statistics Resulting from Independent Standard Parametric Modeling for Overall Survival in Placebo Arm", "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Mean Rank AIC BIC", "FALSE")
                        )     
  expect_equal(length(devt3), 1) 
  expect_match(devt3$e0stdflex0gof$rtf_title[1],"Goodness")
  
  # check that function modifies formatting for selected output as per output_suffix inside the wrapper
  devt4 <- hta_flexsurv(population_from = mkanr::adsl,
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
                        id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                        rtf_prefix = "survce0os0arm10",
                        rtf_meta = fct_rtf_meta(output_suffix=list("all"),
                                                rtf_title_pop=c("Subtitle 1", "(Population)"),
                                                rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                                                param="Overall Survival",
                                                therapy_arm=levels(tte[[therapy_data_var$name]]),
                                                output_suffix_update = list("e0stdflex0gof", "e0stdflex0gof", "e0stdflex0gof"),
                                                rtf_param = list("rtf_title[1]","header_label", "csv_output"),
                                                models_title="Standard Parametric and Flexible Parametric Spline",
                                                modified_value=list("Goodness of Fit Statistics Resulting from DEVTEST", "Survival Model DEVTEST|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Mean Rank AIC BIC", "FALSE")),
                        outdata_path = tempdir(),
                        outgraph_path = tempdir(),
                        outtable_path = tempdir()
  )
  
  # load result from wrapper use
  load(file.path(tmp_dir, "survce0os0arm10e0stdflex0gof.RData"))
  devtest4_r = striprtf::read_rtf(file.path(tmp_dir, "survce0os0arm10e0stdflex0gof.rtf"))
  expect_match(devtest4_r[1],"Goodness of Fit Statistics Resulting from DEVTEST")
  expect_match(devtest4_r[4],"| Survival Model DEVTEST|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC | ")
})

test_that("fct_rtf_meta time_horizon and arm works", {
  # run with defaults 
  devtest1_metadata<-fct_rtf_meta(
    output_suffix = list("all"),
    rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
    rtf_footnote_pop = c("Database Cutoff Date: XXX"),
    param = "<endpoint>",
    therapy_arm = "B: Placebo",
    output_suffix_update = list(),
    rtf_param = list(),
    modified_value = list()
  )
  
  # run with time_horizon = c("20 Years") 
  devtest2_metadata<-fct_rtf_meta(
    output_suffix = list("all"),
    rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
    rtf_footnote_pop = c("Database Cutoff Date: XXX"),
    time_horizon = c("20 Years"),
    param = "<endpoint>",
    therapy_arm = "B: Placebo",
    output_suffix_update = list(),
    rtf_param = list(),
    modified_value = list()
  )
  
  expect_equal(devtest1_metadata,devtest2_metadata)
  
})
