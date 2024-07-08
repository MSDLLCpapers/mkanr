#temporary directory
tmp_dir <- tempdir()

# path to dummy data
data(adsl)
data(adtte)

#load dummy data
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

#fits list
test_formula <- stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1)

flexsurv_fits_temp<-run_models_flexsurvreg(
  preprocessed_data = tte,
  surv_formula = test_formula,
  dist_list = list(exponential="exponential",
                    weibull="weibull", 
                    lognormal="lognormal", 
                    loglogistic="llogis")
)

# test bulletproof functions by running code that you expect to throw an error based on bulletproof requirements

## input parameters check ----
test_that("bullet_proof: check input parameter section works as expected", {
  
  expect_warning(bullet_proof(population_from = adsl,
                                     observation_from = adtte,
                                     therapy_data_var = "TRT01A",
                                     doesnotexist="fake"),
                 regexp=fixed("Bulletproofing: no check available for: doesnotexist"))
  
})

# population_from/ observation_from checks ----
test_that("bullet_proof: check pop_form/obs_form checks section works as expected", {
  
  #population_from / observation_form
  #character
  expect_error(bullet_proof(population_from = NA,
                                   observation_from = adtte,
  ),
  regexp=fixed("NA \\(population_from\\) should be of type character and length 1"))
  
  
  #length 1
  expect_error(bullet_proof(population_from = c('test1', 'test2'),
                                   observation_from = adtte,
  ),regexp=("Bulletproofing: "))
  
  expect_error(bullet_proof(population_from = adsl,
                                   observation_from = c("tst1","test2"),
  ),regexp=("Bulletproofing: "))
  
  expect_error(bullet_proof(population_from = adsl,
                                   observation_from = "test2",
  ),regexp=("Bulletproofing: "))
  
  #file does not exist
  expect_error(bullet_proof(population_from = file.path(system.file('sasdata', package = 'mkanr'), "adsl2.sas7bdat"),
                                   observation_from = adtte,
  ))
  
  expect_error(bullet_proof(population_from = adsl,
                                   observation_from = file.path(system.file("sasdata", package = "mkanr"), "adtte2.sas7bdat"),
  ))
  
  #required variables
  expect_error(bullet_proof(population_from = adsl |> dplyr::select(-"USUBJID")
  ),"USUBJID should exist in population_from data")
  
  expect_error(bullet_proof(observation_from = adtte |> dplyr::select(-"USUBJID")
  ),"USUBJID should exist in observation_from data")
  
  expect_error(bullet_proof(observation_from = adtte |> dplyr::select(-"CNSR")
  ),"CNSR should exist in observation_from data")
  
  expect_error(bullet_proof(observation_from = adtte |> dplyr::select(-"PARAMCD")
  ),"PARAMCD should exist in observation_from data")
  
  
  #empty data check
  expect_error(bullet_proof(population_from = adsl |> dplyr::filter(USUBJID=="doesnotexist")
  ),"population_form is an empty dataset")
  
  expect_error(bullet_proof(observation_from = adtte |> dplyr::filter(USUBJID=="doesnotexist")
  ),"observation_from is an empty dataset")
  
  #unique key check
  expect_error(bullet_proof(population_from = rbind(adsl, adsl[1,])
  ),"Bulletproofing: population_from unique key should be USUBJID")
  
  expect_error(bullet_proof(observation_from = rbind(adtte, adtte[1,])
  ),"Bulletproofing: observation_from unique key should be USUBJID, PARAMCD")
  
})

## preprocessed_data check ----
test_that("bullet_proof: check preprocessed_data check work as expected", {
  
  expect_error(bullet_proof(preprocessed_data = 1),
               regexp = "Bulletproofing: 1 \\(preprocessed_data\\) should be of type data.frame")
  
})

# population_where/ observation_where checks ----
test_that("bullet_proof: check pop_where/obs_where checks section works as expected", {
  
  #population_from/observation_from requirements
  expect_error(bullet_proof(population_where = ITTFL=='Y'),
               "Bulletproofing: population_where check could not be performed without population_from parameter")
  
  expect_error(bullet_proof(observation_where = PARAMCD=='OS'),
               "Bulletproofing: observation_where check could not be performed without observation_from parameter")
  
  #variable used not available in data
  expect_error(bullet_proof(population_from = adsl,
                                   population_where = ITTFL=='Y' & RRR>2),
               "Bulletproofing: variable RRR used in population_where is not available in population_from")
  
  expect_error(bullet_proof(observation_from = adtte,
                                   observation_where = NOTEXIST=='OS'),
               "Bulletproofing: variable NOTEXIST used in observation_where is not available in observation_from")
  
  #combination result in empty data check
  expect_error(bullet_proof(population_from = adsl,
                                   population_where = TRT01P=='NOTEXIST'),
               "Bulletproofing: combination of population_from and population_where result in an empty dataset")
  
  expect_error(bullet_proof(observation_from = adtte,
                                   observation_where = PARAMCD=='OS((('),
               "Bulletproofing: combination of observation_from and observation_where result in an empty dataset")
  
  #unique key check
  expect_error(bullet_proof(population_from = rbind(adsl, adsl[1,]),
                            population_where = ITTFL=="Y"
  ),"Bulletproofing: combination of population_from and population_where unique key should be USUBJID")
  
  expect_error(bullet_proof(observation_from = rbind(adtte, adtte[1,]),
                                   observation_where = ITTFL=="Y"
  ),"Bulletproofing: combination of observation_from and observation_where unique key should be USUBJID, PARAMCD")
  
})


## analysis variable check ----
test_that("bullet_proof: check analysis_variable checks work as expected", {
  
  #list not correct format
  expect_error(bullet_proof(analysis_variable = list(name2="AVAL2", derivation="AVAL/7", label='Analysis Variable 2')),
               "Bulletproofing: analysis_variable should be of type list and have the following 3 elements: name, derivation, label")
  
  expect_error(bullet_proof(analysis_variable_unit = list(name="AVAL2", derivation="AVAL/7")),
               "Bulletproofing: analysis_variable_unit should be of type list and have the following 3 elements: name, derivation, label")
  
  #not character and/or length 1
  expect_error(bullet_proof(analysis_variable = list(name=NA, derivation="AVAL/7", label='Analysis Variable 2')),
               "Bulletproofing: elements of list provided in analysis_variable should be of type character and length equal to 1")
  
  expect_error(bullet_proof(analysis_variable_unit = list(name="AVAL2U", derivation=c("test","test2"), label='Analysis Variable 2')),
               "Bulletproofing: elements of list provided in analysis_variable_unit should be of type character and length equal to 1")
  
  expect_error(bullet_proof(analysis_variable = list(name="AVAL", derivation="AVAL/7", label=c("LABEL",1))),
               "Bulletproofing: elements of list provided in analysis_variable should be of type character and length equal to 1")
  
  #observation_from requirement
  expect_error(bullet_proof(analysis_variable = list(name="AVAL", derivation="AVAL/7", label='Analysis Variable 2')),
               "Bulletproofing: analysis_variable check for derivation element could not be performed without observation_from parameter")
  
  #variable in derivation not available in data
  expect_error(bullet_proof(observation_from = adtte,
                                   analysis_variable = list(name="AVAL", derivation="AVALX/7", label='Analysis Variable 2')),
               "Bulletproofing: variable AVALX used in analysis_variable\\$derivation is not available in observation_from data")
  
})

## therapy_data_var check ----
test_that("bullet_proof: check therapy_data_var checks work as expected", {
  
  #therapy_data_var name alone
  expect_error(bullet_proof(therapy_data_var = 1),
               "Bulletproofing: therapy_data_var name should be of type character and length 1")
  
  expect_error(bullet_proof(population_from = adsl,
                                   therapy_data_var = "TRT0XP"),
               "Bulletproofing: TRT0XP \\(therapy_data_var name\\) should exist in population_from data")
  
  #list not correct format
  expect_error(bullet_proof(therapy_data_var_list = list(name2="TRT01P",levels=NULL,labels=NULL,name_label=NULL)),
               "Bulletproofing: therapy_data_var list should be of type list and have the following 4 elements: name, levels, labels, name_label")
  
  #name not character and/or length 1
  expect_error(bullet_proof(therapy_data_var_list = list(name=1,levels=NULL,labels=NULL,name_label=NULL)),
               "Bulletproofing: element name of therapy_data_var list should be of type character and length equal to 1")
  
  #levels/labels not character or NULL
  expect_error(bullet_proof(therapy_data_var_list = list(name="TRT01P",levels=NA,labels=NULL,name_label=NULL)),
               "Bulletproofing: element levels of therapy_data_var list should be of type character or numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(therapy_data_var_list = list(name="TRT01P",levels=c('2','2'),labels=NA,name_label=NULL)),
               "Bulletproofing: element labels of therapy_data_var list should be of type character and length greater or equal to 1")
  
  #levels and labels should be same length
  expect_error(bullet_proof(therapy_data_var_list = list(name="TRT01P",levels=c('2','2'),labels=c('3'),name_label=NULL)))
  
  #population_from requirement
  expect_error(bullet_proof(therapy_data_var_list = list(name="TRT01PN",levels=c(1,2),labels=c("test1","test2"),name_label=NULL)),
               "Bulletproofing: therapy_data_var_list check could not be performed without population_from parameter")
  
  #levels provided not in data
  expect_error(bullet_proof(population_from = adsl,
                                   therapy_data_var_list = list(name="TRT01PN",levels=c(1,3),labels=c("test1","test2"),name_label=NULL)),
               "Bulletproofing: all therapy_data_var\\$levels should exist in values of TRT01PN in population_from data")
  
  
  
})

## time check ----
test_that("bullet_proof: check time section works as expected", {
  
  #not conform
  expect_error(bullet_proof(time = 1),
               "Bulletproofing: time name should be of type character and length 1")
  
  expect_error(bullet_proof(time = c("test","test2")),
               "Bulletproofing: time name should be of type character and length 1")
  
  expect_error(bullet_proof(observation_from = adtte,
                                   time = "test"),
               "Bulletproofing: test \\(time\\) should exist in observation_from data")
})

## rename_output check ----
test_that("bullet_proof: check rename_output section works as expected", {
  
  #not conform
  expect_error(bullet_proof(rename_output = 1),
               "Bulletproofing: rename_output should be of type character and length equal to 1")
  
  expect_error(bullet_proof(rename_output = c("test","test2")),
               "Bulletproofing: rename_output should be of type character and length equal to 1")
  
  expect_error(bullet_proof(rename_output = "not_conform"),
               "Bulletproofing: rename_output should only have lowercase letters and numbers")
})

## outdata_path check ----
test_that("bullet_proof: check paths section works as expected", {
  
  #path does not exist
  expect_error(bullet_proof(outdata_path = "NOTEXIST"),
               "Bulletproofing: NOTEXIST \\(outdata_path\\) directory does not exist, please enter valid path")
  
  #type not conform
  expect_error(bullet_proof(outtable_path = 1),
               "Bulletproofing: 1 \\(outtable_path\\) should be of type character and length 1")
  
  expect_error(bullet_proof(outgraph_path = NA),
               "Bulletproofing: NA \\(outgraph_path\\) should be of type character and length 1")
  
  #not length 1
  expect_error(bullet_proof(outdata_path = c("test1","test2")),
               "Bulletproofing: test1test2 \\(outdata_path\\) should be of type character and length 1")
  
})

## surv_formula check ----
test_that("bullet_proof: check surv_formula section works as expected", {
  
  #preprocessed_data requirement
  expect_error(bullet_proof(surv_formula = formula(survival::Surv(AVAL3,1-CNSR) ~ TRT01P)),
               "Bulletproofing: surv_formula requires preprocessed_data or preprocessed_data_km to be checked"
  )
  
  #not class formula
  expect_error(bullet_proof(surv_formula = tte,
                                   preprocessed_data=tte),
               "Bulletproofing: tte \\(surv_formula\\) should be of class formula")
  
  #not survival::Surv prefix
  expect_error(bullet_proof(surv_formula = formula(Surv(AVAL3,1-CNSR) ~ TRT01P),
                                   preprocessed_data=tte),"Bulletproofing: survival::Surv should be used in formula")
  
  #variable not in tte data
  expect_error(bullet_proof(surv_formula = formula(survival::Surv(AVAL3,1-CNSR) ~ TRT01P),
                                   preprocessed_data=tte),
               "Bulletproofing: formula\\(survival::Surv\\(AVAL3, 1 - CNSR\\) ~ TRT01P\\) \\(surv_formula\\) can not be used with preprocessed_tte due to error/warning: object 'AVAL3' not found")
  
  #variable not in tte_km data
  expect_error(bullet_proof(surv_formula = formula(survival::Surv(AVAL3,1-CNSR) ~ TRT01P),
                                   preprocessed_data_km=tte_km),
               "Bulletproofing: formula\\(survival::Surv\\(AVAL3, 1 - CNSR\\) ~ TRT01P\\) \\(surv_formula\\) can not be used with preprocessed_tte_km due to error/warning: object 'AVAL3' not found")

})

## fits check ----
test_that("bullet_proof: check fits checks work as expected", {
  
  #not list
  expect_error(bullet_proof(fits_flexsurv = 1),
               "Bulletproofing: 1 \\(fits_flexsurv\\) should be a list of length greater of equal to 1")
  expect_error(bullet_proof(fits_survreg = NA),
               "Bulletproofing: NA \\(fits_survreg\\) should be a list of length greater of equal to 1")
  expect_error(bullet_proof(fits = NA),
               "Bulletproofing: NA \\(fits\\) should be a list of length greater of equal to 1")
  
  #empty list
  expect_error(bullet_proof(fits_flexsurv = list()),
               "Bulletproofing: list\\(\\) \\(fits_flexsurv\\) should be a list of length greater of equal to 1")
  expect_error(bullet_proof(fits_survreg = list()),
               "Bulletproofing: list\\(\\) \\(fits_survreg\\) should be a list of length greater of equal to 1")
  expect_error(bullet_proof(fits = list()),
               "Bulletproofing: list\\(\\) \\(fits\\) should be a list of length greater of equal to 1")
  
  #not all elements named
  expect_error(bullet_proof(fits_flexsurv = list(a=flexsurv_fits_temp$exponential,flexsurv_fits_temp$weibull)),
               "Bulletproofing: list\\(a = flexsurv_fits_temp\\$exponential, flexsurv_fits_temp\\$weibull\\) \\(fits_flexsurv\\) elements should all be named")

  #not all element correct class
  expect_error(bullet_proof(fits_survreg = list(a=flexsurv_fits_temp$exponential,b=flexsurv_fits_temp$weibull)),
               "Bulletproofing: element a of fits list should be of class survreg")
  expect_error(bullet_proof(fits = list(a=flexsurv_fits_temp$exponential,b=lm(AGE ~ SEX, data=tte))),
               "Bulletproofing: element b of fits list should be of class survreg or flexsurvreg")
  
})

## break_point check ----
test_that("bullet_proof: check break_point checks work as expected", {
  
  #not conform
  expect_error(bullet_proof(break_point = "test"),
               "Bulletproofing: break_point should be either NULL or type numeric and length equal to 1")
  expect_error(bullet_proof(break_point = c(1,2)),
               "Bulletproofing: break_point should be either NULL or type numeric and length equal to 1")
  expect_error(bullet_proof(break_point = -4),
               "Bulletproofing: break_point should be greater or equal to 0")
  
  #does not match surv_formula, preprocessed_data and preprocessed_data_km result
  expect_error(bullet_proof(break_point = 9,
                                   surv_formula = formula(survival::Surv(AVAL2,1-CNSR) ~ TRT01P),
                                   preprocessed_data=tte,
                                   preprocessed_data_km=tte_km),
               "Bulletproofing: break_point should be equal to 0 \\(max\\(preprocessed_data_km\\$endpoint\\) - max\\(preprocessed_data\\$endpoint\\)\\)")
  
})

## custom_model_label check ----
test_that("bullet_proof: check custom_model_label checks work as expected", {
  
  #not conform
  expect_error(bullet_proof(custom_model_label = c("Y")),
               "Bulletproofing: custom_model_label should be equal to either TRUE or FALSE")
})

## id_analysis ----
test_that("bullet_proof: check id_analysis checks work as expected", {
  
  #not conform
  expect_error(bullet_proof(id_analysis=''),
               "Bulletproofing:")
  expect_error(bullet_proof(id_analysis=1),
               "Bulletproofing: id_analysis should be type character and length greater or equal to 1")
  expect_error(bullet_proof(id_analysis=NA),
               "Bulletproofing: id_analysis should be type character and length greater or equal to 1")
  expect_error(bullet_proof(id_analysis='doesnotexist'),
               "Bulletproofing:")
  
})

## wrapper input checks ----
test_that("bullet_proof: check wrapper input checks work as expected", {
  
  #not conform
  expect_error(bullet_proof(rtf_title_pop = 1,
                                   rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                                   rtf_prefix = paste0("survce0", "os", "0"),
                                   param = "Overall Survival"),"Bulletproofing: rtf_title_pop should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
                                   rtf_footnote_pop = NA,
                                   rtf_prefix = paste0("survce0", "os", "0"),
                                   param = "Overall Survival"),"Bulletproofing: rtf_footnote_pop should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
                                   rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                                   rtf_prefix = 1,
                                   param = "Overall Survival"),"Bulletproofing: rtf_prefix should be type character and length greater or equal to 1")

  expect_error(bullet_proof(rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
                                   rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                                   rtf_prefix = paste0("survce0", "0", "os", "0"),
                                   param = NULL),"Bulletproofing: param should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(time_unit=NA),"Bulletproofing: time_unit should be type character and length equal to 1")
  
  expect_error(bullet_proof(treatment_label=111),"Bulletproofing: treatment_label should be type character")
  
  expect_error(bullet_proof(time_unit='T'),"Bulletproofing: time_unit should be equal to 'D' for analysis in Days, 'W' for analysis in Weeks, 'M' for analysis in Months or 'Y' for analysis in Years")
  
  expect_error(bullet_proof(parametric_list=list(),
                                   spline_list=list()),
               "Bulletproofing: at least one element should exist in either parametric_list or spline_list")
  
  expect_error(bullet_proof(parametric_list=NA,
                                   spline_list=list()),
               "Bulletproofing: parametric_list should be type list and either equal to list\\(\\) or of length greater or equal to 1")
  
  expect_error(bullet_proof(parametric_list=NA),
               "Bulletproofing: parametric_list should be type list and either equal to list\\(\\) or of length greater or equal to 1")
  
  expect_error(bullet_proof(ci=TESTVAL))
  
})


## Output formatting checks ----
test_that("bullet_proof: check output formatting checks work as expected", {
  
  expect_error(bullet_proof(xlab = 1),
               "Bulletproofing: xlab should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(ylab = 1),
               "Bulletproofing: ylab should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(col_just = 1),
               "Bulletproofing: col_just should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(header_label = 1),
               "Bulletproofing: header_label should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(levelvar = 1),
               "Bulletproofing: levelvar should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(levelvar_legend = 1),
               "Bulletproofing: levelvar_legend should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(models_title = 1),
               "Bulletproofing: models_title should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(palette = 1),
               "Bulletproofing: palette should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_footnote = 1),
               "Bulletproofing: rtf_footnote should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_title = 1),
               "Bulletproofing: rtf_title should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_title_pop = 1),
               "Bulletproofing: rtf_title_pop should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(rtf_footnote_pop = 1),
               "Bulletproofing: rtf_footnote_pop should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(therapy_arm = 1),
               "Bulletproofing: therapy_arm should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(treatment_label = 1),
               "Bulletproofing: treatment_label should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(time_horizon = 1),
               "Bulletproofing: time_horizon should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(xvar = 1),
               "Bulletproofing: xvar should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(yvar = 1),
               "Bulletproofing: yvar should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(facetvar = 1),
               "Bulletproofing: facetvar should be either null or type character and length greater or equal to 1")
  
  expect_error(bullet_proof(fig_height = "test"),
               "Bulletproofing: fig_height should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(fig_line_size = "test"),
               "Bulletproofing: fig_line_size should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(fig_res = "test"),
               "Bulletproofing: fig_res should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(fig_scale = "test"),
               "Bulletproofing: fig_scale should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(fig_width = "test"),
               "Bulletproofing: fig_width should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(digits = "test"),
               "Bulletproofing: digits should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(legend_textsize = "test"),
               "Bulletproofing: legend_textsize should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(rel_col_widths = "test"),
               "Bulletproofing: rel_col_widths should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(seed_value = "test"),
               "Bulletproofing: seed_value should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(predict_time = "test"),
               "Bulletproofing: predict_time should be type numeric and length greater or equal to 1")
  
  expect_error(bullet_proof(orientation = 2),
               "Bulletproofing: orientation should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(orientation = "horizontal"),
               "Bulletproofing: orientation should be either 'portrait' or 'landscape'")
  
  expect_error(bullet_proof(legend_position = 2),
               "Bulletproofing: legend_position should be type character and length greater or equal to 1")
  
  expect_error(bullet_proof(legend_position = "center"),
               "Bulletproofing: legend_position should be either 'none', 'left', 'right', 'bottom' or 'top'")
  
  expect_error(bullet_proof(gof_arrange = 2),
               "Bulletproofing: gof_arrange should be either NULL or type character and length greater or equal to 1")
  
  expect_error(bullet_proof(gof_arrange = "Rank Mean"),
               "Bulletproofing: gof_arrange should be either 'Rank AIC', 'Rank BIC' or 'Rank Mean AIC BIC' ")
  
  expect_error(bullet_proof(custom_model_label = "test"),
               "Bulletproofing: custom_model_label should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(csv_output = "test"),
               "Bulletproofing: csv_output should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(ci = "test"),
               "Bulletproofing: ci should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(se = "test"),
               "Bulletproofing: se should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(exp = "test"),
               "Bulletproofing: exp should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(boundknots = "test"),
               "Bulletproofing: boundknots should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(fig_grid_x = "test"),
               "Bulletproofing: fig_grid_x should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(fig_grid_y = "test"),
               "Bulletproofing: fig_grid_y should be equal to either TRUE or FALSE")
  
  expect_error(bullet_proof(output_suffix_update = "test"),
               "Bulletproofing: output_suffix_update should be either a list or NULL")
  
  expect_error(bullet_proof(rtf_param = "test"),
               "Bulletproofing: rtf_param should be either a list or NULL")
  
  expect_error(bullet_proof(scalexk_list = "test"),
               "Bulletproofing: scalexk_list should be either a list or NULL")
  
  expect_error(bullet_proof(modified_value = "test"),
               "Bulletproofing: modified_value should be a list")
  
  expect_error(bullet_proof(output_suffix = "test"),
               "Bulletproofing: output_suffix should be a list")

  expect_error(bullet_proof(data = "test"),
               "Bulletproofing: data should be a data frame")
  
  expect_error(bullet_proof(facetvar_legend = 1),
               "Bulletproofing: facetvar_legend should be of class function \\(ggplot2::labeller\\) and length 1")
  
})


## full wrapped call check ----
test_that("bullet_proof: check clean call work as expected", {
  
  test_func <- function(...){
    bullet_proof(...)
  }
  
  
  #preprocessed_data requirement
  expect_true(test_func(population_from =adsl,
                        population_where = ITTFL=='Y' & TRT01P=="B: Placebo",
                        observation_from = adtte,
                        observation_where = PARAMCD=='OS',
                        analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
                        analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
                        therapy_data_var_list=list(name="TRT01P",levels=NULL,labels=NULL,name_label=NULL),
                        therapy_data_var="TRT01A",
                        outdata_path=tmp_dir,
                        outgraph_path=tmp_dir,
                        outtable_path=tmp_dir,
                        break_point = 0,
                        surv_formula = formula(survival::Surv(AVAL2,1-CNSR) ~ 1),
                        preprocessed_data=tte,
                        preprocessed_data_km=tte_km,
                        fits_flexsurv=flexsurv_fits_temp, 
                        fits = flexsurv_fits_temp,
                        id_analysis=c('e0stdflex0gof',
                                      'e0stdflex0fig0timeshort',
                                      'e0stdflex0fig0timelong'),
                        rtf_title_pop = c("Subtitle 1", "(ITT Population)"),
                        rtf_footnote_pop = c("Database Cutoff Date: XXX"),
                        rtf_prefix = paste0("survce0", "0", "os", "0"),
                        param = "Overall Survival",
                        time_unit='W',
                        ci= TRUE,
                        parametric_list=list(`Exponential`="exp",
                                             `Weibull`="weibull", `Gompertz`="gompertz",
                                             `Gamma`="gamma",
                                             `Log-normal`="lognormal", 
                                             `Log-logistic`="llogis", 
                                             `Generalized Gamma`="gengamma"),
                        spline_list=list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
                                         `Spline on hazard - 2 knots`=list(scale="hazard",k=2),
                                         `Spline on hazard - 3 knots`=list(scale="hazard",k=3),
                                         `Spline on odds - 1 knot`=list(scale="odds",k=1),
                                         `Spline on odds - 2 knots`=list(scale="odds",k=2),
                                         `Spline on odds - 3 knots`=list(scale="odds",k=3),
                                         `Spline on probit - 1 knot`=list(scale="normal",k=1),
                                         `Spline on probit - 2 knots`=list(scale="normal",k=2),
                                         `Spline on probit - 3 knots`=list(scale="normal",k=3))
  )
  )
  
  
})