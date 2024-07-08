# path to dummy data
## R format
data(adsl)
data(adtte)

## SAS format
datapath <- system.file("sasdata", package = "mkanr")

adsl_sas <- file.path(datapath, "adsl.sas7bdat")
adtte_sas <- file.path(datapath, "adtte.sas7bdat")

test_that("preprocess_data double programming works - default parameters (except data path) for R data format", {
  
  expect_equal(
    adsl %>%
      dplyr::left_join(adtte, by = "USUBJID",multiple = "all") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P)
      ),
    preprocess_data(
      population_from = adsl,
      population_where = ,
      observation_from = adtte,
      observation_where = ,
      analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
      therapy_data_var=list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL)
      ),
    ignore_attr = TRUE
  )
})

test_that("preprocess_data double programming works - default parameters (except data path) for SAS data format", {
  
  expect_equal(
    haven::read_sas(adsl_sas) %>%
      dplyr::left_join(haven::read_sas(adtte_sas), by = "USUBJID",multiple = "all") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P)
      ),
    preprocess_data(
      population_from = adsl_sas,
      population_where = ,
      observation_from = adtte_sas,
      observation_where = ,
      analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
      therapy_data_var=list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL)
    ),
    ignore_attr = TRUE
  )
})

test_that("preprocess_data double programming works - custom treatment label", {
  
  expect_equal(
    adsl %>%
      dplyr::left_join(adtte, by = "USUBJID",multiple = "all") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P,levels=c("Zembrolizumab 1mg", "Chemotherapy 0mg" ),labels=c("Zembrolizumab 1mg", " Chemotherapy 0mg"))
      ),
    preprocess_data(
      population_from = adsl,
      population_where = ,
      observation_from = adtte,
      observation_where = ,
      analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
      therapy_data_var=list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=c("Zembrolizumab 1mg", "Chemotherapy 0mg" ),labels=c("Zembrolizumab 1mg", " Chemotherapy 0mg"))
      ),
    ignore_attr = TRUE
  )
})

test_that("preprocess_data double programming works - population_where works as expected", {

  expect_equal(
    adsl %>%
      dplyr::filter(ITTFL=="Y" & TRT01P=="B: Placebo") %>%
      dplyr::left_join(adtte, by = "USUBJID",multiple = "all") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P)
      ),
    preprocess_data(
      population_from = adsl,
      population_where = ITTFL=="Y" & TRT01P=="B: Placebo",
      observation_from = adtte,
      observation_where = ,
      analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
    ),
    ignore_attr = TRUE
  )
})


test_that("preprocess_data double programming works - observation_where works as expected", {
  
  expect_equal(
    adsl %>%
      dplyr::filter(ITTFL=="Y" & TRT01P=="B: Placebo") %>%
      dplyr::left_join(adtte, by = "USUBJID",multiple = "all") %>%
      dplyr::filter(PARAMCD=="OS") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P)
      ),
    preprocess_data(
      population_from = adsl,
      population_where = ITTFL=="Y" & TRT01P=="B: Placebo",
      observation_from = adtte,
      observation_where =  PARAMCD=="OS",
      analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
    ),
    ignore_attr = TRUE
  )
})

test_that("preprocess_data double programming works - complex analysis variable derivation works as expected", {
  
  expect_equal(
    adsl %>%
      dplyr::filter(ITTFL=="Y" & TRT01P=="B: Placebo") %>%
      dplyr::left_join(adtte, by = "USUBJID",multiple = "all") %>%
      dplyr::filter(PARAMCD=="OS") %>%
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., ""))) %>%
      dplyr::select(!dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x")) %>%
      dplyr::mutate(
        AVAL2 = AVAL / 7 - 10,
        AVAL2U = "WEEKS",
        TRT01P = factor(TRT01P)
      ),
    preprocess_data(
      population_from = adsl,
      population_where = ITTFL=="Y" & TRT01P=="B: Placebo",
      observation_from = adtte,
      observation_where =  PARAMCD=="OS",
      analysis_variable = list(name="AVAL2", derivation="AVAL/7-10", label='Analysis Variable 2'),
      analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
    ),
    ignore_attr = TRUE
  )
})


test_that("preprocess_data final data empty works as expected", {
  
  expect_error(preprocess_data(
    population_from = adsl,
    population_where = ITTFL=="Y" & TRT01P=="B: Placebo",
    observation_from = adtte,
    observation_where =  PARAMCD=="OS" & AVAL2>10 & TRT01P=="B: Placebo",
    analysis_variable = list(name="AVAL2", derivation="AVAL/7-10", label='Analysis Variable 2'),
    analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
  ),"Subjects in population level dataset do not have records in observation level dataset, please consider adding a population flag in population_from or reach out to survival extrapolation SME")
  
})

# new input data feature
adsl <- adsl |>
  # Important: convert "" string to NA in character variables otherwise there are not considered missing
  dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))

adtte <- adtte |>
  # Important: convert "" string to NA in character variables otherwise there are not considered missing
  dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))


test_that("preprocess_data therapy_data_var works as expected", {
  
  datad1<-preprocess_data(
    population_from = adsl,
    population_where = quo(ITTFL=="Y" & TRT01P=="B: Placebo"),
    observation_from = adtte,
    observation_where =  PARAMCD=="OS" ,
    analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
    analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable'),
    therapy_data_var=list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL)
  )
  
  expect_equal(attributes(datad1$TRT01P)$label,"Planned Treatment for Period 01")
  
  datad2<-preprocess_data(
    population_from = adsl,
    population_where = quo(ITTFL=="Y" & TRT01P=="B: Placebo"),
    observation_from = adtte,
    observation_where =  PARAMCD=="OS" ,
    analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
    analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable'),
    therapy_data_var=list(name="TRT01A",name_label="Actual Treatment for Period 01",levels=NULL,labels=NULL)
  )
  
  expect_equal(attributes(datad2$TRT01A)$label,"Actual Treatment for Period 01")
  
})