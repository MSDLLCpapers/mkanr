#' Preprocess Data for Analysis
#'
#' Uses input data and parameters to generate pre-processed data. 
#' 
#' For more information on quosures go to https://adv-r.hadley.nz/evaluation.html#quosures 
#'
#' @section Specification:
#' \if{latex}{ This function uses input data and parameters to generate pre-processed
#' data ready for analysis. Function assumes time variable is available in days 
#' in observation_from.
#'
#' Logical Specifications:
#' 
#'   \enumerate{
#'   \item TryCatch to identify x_where condition type
#'   \item Preprocess quosures for x_where conditions
#'   \item Check input parameters using \code{\link{bullet_proof}}
#'   \item Get the data from population_from and filter by population_where.
#'      \enumerate{
#'         \item if population not a data.frame, read in population_from data with read_sas function
#'         \item filter data with population_where condition
#'         \item convert empty character values to missing (NA) values
#'         \item convert therapy variable to factor with information provided in
#'            therapy_data_var
#'         }
#'   \item Get the data from observation_from, derive analysis variable and filter by observation_where 
#'      \enumerate{
#'         \item if observation not a data.frame, read in observation_from data with read_sas function
#'         \item convert empty character values to missing (NA) values
#'         \item derive analysis variable and associated unit variable based on input
#'           analysis_variable and analysis_variable_unit (name and derivation elements)
#'         \item assign analysis variable and associated unit variable labels based on 
#'           input analysis_variable and analysis_variable_unit (label element)
#'        \item filter data with observation_where condition
#'         \item check if participants in population not present in observation 
#'         }
#'   \item Combine population and observation data
#'      \enumerate{
#'         \item left join population data with observation data and keep all variables
#'             except duplicates
#'         }
#'   \item Check result is a non-empty data.frame
#'   \item Return pre-processed data
#'   }
#'
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param population_from path to population SAS data or data.frame
#' @param population_where quosure or logical vector for where clause
#' @param observation_from path to observation SAS data or data.frame
#' @param observation_where quosure or logical vector for where clause
#' @param analysis_variable {`list`}\cr analysis variable derivation metadata with elements:
#' \tabular{lll}{ \code{name} \tab {`character`} \tab  variable name \cr 
#'                 \code{derivation} \tab {`character`} \tab variable derivation \cr 
#'                 \code{label} \tab {`character`} \tab variable label \cr}
#' @param analysis_variable_unit {`list`}\cr analysis variable unit derivation metadata with elements:
#' \tabular{lll}{ \code{name} \tab {`character`} \tab  variable name \cr 
#'                 \code{derivation} \tab {`character`} \tab variable derivation \cr 
#'                 \code{label} \tab {`character`} \tab variable label \cr}
#' @param therapy_data_var {`list`}\cr therapy variable factor metadata with elements:
#' \tabular{lll}{ \code{name} \tab {`character`} \tab  variable name to convert \cr 
#'                 \code{levels} \tab {`character`} \tab preferred order of values taken by variable \cr 
#'                 \code{labels} \tab {`character`} \tab preferred label of values taken by variable \cr
#'                 \code{name_label} \tab {`character`} \tab variable label \cr}
#'                 if levels and labels are set to NULL, sort(unique(name)) will be
#'                 used for both
#'                 see [base::factor()] for details
#'
#' @return A data.frame of class \code{`tibble`} containing pre processed data
#'    ready to be used in modelling functions. The pre-processing includes merging
#'    of population and observation data, filtering, derivation of analysis variables
#'    and creation of factor for treatment variable.
#'    
#' @import stringr
#' @importFrom  rlang is_missing quo_is_null enquo
#' @import dplyr
#' @importFrom haven read_sas
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' #example using path and quosures
#' data1<-preprocess_data(
#' population_from = mkanr::adsl,
#' population_where = quo(ITTFL=="Y" & TRT01P=="B: Placebo"),
#' observation_from = mkanr::adtte,
#' observation_where =  quo(PARAMCD=="OS") ,
#' analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
#' analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit')
#' )
#' 
#' }
preprocess_data <- function(population_from = adsl,
                           population_where = TRUE,
                           observation_from = adtte,
                           observation_where = TRUE,
                           analysis_variable = list(name="AVAL2", derivation="AVAL/7", label='Analysis Variable 2'),
                           analysis_variable_unit = list(name="AVAL2U", derivation="'WEEKS'", label='Analysis Variable 2 Unit'),
                           therapy_data_var=list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL)) {
  
  #tryCatch to identify x_where condition type ----
  pop_error<-tryCatch(
    population_where,
    error = function(e)  {list(e)},
    warning = function(w) {list(w)} 
  )
  
  obs_error<-tryCatch(
    observation_where,
    error = function(e)  {list(e)},
    warning = function(w) {list(w)} 
  )

  # preprocess quosures for x_where conditions ----
  if(is.list(pop_error) && ("error" %in% class(pop_error[[1]]) || "simpleError" %in% class(pop_error[[1]]) ||
                            "warning" %in% class(pop_error[[1]]) || "simpleWarning" %in% class(pop_error[[1]]))){
    enquo_pwhere <- rlang::enquo(population_where)
  }else if (rlang::quo_is_null(rlang::enquo(population_where)) | all(rlang::get_expr(rlang::enquo(population_where))==rlang::expr())) {
    enquo_pwhere <- TRUE
  }else{
    enquo_pwhere<-population_where
  }
  
  if(is.list(obs_error) && ("error" %in% class(obs_error[[1]]) || "simpleError" %in% class(obs_error[[1]]) ||
                            "warning" %in% class(obs_error[[1]]) || "simpleWarning" %in% class(obs_error[[1]]))){
    enquo_owhere <- rlang::enquo(observation_where)
  }else if (rlang::quo_is_null(rlang::enquo(observation_where)) | all(rlang::get_expr(rlang::enquo(observation_where))==rlang::expr())) {
    enquo_owhere <- TRUE
  }else{
    enquo_owhere <- observation_where
  }
    
  # bulletproofing ----
  bullet_proof(population_from = population_from,
               population_where = !!enquo_pwhere,
               observation_from = observation_from,
               observation_where = !!enquo_owhere,
               analysis_variable =analysis_variable,
               analysis_variable_unit = analysis_variable_unit,
               therapy_data_var = therapy_data_var$name)
  
  # population ----
  if (!is.data.frame(population_from )){
    pop <- haven::read_sas(population_from) |>
      # Important: convert "" string to NA in character variables otherwise there are not considered missing
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))
  }else{
    pop<-population_from |>
      # Important: convert "" string to NA in character variables otherwise there are not considered missing
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))
  }
  
  if(rlang::is_quosure(pop_error) | (is.list(pop_error) && ("error" %in% class(pop_error[[1]]) || "simpleError" %in% class(pop_error[[1]]) ||
                            "warning" %in% class(pop_error[[1]]) || "simpleWarning" %in% class(pop_error[[1]])))){
    pop <- pop |>
      dplyr::filter(!!enquo_pwhere)
  }else{
    pop <- subset(pop,enquo_pwhere)
  }

  #Convert treatment to factor
  if (!is.null(therapy_data_var$levels) && !is.null(therapy_data_var$labels)) {
    pop[[therapy_data_var$name]] <- factor(pop[[therapy_data_var$name]], levels = therapy_data_var$levels, labels = therapy_data_var$labels)
  } else {
    pop[[therapy_data_var$name]] <- factor(pop[[therapy_data_var$name]], levels = sort(unique(pop[[therapy_data_var$name]])), labels = sort(unique(pop[[therapy_data_var$name]])))
  }
  

  # observation -----
  if (!is.data.frame(observation_from )){
    adtemp <- haven::read_sas(observation_from) |>
      # Important: convert "" string to NA in character variables otherwise there are not considered missing
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))
  }else{
    adtemp<-observation_from |>
      # Important: convert "" string to NA in character variables otherwise there are not considered missing
      dplyr::mutate_if(is.character, list(~ dplyr::na_if(., "")))
  }
  
  #Derive analysis variables
  adtemp[analysis_variable$name]<- with(adtemp, eval(parse(text = analysis_variable$derivation)))
  adtemp[analysis_variable_unit$name]<- with(adtemp, eval(parse(text = analysis_variable_unit$derivation)))
  
  #assign label 
  attributes(adtemp[[analysis_variable$name]])$label<-analysis_variable$label
  attributes(adtemp[[analysis_variable_unit$name]])$label<-analysis_variable_unit$label
  
  #filter observation
  if(rlang::is_quosure(pop_error) | (is.list(obs_error) && ("error" %in% class(obs_error[[1]]) || "simpleError" %in% class(obs_error[[1]]) ||
                            "warning" %in% class(obs_error[[1]]) || "simpleWarning" %in% class(obs_error[[1]])))){
    adtemp <-adtemp |>
      # Filter according to observation_where
      dplyr::filter(!!enquo_owhere)
  }else{
    adtemp <- subset(adtemp,enquo_owhere)
  }

  # Check if participants in population not present in observation
  if (!all(pop$USUBJID %in% adtemp$USUBJID)) {
    stop("Subjects in population level dataset do not have records in observation level dataset, please consider adding a population flag in population_from or reach out to survival extrapolation SME")
  }
  
  preprocessed_data <- pop |>
    dplyr::left_join(adtemp, by = c("USUBJID"),multiple = "all") |>
    # Remove duplicates variables created by left_join
    dplyr::select(!dplyr::ends_with(".y")) |>
    # Remove .x suffix from variables created by lef_join
    dplyr::rename_with(~ stringr::str_remove(., stringr::regex("\\.x$")), dplyr::ends_with(".x"))
  
  # empty dataset check
  if (nrow(preprocessed_data) == 0) stop("Combination of input parameters result in an empty dataset")

  #assign treatment label 
  attributes(preprocessed_data[[therapy_data_var$name]])$label<-therapy_data_var$name_label
  
  # return data -----
  preprocessed_data
}
