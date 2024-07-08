#' Centralized bullet proofing
#'
#' Checks the validity of input parameters
#'
#' @section Specification:
#' \if{latex}{This function checks the validity of the parameters only for the
#' below given list. Input of any other parameters will not be checked.
#'   \enumerate{
#'   \item population_from
#'   \enumerate{
#'         \item should be type character and length 1
#'         \item file should exist (see file.exists())
#'         \item data should have required variables: USUBJID
#'         \item data should not be empty (load with haven::read_sas() or data())
#'         \item data unique key should be ('USUBJID')
#'         }
#'   \item observation_from
#'   \enumerate{
#'         \item should be type character and length 1
#'         \item file should exist (see file.exists())
#'         \item data should have required variables: USUBJID, CNSR, PARAMCD
#'         \item data should not be empty (load with haven::read_sas() or data())
#'         \item data unique key should be ('USUBJID', 'PARAMCD')
#'         }
#'   \item population_where
#'   \enumerate{
#'         \item population_from should also be provided
#'         \item check variable(s) used are available in data
#'         \item filtered data should not be empty
#'         }
#'   \item observation_where
#'   \enumerate{
#'         \item observation_from should also be provided
#'         \item check variable(s) used are available in data
#'         \item filtered data should not be empty
#'         }
#'   \item therapy_data_var
#'   \enumerate{
#'         \item should be type character and length 1
#'         \item if population_from provided, should exist in population_from
#'         \item if population_from provided, numeric equivalent (nameN) should exist in population_from
#'         }
#'   \item therapy_data_var_list
#'   \enumerate{
#'         \item should be list with elements: name, levels and labels
#'         \item name value should be type character and length 1
#'         \item levels values should be type character or numeric and length >=1
#'         \item labels values should be type character and length >=1
#'         \item levels and labels values should be the same length
#'         \item levels values should exist in population_from$name values
#'         }
#'   \item rename_output
#'   \enumerate{
#'         \item should be type character and length 1
#'         \item should only have lowercase letters and numbers
#'         }
#'   \item analysis_variable
#'   \enumerate{
#'         \item should be list with elements: name, derivation and label
#'         \item all values should be type character and length 1
#'         \item observation_from should be provided
#'         \item variables used in derivation should be available in data
#'         }
#'   \item analysis_variable_unit
#'   \enumerate{
#'         \item should be list with elements: name, derivation and label
#'         \item all values should be type character and length 1
#'         }
#'   \item preprocessed_data / preprocessed_data_km
#'   \enumerate{
#'         \item should be of type data.frame
#'         }
#'   \item surv_formula
#'   \enumerate{
#'         \item requires preprocessed_data or preprocessed_data_km to run tests
#'         \item should be class formula
#'         \item should have survival::Surv prefix used in left hand side
#'         \item variables used in right hand side should exist in data
#'         \item extract endpoint for subsequent tests: surv_formula[[2]][[2]]
#'         }
#'   \item outdata_path / outgraph_path /outtable_path
#'   \enumerate{
#'         \item should be of type character and length 1
#'         \item directory should exist (see dir.exists())
#'         }
#'   \item fits_flexsurv
#'   \enumerate{
#'         \item should be of type list and length >=1
#'         \item all elements should be named and class flexsurvreg
#'         }
#'   \item fits_survreg
#'   \enumerate{
#'         \item should be of type list and length >=1
#'         \item all elements should be named and class survreg
#'         }
#'   \item fits
#'   \enumerate{
#'         \item should be of type list and length >=1
#'         \item all elements should be named and class survreg or flexsurvreg
#'         }
#'   \item break_point
#'   \enumerate{
#'         \item should be type numeric, length 1 and greater or equal to 0
#'         \item if preprocessed_data_km, preprocessed_data and surv_formula provided
#'          \enumerate{
#'          \item endpoint extracted from surv_formula should exist
#'          \item endpoint should exist in both preprocessed_data and preprocessed_data_km
#'          \item break_point should be equal to max(preprocessed_data_km$endpoint) - max(preprocessed_data$endpoint)
#'         }
#'         }
#'   \item id_analysis
#'   \enumerate{
#'         \item should be type character and length greater or equal to 1
#'         \item should be subset of c('e0stdflex0gof','e0stdflex0fig0timeshort','e0stdflex0fig0timelong')
#'         }
#'  \item xlab / ylab /  col_just /  header_label /  levelvar /  levelvar_legend /  models_title /  palette /  rtf_footnote /  rtf_title /  rtf_title_pop /  rtf_footnote_pop /  therapy_arm /  treatment_label /  time_horizon /  xvar /  yvar
#'  \enumerate{
#'        \item should be type character and length greater or equal to 1
#'        }
#' \item facetvar
#' \enumerate{
#'        \item should be either null or type character and length greater or equal to 1
#'        }
#' \item fig_height  / fig_line_size /  fig_res /  fig_scale /  fig_width /  digits /  legend_textsize /  rel_col_widths /  seed_value /  predict_time
#' \enumerate{
#'        \item should be type numeric and length greater or equal to 1
#'        }
#' \item time_unit
#' \enumerate{
#'        \item should be type character and length greater or equal to 1 
#'        \item should be equal to 'd' for analysis in days, 'w' for analysis in weeks, 'm' for analysis in months or 'y' for analysis in years
#'        }
#' \item orientation
#' \enumerate{
#'        \item should be type character and length greater or equal to 1 
#'        \item should be either 'portrait' or 'landscape'
#'        }
#' \item legend_position
#' \enumerate{
#'        \item should be type character and length greater or equal to 1 
#'        \item should be either 'none', 'left', 'right', 'bottom' or 'top'
#'        }
#' \item gof_arrange
#' \enumerate{
#'        \item should be either null or type character and length greater or equal to 1 
#'        \item should be either 'rank aic', 'rank bic' or 'rank mean aic bic'
#'        }
#' \item custom_model_label /  csv_output /  ci /  se /  exp /  boundknots /  fig_grid_x /  fig_grid_y
#' \enumerate{
#'       \item should be equal to either true or false
#'        }
#' \item output_suffix_update / rtf_param /  scalexk_list
#' \enumerate{
#'        \item should be either a list or null
#'        }
#' \item modified_value / output_suffix
#' \enumerate{
#'       \item should be a list
#'        }
#' \item data
#' \enumerate{
#'        \item should be a data frame
#'       }
#' \item facetvar_legend
#' \enumerate{
#'        \item should be of class function (ggplot2::labeller) and length 1
#'        }
#' }
#'
#' Logical Specifications:
#'
#' \enumerate{
#'   \item Check input parameters have check available
#'   \item Run the first loop on the list of parameters especially on
#'   population_from and observation_from to check and capture the symbols and
#'   column names
#'   \item Check other expression element logic based on the name of the element
#'   \item Return message if all check were successful
#' }
#' }
#'
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#'
#' @param ... any test arguments to be bullet proofed
#'
#' @return NA
#'
#' @import dplyr
#' @import rlang
#' @import stringr
#' @importFrom haven read_sas
#' @importFrom purrr pmap set_names
#'
bullet_proof <- function(...) {
  #################################################################
  # 1.  ---- check input parameters have check available ----
  #################################################################
  
  dots <- rlang::enquos(...)
  
  checks_available_for <- toupper(
    c(
      'population_from',
      'population_where',
      'observation_from',
      'observation_where',
      'therapy_data_var',
      'therapy_data_var_list',
      'preprocessed_data',
      'preprocessed_data_km',
      'rename_output',
      'surv_formula',
      'analysis_variable',
      'analysis_variable_unit',
      'outdata_path',
      'outgraph_path',
      'outtable_path',
      'fits_flexsurv',
      'fits_survreg',
      'fits',
      'break_point',
      'id_analysis',
      'rtf_title_pop',
      'rtf_footnote_pop',
      'rtf_prefix',
      'subpop_folder_name',
      'param',
      'time',
      'time_unit',
      'treatment_label',
      'predict_time',
      'parametric_list',
      'spline_list',
      'custom_model_label',
      'boundknots',
      'col_just',
      'csv_output',
      'data',
      'digits',
      'dist_list',
      'exp',
      'facetvar',
      'facetvar_legend',
      'fig_grid_x',
      'fig_grid_y',
      'fig_height',
      'fig_line_size',
      'fig_res',
      'fig_scale',
      'fig_width',
      'gof_arrange',
      'header_label',
      'legend_position',
      'legend_textsize',
      'levelvar',
      'levelvar_legend',
      'models_title',
      'modified_value',
      'orientation',
      'output_suffix',
      'output_suffix_update',
      'palette',
      'rel_col_widths',
      'rtf_footnote',
      'rtf_param',
      'rtf_title',
      'scalexk_list',
      'seed_value',
      'therapy_arm',
      'time_horizon',
      'xlab',
      'xvar',
      'ylab',
      'yvar',
      'ci',
      'se'
    )
  )
  
  request_ids = c('e0stdflex0gof',
                  'e0stdflex0fig0timeshort',
                  'e0stdflex0fig0timelong')
  
  lapply(names(dots), function(x)
    if (!(toupper(x) %in% checks_available_for))
      warning("Bulletproofing: no check available for: ", x))
  
  #################################################################
  # 2.  ---- Run the second loop on the list of parameters
  #          especially on population_from and observation_from
  #          to check and capture the symbols and column names ----
  #################################################################
  
  temp_data <-
    purrr::pmap(list(name = names(dots), dot = rlang::get_expr(dots)),
                function(name, dot) {
                  if (toupper(name) %in% c("POPULATION_FROM", "OBSERVATION_FROM")) {
                    if (!is.data.frame(rlang::eval_tidy(dot))) {
                      if (!(is.character(rlang::eval_tidy(dot)) &&
                            length(rlang::eval_tidy(dot)) == 1))
                        stop(
                          "Bulletproofing: ",
                          rlang::as_label(dot),
                          " (",
                          name,
                          ") should be of type character and length 1"
                        )
                      
                      if (!stringr::str_detect(rlang::eval_tidy(dot),
                                               stringr::regex("\\.sas7bdat$")))
                        stop(
                          "Bulletproofing: ",
                          rlang::as_label(dot),
                          " (",
                          name,
                          ") should have the suffix .sas7bdat"
                        )
                      
                      if (!file.exists(rlang::eval_tidy(dot)))
                        stop("Bulletproofing: ",
                             rlang::as_label(dot),
                             " (",
                             name,
                             ") does not exist")
                      
                      # Capture the symbol and column names
                      haven::read_sas(rlang::eval_tidy(dot)) %>%
                        # Important: convert "" string to NA in character variables otherwise there are not considered missing
                        dplyr::mutate_if(is.character, list( ~ dplyr::na_if(., "")))
                    } else{
                      rlang::eval_tidy(dot)
                    }
                    
                  }
                }) %>% purrr::set_names(names(dots))
  
  if ("population_from" %in% names(temp_data)) {
    pop <- temp_data[["population_from"]]
    pop_names <- colnames(pop)
    
    # USUBJID check
    if (!("USUBJID" %in% pop_names))
      stop("USUBJID should exist in population_from data")
    # empty data check
    if (nrow(pop) == 0)
      stop("population_form is an empty dataset")
    
    # Unique key check
    if (!("population_where" %in% names(temp_data))) {
      if (!all(
        pop %>% dplyr::count(USUBJID) %>% dplyr::count(n, name = "count") %>% dplyr::pull(n) == 1
      ))
        stop("Bulletproofing: population_from unique key should be USUBJID")
    }
  }
  if ("observation_from" %in% names(temp_data)) {
    obs <- temp_data[["observation_from"]]
    obs_names <- colnames(obs)
    
    # USUBJID check
    if (!("USUBJID" %in% obs_names))
      stop("USUBJID should exist in observation_from data")
    # CNSR check
    if (!("CNSR" %in% obs_names))
      stop("CNSR should exist in observation_from data")
    # PARAMCD check
    if (!("PARAMCD" %in% obs_names))
      stop("PARAMCD should exist in observation_from data")
    # empty table check
    if (nrow(obs) == 0)
      stop("observation_from is an empty dataset")
    
    # Unique key check
    if (!("observation_where" %in% names(temp_data))) {
      if (!all(
        obs %>% dplyr::count(USUBJID, PARAMCD) %>% dplyr::count(n, name = "count") %>% dplyr::pull(n) == 1
      ))
        stop("Bulletproofing: observation_from unique key should be USUBJID, PARAMCD")
    }
  }
  
  ### --- preprocessed_data check --- ###
  tte <- purrr::pmap(list(name = names(dots), dot = dots),
                     function(name, dot) {
                       if (toupper(name) %in% c("PREPROCESSED_DATA", "PREPROCESSED_DATA_KM")) {
                         if (!is.data.frame(rlang::eval_tidy(dot)))
                           stop(
                             "Bulletproofing: ",
                             rlang::as_label(dot),
                             " (",
                             name,
                             ") should be of type data.frame"
                           )
                         
                         rlang::eval_tidy(dot)
                       }
                     }) %>% purrr::set_names(names(dots))
  
  ####################################################
  # 2.  ---- Check other expression element logic
  #          based on the name of the element ----
  ####################################################
  
  # Analysis variable check
  purrr::pmap(list(name = names(dots), dot = rlang::get_expr(dots)),
              function(name, dot) {
                ### --- analysis_variable --- ###
                if (toupper(name) %in% c("ANALYSIS_VARIABLE", "ANALYSIS_VARIABLE_UNIT")) {
                  if (!(
                    is.list(rlang::eval_tidy(dot)) &&
                    length(rlang::eval_tidy(dot)) == 3 &&
                    all(
                      c("name", "derivation", "label") %in% names(rlang::eval_tidy(dot))
                    )
                  ))
                    stop(
                      "Bulletproofing: ",
                      name,
                      " should be of type list and have the following 3 elements: name, derivation, label"
                    )
                  
                  lapply(rlang::eval_tidy(dot),
                         function(x) {
                           if (!(is.character(x) &&
                                 length(x) == 1))
                             stop(
                               "Bulletproofing: elements of list provided in ",
                               name,
                               " should be of type character and length equal to 1"
                             )
                         })
                  
                  if (toupper(name) %in% c("ANALYSIS_VARIABLE")) {
                    if (!exists("obs_names")) {
                      stop(
                        "Bulletproofing: ",
                        name,
                        " check for derivation element could not be performed ",
                        "without observation_from parameter"
                      )
                    }
                    
                    vars <-
                      unlist(stringr::str_extract(
                        unlist(
                          stringr::str_extract_all(rlang::eval_tidy(dot)[["derivation"]],
                                                   stringr::boundary("word"))
                        ),
                        "^[A-Z].+[A-Z0-9]$"
                      ))
                    
                    # if extracted values are not in the names of population_from data
                    lapply(vars[!is.na(vars)],
                           function(x) {
                             if (!(x %in%  eval(as.symbol("obs_names"))))
                               stop(
                                 'Bulletproofing: variable ',
                                 x,
                                 ' used in ',
                                 name,
                                 '$derivation is not available in observation_from data'
                               )
                           })
                    
                  }
                  
                  if ("OBSERVATION_FROM" %in% toupper(names(dots))) {
                    obs[rlang::eval_tidy(dot)[["name"]]] = with(obs, eval(parse(text = rlang::eval_tidy(dot)[["derivation"]])))
                    obs <<- obs
                    obs_names <<- colnames(obs)
                  }
                }
              })
  
  #Consider population_where and observation_where
  temp_data_where <- purrr::pmap(list(name = names(dots), dot = dots),
                                 function(name, dot) {
                                   ### --- population_where & observation_where --- ###
                                   if (toupper(name) %in% c("POPULATION_WHERE", "OBSERVATION_WHERE")) {
                                     prefix_chr <- substr(name, 1, 3)
                                     selection <-
                                       strsplit(name, split = '_', fixed = TRUE)[[1]][[1]]
                                     
                                     if (!exists(paste0(prefix_chr, "_names"))) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " check could not be performed ",
                                         "without ",
                                         selection,
                                         "_from parameter"
                                       )
                                     }
                                     
                                     if ("simpleError" %in% class(tryCatch(
                                       rlang::eval_tidy(dot),
                                       error = function(e)  {
                                         list(e)
                                       },
                                       warning = function(w) {
                                         list(w)
                                       }
                                     )[[1]])) {
                                       # Convert the expression to a string
                                       str <-
                                         paste0(rlang::quo_name(dot), collapse = '')
                                       vars <-
                                         stringr::str_remove_all(# unlist to create a vector
                                           unlist(
                                             stringr::str_extract_all(
                                               # Remove the character strings, double or single quotes (variable-
                                               # values), R reserved values - TRUE, FALSE, NA & NULL
                                               stringr::str_remove_all(string = str, pattern =
                                                                         "([\"\']{1}[^\"\']*[\"\']{1})|\bTRUE\b|\bFALSE\b|\bNA\b|\bNULL\b"),
                                               # Extract the variable names with num, char, period or underscore
                                               # or subset variables from data, ex: adsl$USUBJID
                                               "([^\\W0-9.]|([\\.][^\\d])|(\\w|\\.)*[$])(\\w|\\.)*[\\s=,)\\[\\]]"
                                             )
                                           ),
                                           # Pattern to remove the special characters from variable names
                                           # other than the below characters
                                           pattern = "[^\\w\\d\\.\\$]")
                                       
                                       # if extracted values are not in the names of population_from data
                                       lapply(vars,
                                              function(x) {
                                                if (!(x %in%   eval(parse(text = paste0(
                                                  prefix_chr, "_names"
                                                )))))
                                                  stop(
                                                    'Bulletproofing: variable ',
                                                    x,
                                                    ' used in ',
                                                    name,
                                                    ' is not available in ',
                                                    selection,
                                                    "_from"
                                                  )
                                              })
                                       
                                       if (rlang::get_expr(dot) != rlang::expr()) {
                                         eval(parse(text = prefix_chr)) %>%
                                           dplyr::filter(!!enquo(dot))
                                       } else{
                                         eval(parse(text = prefix_chr))
                                       }
                                     } else if (all(is.logical(rlang::eval_tidy(dot)))) {
                                       subset(eval(parse(text = prefix_chr)), rlang::eval_tidy(dot))
                                     } else{
                                       stop("Bulletproofing: ",
                                            name,
                                            " check could not be performed. Input type not handled")
                                     }
                                     
                                   }
                                   
                                 }) %>% purrr::set_names(names(dots))
  
  if ("population_where" %in% names(temp_data_where)) {
    pop <- temp_data_where[["population_where"]]
    pop_names <- colnames(pop)
    if (nrow(pop) == 0)
      stop(
        "Bulletproofing: combination of population_from and population_where result in an empty dataset"
      )
    # Unique key check
    if (!all(
      pop %>% dplyr::count(USUBJID) %>% dplyr::count(n, name = "count") %>% dplyr::pull(n) == 1
    ))
      stop(
        "Bulletproofing: combination of population_from and population_where unique key should be USUBJID"
      )
  }
  if ("observation_where" %in% names(temp_data_where)) {
    obs <- temp_data_where[["observation_where"]]
    obs_names <- colnames(obs)
    if (nrow(obs) == 0)
      stop(
        "Bulletproofing: combination of observation_from and observation_where result in an empty dataset"
      )
    # Unique key check
    if (!all(
      obs %>% dplyr::count(USUBJID, PARAMCD) %>% dplyr::count(n, name = "count") %>% dplyr::pull(n) == 1
    ))
      stop(
        "Bulletproofing: combination of observation_from and observation_where unique key should be USUBJID, PARAMCD"
      )
  }
  
  ### --- formula check --- ###
  endpoint_formula <- purrr::pmap(list(name = names(dots), dot = dots),
                                  function(name, dot) {
                                    if (toupper(name) == "SURV_FORMULA") {
                                      if (!(any("formula" %in% class(rlang::eval_tidy(dot)) )))
                                        stop(
                                          "Bulletproofing: ",
                                          rlang::as_label(dot),
                                          " (",
                                          name,
                                          ") should be of class formula"
                                        )
                                      
                                      if (!(rlang::eval_tidy(dot)[[2]][[1]] ==
                                            "survival::Surv"))
                                        stop("Bulletproofing: survival::Surv should be used in formula")
                                      
                                      if ("PREPROCESSED_DATA" %in% toupper(names(rlang::exprs(...)))) {
                                        test_error <- tryCatch(
                                          model.frame(rlang::eval_tidy(dot), data = tte[["preprocessed_data"]]),
                                          error = function(e)  {
                                            list(e)
                                          },
                                          warning = function(w) {
                                            list(w)
                                          }
                                        )
                                        
                                        if (is.list(test_error) &&
                                            (
                                              "error" %in% class(test_error[[1]]) ||
                                              "simpleError" %in% class(test_error[[1]]) ||
                                              "warning" %in% class(test_error[[1]]) ||
                                              "simpleWarning" %in% class(test_error[[1]])
                                            ))
                                          stop(
                                            "Bulletproofing: ",
                                            rlang::as_label(dot),
                                            " (",
                                            name,
                                            ") can not be used with preprocessed_tte due to error/warning: ",
                                            test_error[[1]]$message
                                          )
                                        
                                        model_frame_temp <-
                                          model.frame(rlang::eval_tidy(dot), data = tte[["preprocessed_data"]])
                                        
                                        lapply(model_frame_temp[, -1], function(x) {
                                          if (!is.factor(x))
                                            stop(
                                              "Bulletproofing: only factor are supported in right hand side of model survival"
                                            )
                                        })
                                        
                                      } else if ("PREPROCESSED_DATA_KM" %in% toupper(names(rlang::exprs(...)))) {
                                        test_error_km <- tryCatch(
                                          model.frame(rlang::eval_tidy(dot), data = tte[["preprocessed_data_km"]]),
                                          error = function(e)  {
                                            list(e)
                                          },
                                          warning = function(w) {
                                            list(w)
                                          }
                                        )
                                        
                                        if (is.list(test_error_km) &&
                                            (
                                              "error" %in% class(test_error_km[[1]]) ||
                                              "simpleError" %in% class(test_error_km[[1]]) ||
                                              "warning" %in% class(test_error_km[[1]]) ||
                                              "simpleWarning" %in% class(test_error_km[[1]])
                                            ))
                                          stop(
                                            "Bulletproofing: ",
                                            rlang::as_label(dot),
                                            " (",
                                            name,
                                            ") can not be used with preprocessed_tte_km due to error/warning: ",
                                            test_error_km[[1]]$message
                                          )
                                        
                                        model_frame_temp <-
                                          model.frame(rlang::eval_tidy(dot), data = tte[["preprocessed_data_km"]])
                                        
                                      } else{
                                        stop(
                                          "Bulletproofing: ",
                                          name,
                                          " requires preprocessed_data or preprocessed_data_km to be checked"
                                        )
                                      }
                                      
                                      rlang::eval_tidy(dot)[[2]][[2]]
                                    }
                                    
                                  }) %>% purrr::set_names(names(dots))
  
  #Perform other checks
  empty_no_return <- purrr::pmap(list(name = names(dots), dot = dots),
                                 function(name, dot) {
                                   ### --- therapy_data_var name --- ###
                                   if (toupper(name) %in% c("THERAPY_DATA_VAR")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) == 1))
                                       stop("Bulletproofing: therapy_data_var name should be of type character and length 1")
                                     
                                     if (exists(quote(pop_names))) {
                                       # Check therapy_data_var exists in population_from data
                                       if (!(rlang::eval_tidy(dot) %in% pop_names))
                                         stop(
                                           "Bulletproofing: ",
                                           rlang::eval_tidy(dot),
                                           " (therapy_data_var name) should exist in population_from data"
                                         )
                                       
                                     }
                                     
                                   }
                                   
                                   ### --- therapy_data_var list --- ###
                                   if (toupper(name) %in% c("THERAPY_DATA_VAR_LIST")) {
                                     if (!(
                                       is.list(rlang::eval_tidy(dot)) &&
                                       length(rlang::eval_tidy(dot)) == 4 &&
                                       all(
                                         c("name", "levels", "labels", "name_label") %in% names(rlang::eval_tidy(dot))
                                       )
                                     ))
                                       stop(
                                         "Bulletproofing: therapy_data_var list should be of type list and have the following 4 elements: name, levels, labels, name_label"
                                       )
                                     
                                     purrr::pmap(list(
                                       x = rlang::eval_tidy(dot),
                                       y = names(rlang::eval_tidy(dot))
                                     ),
                                     function(x, y) {
                                       if (toupper(y) == "NAME" && !(is.character(x) &&
                                                                     length(x) == 1)) {
                                         stop(
                                           "Bulletproofing: element ",
                                           y,
                                           " of therapy_data_var list should be of type character and length equal to 1"
                                         )
                                       } else if (toupper(y) ==
                                                  "LEVELS" &&
                                                  !is.null(x) &&
                                                  !((is.character(x) | is.numeric(x)) &&  length(x) >= 1)) {
                                         stop(
                                           "Bulletproofing: element ",
                                           y,
                                           " of therapy_data_var list should be of type character or numeric and length greater or equal to 1"
                                         )
                                       } else if (toupper(y) ==
                                                  "LABELS" &&
                                                  !is.null(x) &&
                                                  !((is.character(x) | is.numeric(x)) &&  length(x) >= 1)) {
                                         stop(
                                           "Bulletproofing: element ",
                                           y,
                                           " of therapy_data_var list should be of type character and length greater or equal to 1"
                                         )
                                       }
                                     })
                                     
                                     therapy_data_var_temp = rlang::eval_tidy(dot)
                                     
                                     if (!is.null(therapy_data_var_temp$levels) &&
                                         !is.null(therapy_data_var_temp$labels) &&
                                         !identical(
                                           length(therapy_data_var_temp$levels),
                                           length(therapy_data_var_temp$labels)
                                         ))
                                       stop(
                                         "Bulletproofing: therapy_data_var$levels and therapy_data_var$labels should be the same length"
                                       )
                                     
                                     if (!exists("pop_names")) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " check could not be performed ",
                                         "without population_from parameter"
                                       )
                                     }
                                     
                                     if (!identical(therapy_data_var_temp$levels %in% unique(pop[[therapy_data_var_temp$name]]),
                                                    rep(TRUE, length(therapy_data_var_temp$levels))))
                                       stop(
                                         "Bulletproofing: all therapy_data_var$levels should exist in values of ",
                                         therapy_data_var_temp$name,
                                         " in population_from data"
                                       )
                                   }
                                   
                                   ### --- time name --- ###
                                   if (toupper(name) %in% c("TIME")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) == 1))
                                       stop("Bulletproofing: time name should be of type character and length 1")
                                     
                                     if (exists(quote(obs_names))) {
                                       # Check time exists in observation_from data
                                       if (!(rlang::eval_tidy(dot) %in% obs_names))
                                         stop(
                                           "Bulletproofing: ",
                                           rlang::eval_tidy(dot),
                                           " (time) should exist in observation_from data"
                                         )
                                     }
                                     
                                   }
                                   
                                   ### --- rename_output check --- ###
                                   if (toupper(name) == "RENAME_OUTPUT") {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) == 1)) {
                                       stop("Bulletproofing: rename_output should be of type character and length equal to 1")
                                     }
                                     if (stringr::str_detect(rlang::eval_tidy(dot),
                                                             "[[:upper:]]|[:punct:]|[:space:]")) {
                                       stop("Bulletproofing: rename_output should only have lowercase letters and numbers")
                                     }
                                   }
                                   
                                   ### --- path check --- ###
                                   if (toupper(name) %in% c("OUTDATA_PATH", "OUTTABLE_PATH", "OUTGRAPH_PATH")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) == 1))
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::eval_tidy(dot),
                                         " (",
                                         name,
                                         ") should be of type character and length 1"
                                       )
                                     
                                     if (!dir.exists(rlang::eval_tidy(dot))) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::eval_tidy(dot),
                                         " (",
                                         name,
                                         ") directory does not exist, please enter valid path"
                                       )
                                     }
                                   }
                                   
                                   
                                   ### --- flexsurv_fit check --- ###
                                   if (toupper(name) == "FITS_FLEXSURV") {
                                     if (!(is.list(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") should be a list of length greater of equal to 1"
                                       )
                                     }
                                     if (!(sum(names(rlang::eval_tidy(dot)) !=
                                               "") == length(rlang::eval_tidy(dot)))) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") elements should all be named"
                                       )
                                     }
                                     
                                     empty <-
                                       purrr::pmap(list(
                                         x = rlang::eval_tidy(dot),
                                         y = names(rlang::eval_tidy(dot))
                                       ),
                                       function(x, y) {
                                         if (!(any(class(x) == "flexsurvreg"))) {
                                           stop("Bulletproofing: element ",
                                                y,
                                                " of fits list should be of class flexsurvreg")
                                         }
                                       })
                                   }
                                   
                                   ### --- survreg_fit check --- ###
                                   if (toupper(name) == "FITS_SURVREG") {
                                     if (!(is.list(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") should be a list of length greater of equal to 1"
                                       )
                                     }
                                     if (!(sum(names(rlang::eval_tidy(dot)) !=
                                               "") == length(rlang::eval_tidy(dot)))) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") elements should all be named"
                                       )
                                     }
                                     
                                     empty <-
                                       purrr::pmap(list(
                                         x = rlang::eval_tidy(dot),
                                         y = names(rlang::eval_tidy(dot))
                                       ),
                                       function(x, y) {
                                         if (!(any(class(x) == "survreg"))) {
                                           stop("Bulletproofing: element ",
                                                y,
                                                " of fits list should be of class survreg")
                                         }
                                       })
                                   }
                                   
                                   ### --- fits check --- ###
                                   if (toupper(name) == "FITS") {
                                     if (!(is.list(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") should be a list of length greater of equal to 1"
                                       )
                                     }
                                     if (!(sum(names(rlang::eval_tidy(dot)) !=
                                               "") == length(rlang::eval_tidy(dot)))) {
                                       stop(
                                         "Bulletproofing: ",
                                         rlang::as_label(dot),
                                         " (",
                                         name,
                                         ") elements should all be named"
                                       )
                                     }
                                     
                                     empty <-
                                       purrr::pmap(list(
                                         x = rlang::eval_tidy(dot),
                                         y = names(rlang::eval_tidy(dot))
                                       ),
                                       function(x, y) {
                                         if (!(any(class(x) == "survreg" | class(x) == "flexsurvreg"))) {
                                           stop(
                                             "Bulletproofing: element ",
                                             y,
                                             " of fits list should be of class survreg or flexsurvreg"
                                           )
                                         }
                                       })
                                     
                                   }
                                   
                                   ### --- break_point check --- ###
                                   if (toupper(name) == "BREAK_POINT") {
                                     if (!(
                                       is.null(rlang::eval_tidy(dot)) |
                                       is.numeric(rlang::eval_tidy(dot)) &&
                                       length(rlang::eval_tidy(dot)) == 1
                                     )) {
                                       stop(
                                         "Bulletproofing: break_point should be either NULL or type numeric and length equal to 1"
                                       )
                                     }
                                     if (!(is.null(rlang::eval_tidy(dot)))) {
                                       if (!(rlang::eval_tidy(dot) >= 0)) {
                                         stop("Bulletproofing: break_point should be greater or equal to 0")
                                       }
                                     }
                                     
                                     if (exists("tte") &&
                                         exists("endpoint_formula") &&
                                         is.data.frame(tte[["preprocessed_data"]]) &&
                                         is.data.frame(tte[["preprocessed_data_km"]]) &&
                                         is.symbol(endpoint_formula[["surv_formula"]])) {
                                       if ((max(tte[["preprocessed_data_km"]][endpoint_formula[["surv_formula"]]]) -
                                            max(tte[["preprocessed_data"]][endpoint_formula[["surv_formula"]]])) != rlang::eval_tidy(dot))
                                         stop(
                                           "Bulletproofing: break_point should be equal to ",
                                           max(tte[["preprocessed_data_km"]][endpoint_formula[["surv_formula"]]]) -
                                             max(tte[["preprocessed_data"]][endpoint_formula[["surv_formula"]]]),
                                           " (max(preprocessed_data_km$endpoint) - max(preprocessed_data$endpoint))"
                                         )
                                     }
                                     
                                   }
                                   
                                   
                                   ### --- id_analysis --- ###
                                   if (toupper(name) %in% c("ID_ANALYSIS")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be type character and length greater or equal to 1"
                                       )
                                     }
                                     if (!identical((rlang::eval_tidy(dot) %in% request_ids),
                                                    rep(TRUE, length(rlang::eval_tidy(dot))))) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be a subset of ",
                                         glue::glue_collapse(request_ids, sep = ", ")
                                       )
                                     }
                                     
                                   }
                                   
                                   ### --- spline_list/parametric_list/dist_list check --- ###
                                   if (toupper(name) %in% c("PARAMETRIC_LIST", "DIST_LIST")) {
                                     if (!is.list(rlang::eval_tidy(dot)) ||
                                         !(identical(rlang::eval_tidy(dot), list()) ||
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be type list and either equal to list() or of length greater or equal to 1"
                                       )
                                     }
                                     if (any(names(rlang::eval_tidy(dot)) ==
                                             "")) {
                                       stop("Bulletproofing: ", name, " should be a named list")
                                     }
                                     
                                     purrr::pmap(list(name2 = names(dots), dot2 =
                                                        dots),
                                                 function(name2, dot2) {
                                                   if (toupper(name2) %in% c("SPLINE_LIST")) {
                                                     if (!is.list(rlang::eval_tidy(dot2)) ||
                                                         !(identical(rlang::eval_tidy(dot2), list()) ||
                                                           length(rlang::eval_tidy(dot2)) >= 1)) {
                                                       stop(
                                                         "Bulletproofing: ",
                                                         name2,
                                                         " should be type list and either equal to list() or of length greater or equal to 1"
                                                       )
                                                     }
                                                     if (any(names(rlang::eval_tidy(dot2)) ==
                                                             "")) {
                                                       stop("Bulletproofing: ", name2, " should be a named list")
                                                     }
                                                     
                                                     if (identical(append(rlang::eval_tidy(dot2), rlang::eval_tidy(dot)),
                                                                   list())) {
                                                       stop(
                                                         "Bulletproofing: at least one element should exist in either parametric_list or spline_list"
                                                       )
                                                     }
                                                     
                                                   }
                                                 })
                                     
                                   }
                                   
                                   
                                   ### --- Output formatting related check --- ###
                                   #### --- should be type character and length greater or equal to 1 --- ####
                                   if (toupper(name) %in% c(
                                     "PARAM",
                                     "RTF_PREFIX",
                                     "XLAB",
                                     "YLAB",
                                     "COL_JUST",
                                     "HEADER_LABEL",
                                     "LEVELVAR",
                                     "LEVELVAR_LEGEND",
                                     "MODELS_TITLE",
                                     "PALETTE",
                                     "RTF_FOOTNOTE",
                                     "RTF_TITLE",
                                     "RTF_TITLE_POP",
                                     "RTF_FOOTNOTE_POP",
                                     "THERAPY_ARM",
                                     "TREATMENT_LABEL",
                                     "TIME_HORIZON",
                                     "XVAR",
                                     "YVAR"
                                   )) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be type character and length greater or equal to 1"
                                       )
                                     }
                                     
                                   }
                                   
                                   #### --- should be either null or type character and length greater or equal to 1 --- ####
                                   if (toupper(name) %in% c("FACETVAR")) {
                                     if (!(
                                       is.null(rlang::eval_tidy(dot)) |
                                       is.character(rlang::eval_tidy(dot)) &&
                                       length(rlang::eval_tidy(dot)) >= 1
                                     )) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be either null or type character and length greater or equal to 1"
                                       )
                                     }
                                     
                                   }
                                   
                                   #### --- should be type numeric and length greater or equal to 1 --- ####
                                   if (toupper(name) %in% c(
                                     "FIG_HEIGHT" ,
                                     "FIG_LINE_SIZE" ,
                                     "FIG_RES" ,
                                     "FIG_SCALE" ,
                                     "FIG_WIDTH",
                                     "DIGITS",
                                     "LEGEND_TEXTSIZE",
                                     "REL_COL_WIDTHS",
                                     "SEED_VALUE",
                                     "PREDICT_TIME"
                                   )) {
                                     if (!(is.numeric(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop("Bulletproofing: ",
                                            name,
                                            " should be type numeric and length greater or equal to 1")
                                     }
                                     
                                   }
                                   
                                   #### --- should be type character and length greater or equal to 1 and should be equal to 'D' for analysis in Days, 'W' for analysis in Weeks, 'M' for analysis in Months or 'Y' for analysis in Years" --- ####
                                   if (toupper(name) %in% c("TIME_UNIT")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) == 1)) {
                                       stop("Bulletproofing: ",
                                            name,
                                            " should be type character and length equal to 1")
                                     }
                                     if (!(rlang::eval_tidy(dot) %in% c("D", "W", "Y", "M"))) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be equal to 'D' for analysis in Days, 'W' for analysis in Weeks, 'M' for analysis in Months or 'Y' for analysis in Years"
                                       )
                                     }
                                     
                                   }
                                   
                                   #### --- should be type character and length greater or equal to 1 and should be either 'portrait' or 'landscape'--- ####
                                   if (toupper(name) %in% c("ORIENTATION")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be type character and length greater or equal to 1"
                                       )
                                     }
                                     if (!(rlang::eval_tidy(dot) %in% c("portrait", "landscape"))) {
                                       stop("Bulletproofing: ",
                                            name,
                                            " should be either 'portrait' or 'landscape'")
                                     }
                                     
                                   }
                                   
                                   #### --- should be type character and length greater or equal to 1 and should be either 'none', 'left', 'right', 'bottom' or 'top'--- ####
                                   if (toupper(name) %in% c("LEGEND_POSITION")) {
                                     if (!(is.character(rlang::eval_tidy(dot)) &&
                                           length(rlang::eval_tidy(dot)) >= 1)) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be type character and length greater or equal to 1"
                                       )
                                     }
                                     if (!(rlang::eval_tidy(dot) %in% c("none", "left", "right", "bottom", "top"))) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be either 'none', 'left', 'right', 'bottom' or 'top'"
                                       )
                                     }
                                     
                                   }
                                   
                                   #### --- should be either NULL or type character and length greater or equal to 1 and should be either 'Rank AIC', 'Rank BIC' or 'Rank Mean AIC BIC'--- ####
                                   if (toupper(name) %in% c("GOF_ARRANGE")) {
                                     if (!(
                                       is.null(rlang::eval_tidy(dot)) |
                                       is.character(rlang::eval_tidy(dot)) &&
                                       length(rlang::eval_tidy(dot)) >= 1
                                     )) {
                                       stop(
                                         "Bulletproofing: ",
                                         name,
                                         " should be either NULL or type character and length greater or equal to 1"
                                       )
                                     }
                                     if (!(is.null(rlang::eval_tidy(dot)))) {
                                       if (!(
                                         is.character(rlang::eval_tidy(dot)) &&
                                         rlang::eval_tidy(dot) %in% c("Rank AIC", "Rank BIC", "Rank Mean AIC BIC")
                                       )) {
                                         stop(
                                           "Bulletproofing: ",
                                           name,
                                           " should be either 'Rank AIC', 'Rank BIC' or 'Rank Mean AIC BIC' "
                                         )
                                       }
                                     }
                                     
                                   }
                                   
                                   #### --- should be equal to either TRUE or FALSE --- ####
                                   if (toupper(name) %in% c(
                                     "CUSTOM_MODEL_LABEL",
                                     "CSV_OUTPUT",
                                     "CI",
                                     "SE",
                                     "EXP",
                                     "BOUNDKNOTS",
                                     "FIG_GRID_X",
                                     "FIG_GRID_Y"
                                   )) {
                                     if (!(rlang::eval_tidy(dot) %in%  c(TRUE, FALSE))) {
                                       stop("Bulletproofing: ",
                                            name,
                                            " should be equal to either TRUE or FALSE")
                                     }
                                     
                                   }
                                   #### --- should be either a list or NULL --- ####
                                   if (toupper(name) %in% c("OUTPUT_SUFFIX_UPDATE", "RTF_PARAM", "SCALEXK_LIST")) {
                                     if (!(is.list(rlang::eval_tidy(dot)) |
                                           is.null(rlang::eval_tidy(dot)))) {
                                       stop("Bulletproofing: ", name, " should be either a list or NULL")
                                     }
                                     
                                   }
                                   
                                   #### --- should be a list --- ####
                                   if (toupper(name) %in% c("MODIFIED_VALUE", "OUTPUT_SUFFIX")) {
                                     if (!(is.list(rlang::eval_tidy(dot)))) {
                                       stop("Bulletproofing: ", name, " should be a list")
                                     }
                                     
                                   }
                                   
                                   #### --- should be a data frame --- ####
                                   if (toupper(name) %in% c("DATA")) {
                                     if (!(is.data.frame(rlang::eval_tidy(dot)))) {
                                       stop("Bulletproofing: ", name, " should be a data frame")
                                     }
                                     
                                   }
                                   
                                   #### --- should be of class function (ggplot2::labeller) and length 1 --- ####
                                   if (toupper(name) %in% c("FACETVAR_LEGEND")) {
                                     if (!any("function" %in% class(rlang::eval_tidy(dot))) &&
                                           length(rlang::eval_tidy(dot)) >= 1) {
                                       stop("Bulletproofing: ", name, " should be of class function (ggplot2::labeller) and length 1")
                                     }
                                     
                                   }
                                   
                                   
                                 })
  
  invisible(TRUE)
  
}
