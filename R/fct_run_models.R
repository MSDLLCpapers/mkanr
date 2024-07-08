#' Flexsurvreg Wrapper
#'
#' `run_models_flexsurvreg()` is a wrapper around [flexsurv::flexsurvreg()].
#' It loops through dist_list to run flexible parametric models for the same set of
#' input parameters (preprocessed_data, surv_formula, na.action, ...) and
#' different distributions (see dist parameter from [flexsurv::flexsurvreg())].
#'
#' @section Specification:
#' \if{latex}{
#' `run_models_flexsurvreg()` is a wrapper around [flexsurv::flexsurvreg()].
#' It loops through dist_list to run flexible parametric models for the same set of
#' input parameters (preprocessed_data, surv_formula, na.action, ...) and
#' different distributions (see dist parameter from [flexsurv::flexsurvreg)].
#' 
#'  Logical Specifications:
#'   \enumerate{
#'   \item Check input parameters using \code{\link{bullet_proof}}
#'   \item Loop through the distribution in dist_list (dist) and run [flexsurv::flexsurvreg()]
#'     function on dist and other input parameters (surv_formula, na.action, ...).
#'         \enumerate{
#'         \item inform user when a model starts running
#'         \item wrap try catch around model run to retrieve errors and warnings if any
#'         \item if flexsurvsreg throws any error/warning, inform user and do not
#'             include model in output
#'         \item if flexsurvreg does not throw error/warning, return model fit
#'             object
#'        }
#'    \item Create list of fits with elements named as in dist_list input
#'    \item Add the following attributes to the list of fits to allow traceability
#'    \enumerate{
#'         \item preprocessed_data (=preprocessed_data)
#'         \item surv_formula (=surv_formula)
#'         \item dist_list (=dist_list)
#'         \item na.action (=na.action)
#'         \item model_type (="flexsurvreg")
#'         \item other (=rlang::exprs(...))
#'        }
#'    \item If at least one model ran without error/warnings return named list 
#'    of flexsurvreg fit objects, throw an error otherwise 
#'    }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param preprocessed_data {`data.frame`}\cr Output of preprocess_data function
#' @param surv_formula {`formula`}\cr Surv formula define with stats::formula and survival::Surv
#' @param dist_list {`list`}\cr List of distributions of parametric model 
#'                see flexsurv::flesurvreg for details
#' @param na.action a missing-data filter function, applied after any 'subset' argument has been used.
#'            Default is na.omit
#' @param ... other parameter to pass in flexsurv::flexsurvreg
#'
#' @return Named list of flexsurvreg objects (see [flexsurv::flexsurvreg()] for details)
#' 
run_models_flexsurvreg <- function(preprocessed_data,
                                  surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ 1),
                                  dist_list = list(exponential="exp",
                                                   weibull="weibull", 
                                                   gompertz="gompertz", 
                                                   gamma="gamma", 
                                                   lognormal="lognormal", 
                                                   loglogistic="llogis", 
                                                   gengamma="gengamma"),
                                  na.action=na.omit,
                                  ...) {
  
  #bulletproofing ----
  bullet_proof(
    preprocessed_data = preprocessed_data,
    surv_formula = surv_formula,
    dist_list = dist_list
  )
  
  # apply h_main_survival_parametric to all distributions ----
  results <- mapply(
    FUN = h_main_flexsurvreg, 
    # arg to vectorize over:
    dist = dist_list,   
    # a list of other arguments to FUN:
    MoreArgs = list( preprocessed_data=preprocessed_data,
                     surv_formula=surv_formula,
                     na.action=na.action,
                     ...),
    SIMPLIFY = FALSE)
  
  
  o_results <- results[!is.na(results)]
  
  if(length(o_results)==0) stop("Modelling: all flexsurvreg models have warnings/error")
  
  #Add attributes
  attributes(o_results)$preprocessed_data<-preprocessed_data
  attributes(o_results)$surv_formula<-surv_formula
  attributes(o_results)$dist_list<-dist_list
  attributes(o_results)$na.action<-na.action
  attributes(o_results)$model_type<-"flexsurvreg"
  attributes(o_results)$other<-rlang::exprs(...)
  
  # return fits ----
  o_results 
}

#' h_main_flexsurvreg main functionality
#'
#' @inheritParams run_models_flexsurvreg
#' @param dist distribution parameter used in flexsurv::flexsurvreg()
#'
#' @import survival
#' @import flexsurv
#'
#' @keywords internal
h_main_flexsurvreg <- function(dist,
                              surv_formula,
                              preprocessed_data,
                              na.action,
                              ...) {
  message("Modelling: working on flexsurvreg model with ", dist, " distribution (dist)")
  
  
  # Fit parametric model
  surv_distrib <- tryCatch(
    flexsurv::flexsurvreg(formula=surv_formula,
                          data = preprocessed_data,
                          dist = dist,
                          na.action=na.action,
                          ...),
    error = function(e)  {list(dist,e)},
    warning = function(w) {list(dist,w)} 
  )
  
  # Dynamically return fit if no error or warning
  if(is.list(surv_distrib) && ("error" %in% class(surv_distrib[[2]]) || "simpleError" %in% class(surv_distrib[[2]]) ||
                               "warning" %in% class(surv_distrib[[2]]) || "simpleWarning" %in% class(surv_distrib[[2]]))){
    message(class(surv_distrib[[2]])[1], " for ", surv_distrib[[1]], " distribution in flexsurv::flexsurvreg with the following message: ",surv_distrib[[2]]$message)
    return(NA)
  }else{
    return(surv_distrib)
  }
  
}


#' Flexsurvspline Wrapper
#'
#' `run_models_flexsurvpline()` is a wrapper around [flexsurv::flexsurvspline()].
#' It loops through scalexk_list to run flexible spline models for the same set of
#' input parameters  (preprocessed_data, surv_formula, na.action, method, seed.value ...)
#' and different scale and knots (see scale, k and knots parameter from [flexsurv::flexsurvspline())].
#'
#' @section Specification:
#' \if{latex}{
#' `run_models_flexsurvpline()` is a wrapper around [flexsurv::flexsurvspline()].
#' It loops through scalexk_list to run flexible spline models for the same set of
#' input parameters  (preprocessed_data, surv_formula, na.action, method, seed.value ...)
#' and different scale and knots (see scale, k and knots parameter from [flexsurv::flexsurvspline())].
#' 
#'  Logical Specifications:
#'   \enumerate{
#'   \item Check input parameters using \code{\link{bullet_proof}}
#'   \item Loop through scalexk_list the number of knots in spline (k) and scale (hazard,
#'       odds or probit) and run [flexsurv::flexsurvspline())] function on a set
#'       of input parameters (preprocessed_data, surv_formula, na.action, method, seed.value ...)
#'         \enumerate{
#'         \item wrap try catch around model to retrieve errors and warnings
#'         \item if flexsurvspline throws any error/warning, inform user and do not
#'             include model output
#'         \item if flexsurvspline does not throw error/warning, return model fit
#'             object
#'        }
#'    \item Create list of fits with elements named as in scalexk_list input
#'    \item Add the following attributes to the list of fits to allow traceability
#'    \enumerate{
#'         \item preprocessed_data (=preprocessed_data)
#'         \item surv_formula (=surv_formula)
#'         \item scalexk_list (=scalexk_list)
#'         \item na.action (=na.action)
#'         \item model_type (="flexsurvspline")
#'         \item method (=method)
#'         \item seed_value (=seed_value)
#'         \item other (=rlang::exprs(...))
#'        }
#'    \item If at least one model ran without error/warnings return named list 
#'    of flexsurvspline fit objects, throw an error otherwise 
#'    }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param preprocessed_data {`data.frame`}\cr Output of preprocess_data function
#' @param surv_formula {`formula`}\cr Surv formula define with stats::formula and survival::Surv
#' @param scalexk_list {`list`}\cr Named list of lists of scale and k parameters
#'                see flexsurv::flexsurvspline for details
#' @param seed_value Value for seed to be used in set.seed function 
#' @param method Method to be used (refer to stats::optim Details section)
#' @param na.action a missing-data filter function, applied after any 'subset' argument has been used.
#'            Default is na.exclude
#' @param ... other parameters to use in flexsurv::flexsurvspline
#' 
#' @return Named list of flexsurvpline fit objects
#' 
run_models_flexsurvspline <- function(preprocessed_data,
                                     surv_formula = stats::formula(survival::Surv(AVAL2, 1-CNSR) ~ TRT01P),
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
                                     na.action=na.omit,
                                     method = "Nelder-Mead",
                                     ...) {
  
  #bulletproofing
  bullet_proof(
    preprocessed_data = preprocessed_data,
    surv_formula = surv_formula,
    scalexk_list = scalexk_list,
    seed_value = seed_value
  )
  
  # loop through scalexk_list
  results <- mapply(
    FUN = h_main_flexsurvspline, 
    # arg to vectorize over:
    scalexk = scalexk_list, 
    # a list of other arguments to FUN:
    MoreArgs = list( preprocessed_data=preprocessed_data,
                     surv_formula=surv_formula,
                     seed_value = seed_value,
                     method=method,
                     na.action=na.action,
                     ...),
    SIMPLIFY = FALSE)
  
  
  o_results <- results[!is.na(results)]
  
  if(length(o_results)==0) stop("Modelling: all flexsurvpline models have warnings/error")
  
  #Add attributes
  attributes(o_results)$preprocessed_data<-preprocessed_data
  attributes(o_results)$surv_formula<-surv_formula
  attributes(o_results)$scalexk_list<-scalexk_list
  attributes(o_results)$model_type<-"flexsurvspline"
  attributes(o_results)$na.action<-na.action
  attributes(o_results)$seed_value<-seed_value
  attributes(o_results)$method<-method
  attributes(o_results)$other<-rlang::exprs(...)
  
  # return list
  o_results 
  
}

#' h_main_flexsurvspline main functionality
#'
#' @inheritParams run_models_flexsurvspline
#' @param scalexk list with scale and knot arguement used in flexsurv::flexsurvreg()
#'
#' @import survival
#' @import flexsurv
#' @importFrom dplyr case_when
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal
h_main_flexsurvspline <- function(scalexk,
                                   preprocessed_data,
                                   surv_formula,
                                   seed_value,
                                   method,
                                   na.action,
                                   ...) {
  
  
  if(length(scalexk$knots)>0){
    message("Modelling: working on flexsurvspline model with ", scalexk$scale, " scale and ",glue::glue("internal knots located at {glue::glue_collapse(scalexk$knots,sep=', ')}"))
  }else{
    message("Modelling: working on flexsurvspline model with ", scalexk$scale, " scale and ",scalexk$k, dplyr::case_when(scalexk$k==1 ~ " knot",
                                                                                                                         TRUE ~ " knots"))
  }
  
  
  
  # Set seed
  set.seed(seed_value)
  
  #run model
  m1 <- tryCatch(flexsurv::flexsurvspline(surv_formula,
                                          data = preprocessed_data, 
                                          scale = scalexk$scale,
                                          k = scalexk$k,
                                          knots=scalexk$knots,
                                          na.action = na.action,
                                          method = method,
                                          ...),
                 error = function(e)  {list(scalexk,e)},
                 warning = function(w) {list(scalexk,w)} 
  )
  
  if(is.list(m1) && ("error" %in% class(m1[[2]]) || "simpleError" %in% class(m1[[2]]) ||
                     "warning" %in% class(m1[[2]]) || "simpleWarning" %in% class(m1[[2]]))){
    message(class(m1[[2]])[1], " for model with scale=", m1[[1]]$scale, " and knot=",m1[[1]]$k," in flexsurv::flesurvspline with the following message: ",m1[[2]]$message)
    return(NA)
  }else{
    return(m1)
  }
}
