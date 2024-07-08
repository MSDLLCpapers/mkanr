#' Extract Goodness of Fit
#'
#' Extract Goodness of Fit information from fit objects. 
#'
#' @section Specification:
#' \if{latex}{
#' Extract Goodness of Fit information from fit objects. 
#'
#' Logical Specifications:
#'   \enumerate{
#'   \item Bulletproof check fit object list using \code{\link{bullet_proof}}
#'   \item Update name of list elements using \code{\link{add_info_fit_desc}} with
#'   parameter exp and digits
#'   \item Loop through the fits in list and extract variance covariance 
#'   information based on class of object
#'         \enumerate{
#'         \item For flexsurvreg and flexsurvspline models, extract Goodness of Fit
#'               (AIC and BIC) from result of flexsurv::glance(fit)  
#'         \item Create tibble with 3 columns: distr_desc with name from step 2,
#'               AIC and BIC
#'         }
#'   \item Return list
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' 
#' @return List with variance covariance information for each element of input list 
#' 
#' @inherit add_info_fit_desc
#' @param fits {`list`}\cr flexsurvreg fit object
#' @param preprocessed_data {`data.frame`}\cr output of preprocess_data function
#'
#' @import dplyr
#' @importFrom purrr pmap
extract_gof <- function(fits=list(),
                        preprocessed_data=attributes(fits)$preprocessed_data,
                        exp=TRUE,
                        digits=2,
                        custom_model_label=TRUE) {
  
  # Bulletproof ----
  bullet_proof(fits=fits,
                      preprocessed_data=preprocessed_data,
                      exp = exp,
                      digits = digits,
                      custom_model_label=custom_model_label)
  
  #fit names - TODO retrieve dist info from fit instead names in list
  distrib_desc <- add_info_fit_desc(fits,
                                    exp=exp,
                                    digits=digits,
                                    custom_model_label=custom_model_label)
  
  # Read in the list of distributions to run models on from distrib parameter ----
  purrr::pmap(list(x=fits,y=distrib_desc),
              function(x,y){
                if(class(x) %in% c("flexsurvreg","flexsurvspline")){
                  tibble::tibble(distrib_desc=y,
                                 flexsurv::glance(x)[,c("AIC","BIC")])
                } 
              })
  
}

#' Extract Survival Time and Estimates
#'
#' Extract Survival Estimates information from fit objects. 
#'
#' @section Specification:
#' \if{latex}{
#' Extract Survival Estimates information from fit objects. 
#'
#' Logical Specifications:
#'   \enumerate{
#'   \item Bulletproof check fit object list using \code{\link{bullet_proof}}
#'   \item Update name of list elements using \code{\link{add_info_fit_desc}} with
#'   parameter exp and digits
#'   \item Loop through the fits in list and extract Survival Estimates information
#'    based on input parameters
#'        \enumerate{
#'             \item Retrieve survival estimate by calling the following function:\cr
#'                   summary(fit,tidy=TRUE,start=0,ci=`ci`,se=`se`,B=`B`,
#'                   na.action=`na.action`,cl=`cl`,type = "survival",t=`t`)
#'                   note: standard error and confidence intervals are generated
#'                    only if inputs are TRUE
#'                   note: set seed before calling summary method for reproducibility
#'                   with set.seed(`seed`)
#'             \item Retrieve hazard estimates by calling the following function:\cr
#'                   summary(fit,tidy=TRUE,start=0,ci=`ci`,se=`se`,B=`B`,
#'                   na.action=`na.action`,cl=`cl`,type = "hazard",t=`t`)
#'                   note: standard error and confidence intervals are generated
#'                    only if inputs are TRUE
#'                   note: set seed before calling summary method for reproducibility
#'                   with set.seed(`seed`)
#'             \item Combine survival and hazard estimate by left merging survival
#'                   with hazard by time point and add suffix _hazard to hazard
#'                    estimates column names
#'             \item Add column param_dist with fit names 
#'             \item Add column distr_desc with updated fit names from step 2
#'         }
#'   \item Format and append all data from all distributions into data frame
#'   \item Add surv_formula, preprocessed_data and t as attributes with same name
#'         surv_formula and preprocessed_data are retrieve form fits attributes
#'   \item Return list
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @return list with survival and hazard estimates information 
#'
#' @inherit add_info_fit_desc
#' @param fits {`list`}\cr flexsurvreg, flexsurvpline fit object
#' @param t {`numeric`}\cr numeric vector for time interval to extract\cr
#'                see t parameter in [flexsurv::summary.flexsurvreg()]
#' @param ci {`logical`}\cr boolean to include confidence interval estimates\cr
#'                see ci parameter in [flexsurv::summary.flexsurvreg()]
#' @param se {`logical`}\cr boolean to include standard error estimates\cr
#'                see se parameter in [flexsurv::summary.flexsurvreg()]
#' @param B {`numeric`}\cr Number of simulations from the normal asymptotic\cr
#'                distribution see B parameter in [flexsurv::summary.flexsurvreg()]
#' @param cl {`numeric`}\cr Width of symmetric confidence intervals, relative to 1\cr
#'                see cl parameter in [flexsurv::summary.flexsurvreg()]
#' @param na.action Function determining what should be done with missing values\cr
#'                see na.action parameter in [flexsurv::summary.flexsurvreg()]
#' @param seed seed to use to allow reproducibility of estimates
#'
#' @import dplyr 
#' @import flexsurv
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' 
extract_est <- function(fits=list(),
                            t=NULL,
                            ci=FALSE,
                            se=FALSE,
                            B=1000,
                            cl = 0.95,
                            na.action = na.pass,
                        exp=TRUE,
                        digits=2,
                        seed=1234,
                        custom_model_label=TRUE) {
  
  # Bulletproof ----
  bullet_proof(fits_flexsurv=fits,
               ci = ci,
               se = se,
               exp = exp,
               digits = digits,
               seed_value = seed,
               custom_model_label=custom_model_label)
  
  #format fit names - TODO retrieve dist info from fit instead names in list
  temp_data<-tibble::tibble(distr_desc=add_info_fit_desc(fits,
                                                         exp=exp,
                                                         digits=digits,
                                                         custom_model_label=custom_model_label),
                            param_dist=names(fits),
                            fits=fits)
  
  surv_00<-purrr::pmap(list(y=temp_data$fits),
                       function(y){
                         set.seed(seed);
                         tibble::tibble(summary(y,tidy=TRUE,start=0,ci=ci,se=se,B=B,na.action=na.action,cl=cl,type = "survival",t=t))
                       }
  )
  
  vec_condition<-c("est")
  if(isTRUE(ci)){
    vec_condition=c(vec_condition,"lcl","ucl")
  }
  if(isTRUE(se)){
    vec_condition=c(vec_condition,"se")
  }
  
  # Read in the list of distributions to run models on from distrib parameter ----
  surv_01<-purrr::pmap(list(base=surv_00,x=temp_data$fits,y=temp_data$distr_desc,z=temp_data$param_dist),
                       function(base,x,y,z,u){
                         set.seed(seed);
                         dplyr::left_join(
                           dplyr::bind_cols(
                             base,
                             tibble::tibble(
                               param_dist=rep(z,dim(base)[1]),
                               distr_desc=rep(y,dim(base)[1])
                             )
                           ),
                           tibble::tibble(summary(x, type = "hazard",start=0 ,ci=ci,se=se,B=B,na.action=na.action,cl=cl, tidy = TRUE,t=t)),
                           by = colnames(dplyr::select(base,-all_of(vec_condition))),
                           suffix=c("","_hazard"))
                       })
  
  #Return list with attributes
  structure(surv_01,
            surv_formula=attributes(fits)$surv_formula,
            preprocessed_data=attributes(fits)$preprocessed_data,
            t=t)
  
}


#' Fit description customization
#' 
#' @section Specification:
#' \if{latex}{
#' Add knots location to fit name. 
#'
#' Logical Specifications:
#'   \enumerate{
#'   \item Bulletproof check fit object list using \code{\link{bullet_proof}}
#'   \item Loop through the fits, extract knot location information and update
#'       fit name. If fit$knots exists:
#'        \enumerate{
#'             \item Extract knot(s)' location information from fit$knots
#'             \item keep only internal knot(s) - remove first and last 
#'             \item round knot location value with round(...,digits=digits) function
#'             \item concatenate fit name and knot location information using 
#'                 glue::glue_collapse(...,sep=', ')
#'         }
#'   \item Output updated name vector
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' @param fits {`list`}\cr flexsurvreg, flexsurvpline fit object
#' @param exp {`logical`}\cr Boolean to indicate if exponential transformation 
#'                           should be used on knots location values
#' @param digits {`integer`}\cr Indicate the number of decimal places to use when
#'                            rounding knots location values see [base::round()]
#' @param custom_model_label {`logical`}\cr Boolean to indicate if spline knots 
#'                            locations should be included in the model label
#'
#' @return Character vector with fit name with knot location added
#' @keywords internal
#' 
#' @importFrom purrr pmap
#' @importFrom glue glue glue_collapse
#'
add_info_fit_desc<-function(fits,
                            exp=TRUE,
                            digits=2,
                            boundknots=FALSE,
                            custom_model_label=TRUE){
  
  # Bulletproof ----
  bullet_proof(fits=fits,
               exp = exp,
               digits = digits,
               boundknots =  boundknots,
               custom_model_label=custom_model_label)
  
  # Update fit name ----
  distr_desc=purrr::pmap(list(fit=fits,name=names(fits)),
                         function(fit,name){
                           if(!is.null(fit$knots) & isTRUE(custom_model_label)){
                             if(isTRUE(exp) & isFALSE(boundknots)){
                               glue::glue("{name} ({glue::glue_collapse(round(head(exp(fit$knots[-1]),-1),digits={digits}),sep=', ')})")
                             }else if(isFALSE(exp) & isFALSE(boundknots)){
                               glue::glue("{name} ({glue::glue_collapse(round(head(fit$knots[-1],-1),digits={digits}),sep=', ')})")
                             }
                             else if(isTRUE(exp) & isTRUE(boundknots)){
                               glue::glue("{name} ({glue::glue_collapse(round(exp(fit$knots),digits={digits}),sep=', ')})")
                             }else if(isFALSE(exp) & isTRUE(boundknots)){
                               glue::glue("{name} ({glue::glue_collapse(round(fit$knots,digits={digits}),sep=', ')})")
                             }
                           }else{
                             name
                           }
                         })
  
  # Output name vector ----
  unlist(distr_desc)
  
}


#' Row header customization
#' 
#' @section Specification:
#' \if{latex}{
#' Add log(..) text to row name if value are on log scale. 
#'
#' Logical Specifications:
#'   \enumerate{
#'   \item Compare res and res.t value object. If fit$res and fit$res.t exist:
#'        \enumerate{
#'             \item Extract row names
#'             \item Loop through rows of res.t/res to compare values in first
#'                   column. If res value is equal to exponential of res.t value
#'                   then update rowname to add log using glue::glue("log({rowname}))
#'         }
#'   \item Output updated row name vector
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' @param fit {`flexsurvreg`}\cr flexsurvreg fit object
#'
#' @return Character vector with updated row names
#' @keywords internal
#' 
#' @importFrom dplyr left_join mutate
#' @importFrom glue glue 
#'
add_log_rest_row <- function(fit) {
  # Update row name ----
  if (!is.null(fit$res.t) & !is.null(fit$res)) {
    res_all = dplyr::left_join(
      tibble::as_tibble(fit$res, rownames = "row_desc"),
      tibble::as_tibble(fit$res.t, rownames = "row_desc"),
      by = "row_desc",
      suffix = c("", "_t")
    ) |>
      dplyr::mutate(row_desc_new = ifelse(est == exp(est_t), glue::glue("log({row_desc})"), row_desc))
    
    res_all$row_desc_new
  }
  
}

