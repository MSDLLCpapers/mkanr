#' Format Survival Estimates and include Kaplan-Meier estimates
#'
#' Format Survival Estimates to:
#' - combine all models
#' - add Kaplan-Meier estimates using either object attributes or additional inputs
#' - apply formula based on Kaplan-Meier estimates and flexsurv estimates
#'
#' @section Specification:
#' \if{latex}{
#' Format Survival Estimates and include Kaplan-Meier estimates.
#'
#' Format Survival Estimates to:
#' - combine all models
#' - add Kaplan-Meier estimates using either object attributes or additional inputs
#' - apply formula based on Kaplan-Meier estimates and flexsurv estimates
#' 
#' Logical Specifications:
#'   \enumerate{
#'   \item Check input parameters using \code{\link{bullet_proof}}
#'   \item Convert est input to data.frame using do.call(dplyr::bind_rows,est)
#'   \item Generate survfit object using survival::survfit(surv_formula,
#'         data = preprocessed_data_km, type = type, ...)
#'   \item Retrieve analysis variable name from surv_formula using surv_formula[[2]][[2]]
#'   \item Extract survival estimates from survfit object with times from 0 to 
#'         max value of preprocessed_data_km$analysisvariable using summary method
#'   \item Combine survfit estimates with flexsurv estimates
#'         \enumerate{
#'         \item Decompose survfit strata variable to identify covariates
#'         \item If at least one covariate found
#'               \enumerate{
#'               \item Format strata variable from survfit strata to match est 
#'               output
#'               \item Format survfit summary to match flexsurv summary using
#'                     param_dist="kaplan_meier"
#'                     distr_desc="Kaplan-Meier"
#'                     time=time
#'                     est=surv
#'                     keep strata
#'               }
#'         \item If no covariate found
#'               \enumerate{
#'               \item Format survfit summary to match flexsurv summary using
#'                     param_dist="kaplan_meier"
#'                     distr_desc="Kaplan-Meier"
#'                     time=time
#'                     est=surv
#'                     keep strata
#'               }
#'         \item Append survfit estimates with estimates from previous step
#'         }
#'   \item Convert distr_desc variable to factor
#'   \item Save RData data to outdata_path with rename_output name
#'   \item Return data.frame
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' 
#' @return {`data.frame`}\cr formatted data with variables:
#' \tabular{lll}{ \code{x_primary} \tab {`double`} \tab time information \cr 
#'                 \code{y_primary} \tab {`double`} \tab original survival estimates \cr 
#'                 \code{param_dist} \tab {`character`} \tab parameter description \cr
#'                 \code{group_primary} \tab {`factor`} \tab  distribution description \cr 
#'                 \code{covariate(s)} \tab {`factor`} \tab covariate(s) as in right hand side of model formula \cr}
#'
#' @param est {`list`}\cr named list with survival estimates, each element represent
#'                       a model  (result from extract_est function)
#' @param preprocessed_data {`data.frame`}\cr data used to produced flexsurv estimates
#' @param preprocessed_data_km {`data.frame`}\cr data used to produced Kaplan-Meier
#'                     estimates (see [survival::survfit()])
#' @param surv_formula {`formula`}\cr formula used to produced Kaplan-Meier
#'                     estimates (see [survival::survfit()])
#' @param type {`character`}\cr type used to produced Kaplan-Meier
#'                     estimates (see [survival::survfit()])
#' @param ... other parameters to pass to survfit (see [survival::survfit()])
#' @param rename_output {`character`}\cr output name
#' @param outdata_path {`character`}\cr location to save output
#'
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split
#' 
#' @examples
#' \dontrun{
#' # regular analysis
#' tte <- preprocess_data(population_from = mkanr::adsl,
#'                 observation_from = mkanr::adtte)
#'                 
#' tte |>
#'    run_models_flexsurvreg() |>
#'    extract_est()|>
#'    format_est()
#'    
#' # piece-wise analysis
#'tte <- preprocess_data(population_from = mkanr::adsl,
#'                       observation_from = mkanr::adtte,
#'                       observation_where = AVAL2>10,
#'                       analysis_variable=list(name="AVAL2", derivation="AVAL/7-10",
#'                                              label='Analysis Variable 2'),
#'                       rm_obs=TRUE)
#'
#'tte_km<- preprocess_data(population_from = mkanr::adsl,
#'                       observation_from = mkanr::adtte,
#'                       observation_where = ,
#'                       analysis_variable=list(name="AVAL2", derivation="AVAL/7",
#'                                              label='Analysis Variable 2'),
#'                       rm_obs=FALSE)
#'
#'tte |>
#'  run_models_flexsurvreg() |>
#'  extract_est()|>
#'  format_est(preprocessed_data_km=tte_km)
#'                 
#' }
format_est <- function(est,
                       preprocessed_data=attributes(est)$preprocessed_data,
                       preprocessed_data_km=attributes(est)$preprocessed_data,
                       surv_formula=attributes(est)$surv_formula,
                       type="fleming",
                       ...,
                       rename_output="test",
                       outdata_path=tempdir()) {
  
  # Bulletproof ----
  bullet_proof(
    preprocessed_data=preprocessed_data,
    preprocessed_data_km=preprocessed_data_km,
    surv_formula=surv_formula,
    rename_output = rename_output,
    outdata_path = outdata_path
  )
  
  # Convert est to data.frame ----
  est_df<-do.call(dplyr::bind_rows,est)
  
  # Survival curve(s) estimates using survfit ----
  survfit_object<-survival::survfit(surv_formula,
                                    data = preprocessed_data_km,
                                    type = type,
                                    ...)
  
  # Extract survival estimates from survfit object ----
  summary_surv<-summary(survfit_object,
                        times=0:ceiling(max(preprocessed_data_km[as.character(surv_formula[[2]])[2]])))
  
  # Combine survfit estimates with flexsurv estimates ----
  
  ## If no covariate ----
  ### Format survfit summary to match flexsurv summary ----
  fmt_summary_surv_02<-tibble::tibble(time=summary_surv$time,
                                      est=summary_surv$surv,
                                      param_dist="kaplan_meier",
                                      distr_desc="Kaplan-Meier")
  
  ## Append survfit estimates with flexsurv estimates ----
  final_01<-dplyr::bind_rows(fmt_summary_surv_02,
                             est_df)
  
  ## Convert distr_desc variable to factor
  final_02<-final_01 |>
      dplyr::mutate(distr_desc=factor(distr_desc,levels=unique(final_01$distr_desc))) |>
      dplyr::select(-est_hazard)
  
  ## Standardized variable names
  final_03 <- final_02 |> 
    dplyr::rename(x_primary=time,
                  y_primary=est,
                  group_primary=distr_desc)
  
  # Save RData data to outdata_path with rename_output name ----
  assign(rename_output, final_03)
  save(list = rename_output, file = file.path(outdata_path, paste0(rename_output, ".RData")))
  
  #Return data ----
  final_03
  
}


#' Format Goodness of Fit 
#'
#' Format Goodness of Fit to:
#' - combine all models
#' - add Mean AIC BIC, rank AIC, rank BIC and rank Mean AIC BIC
#' - order table by one of the variable above
#'
#' @section Specification:
#' \if{latex}{
#' Format Goodness of Fit 
#'
#' Format Goodness of Fit to:
#' - combine all models
#' - add Mean AIC BIC, rank AIC, rank BIC and rank Mean AIC BIC
#' - order table by one of the variable above
#' 
#' Logical Specifications:
#'   \enumerate{
#'   \item Check input parameters using \code{\link{bullet_proof}}
#'   \item Convert extract_gof output to data.frame using 
#'   do.call(dplyr::bind_rows,gof)
#'   \item Derive additional variables
#'        \enumerate{
#'        \item Mean AIC BIC = (AIC + BIC)/2
#'        \item Rank AIC = base::rank(AIC)
#'        \item Rank BIC = base::rank(BIC)
#'        \item Rank Mean AIC BIC = base::rank(`Mean AIC BIC`)
#'        }
#'   \item Order table gof_arrange is a valid column name
#'   \item Save RData data to outdata_path with rename_output name
#'   \item Return data
#'   }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' 
#' @return {`data.frame`}\cr formatted data with variables:
#' \tabular{lll}{ \code{Survival Model} \tab {`character`} \tab Model distribution description \cr 
#'                 \code{AIC} \tab {`double`} \tab Akaike Information Criterion  \cr 
#'                 \code{BIC} \tab {`double`} \tab Bayesian Information Criterion \cr
#'                 \code{Mean AIC BIC} \tab {`double`} \tab Mean of AIC and BIC \cr 
#'                 \code{Rank AIC} \tab {`double`} \tab Akaike Information Criterion ranking  \cr 
#'                 \code{Rank BIC} \tab {`double`} \tab Bayesian Information Criterion ranking \cr
#'                 \code{Rank AIC BIC} \tab {`double`} \tab Mean AIC BIC ranking \cr}
#'
#' @param gof {`list`}\cr output of extract_gof function
#' @param gof_arrange {`character`}\cr variable used to order output. Possible
#'                    values are:
#'                     - NULL = no ordering
#'                     - 'Rank AIC'
#'                     - 'Rank BIC'
#'                     - 'Rank Mean AIC BIC' 
#' @param rename_output {`character`}\cr output name
#' @param outdata_path {`character`}\cr location to save output
#'
#' @import dplyr
format_gof <- function(gof,
                       gof_arrange=NULL,
                       rename_output="test",
                       outdata_path=tempdir()) {
  
  # Bulletproof ----
  bullet_proof(
    gof_arrange = gof_arrange,
    rename_output = rename_output,
    outdata_path = outdata_path
  )
  
  # Convert extract_gof output to data.frame using do.call(dplyr::bind_rows,gof) ----
  gof_data<-do.call(dplyr::bind_rows, gof) 
  
  # Derive additional variables ----
  gof_table<-gof_data |>
    dplyr::mutate(`Mean AIC BIC`=(AIC+BIC)/2,
                  `Rank AIC`=base::rank(AIC),
                  `Rank BIC`=base::rank(BIC),
                  `Rank Mean AIC BIC`=base::rank(`Mean AIC BIC`))
  
  # Order table gof_arrange is a valid column name ----
  if(!is.null(gof_arrange) && (gof_arrange %in% colnames(gof_table))){
    final_01<-gof_table |>
      dplyr::arrange(get(gof_arrange))|>
      dplyr::rename(`Survival Model`=distrib_desc)
  } else{
    final_01<-gof_table|>
      dplyr::rename(`Survival Model`=distrib_desc)
  }
  
  # Save RData data to outdata_path with rename_output name ----
  assign(rename_output, final_01)
  save(list = rename_output, file = file.path(outdata_path, paste0(rename_output, ".RData")))
  
  # Return data -----
  final_01
  
}

