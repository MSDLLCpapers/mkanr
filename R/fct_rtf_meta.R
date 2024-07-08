#' Customize RTF formatting of the outputs
#'
#' This function contains standards metadata for RTF tables and figures.
#' 
#' @section Specification:
#' \if{latex}{ This function contains a list of formats for tables and figures 
#' in RTF format for {mkanr} outputs.
#' It allows the user to update the RTF formatting of the output such as titles, 
#' footnotes, column headers and columns widths as per project requirement.
#'
#' Logical Specifications:
#'   \enumerate{
#'   \item Check input parameters
#'   \item Define a list called `rtf_meta` of RTF formats with default parameter values as per mock-up document 
#'   \item Modify the RTF parameter values in `rtf_meta` list based on input lists as per function definition if `output_suffix_update`, `rtf_param` and `modified_value` are not NULL
#'   \enumerate{ 
#'       \item Define a helper function `update_value` to update a parameter value in RTF formats list as per input lists `output_suffix_update`, `rtf_param` and `modified_value`.
#'       \item Update selected values of parameters and outputs in `rtf_meta` list by applying `pmap` to `update_value` function
#'   }
#'   \item Return list of rtf formatting for the outputs (`rtf_meta`)
#'   \enumerate{
#'       \item if `output_suffix == "all"`, return entire list of RTF formatting for all outputs (`rtf_meta`)
#'       \item else, return RTF formatting for selected output as per output_suffix (`rtf_meta$output_suffix`)  
#'   }
#' }
#' }
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#' 
#' @param output_suffix List containing suffixes of the outputs as per documentation, e.g.: "e0stdflex0gof" for Goodness of Fit table. output_suffix="all" will output entire list of formats for all outputs
#' @param rtf_title_pop Table subtitle(s) for all rtf outputs 
#' @param rtf_footnote_pop Table footnote(s) for all rtf outputs 
#' @param param Parameter label 
#' @param time_horizon Time horizon length for extrapolation 
#' @param therapy_arm Therapy arm description
#' @param models_title Model title 
#' @param output_suffix_update List of outputs as output_suffix for whom the parameter values are to be updated. Please note that the values need to be valid output_suffices as per documentation 
#' @param rtf_param List of RTF parameters for whom the values are to be updated. Please note that the parameters names need to be valid
#' @param modified_value List of modified values  
#' @param therapy_data_var {`list`}\cr therapy variable factor metadata with elements:
#' \tabular{lll}{ \code{name} \tab {`character`} \tab  variable name to convert \cr 
#'                 \code{levels} \tab {`character`} \tab preferred order of values taken by variable \cr 
#'                 \code{labels} \tab {`character`} \tab preferred label of values taken by variable \cr
#'                 \code{name_label} \tab {`character`} \tab variable label \cr}
#' 
#' @return list of lists containing rtf metadata
#' 
#' @export
#'
#' @examples 
#' \dontrun{
#' #Generate default metadata object
#' rtf_metadata<-fct_rtf_meta(output_suffix=list("all"),
#' rtf_title_pop=c("Subtitle 1", "(Population)"),
#' rtf_footnote_pop=c("Database Cutoff Date: XXX"),
#' param="Overall Survival",
#' therapy_data_var = list(name="TRT01P",
#' name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
#' time_horizon=c("20 Years"),
#' models_title="Standard Parametric",
#' therapy_arm = c("B: Placebo", "A: Drug X")
#'                              )
#'                              
#' #Look up Goodness of Fit table metadata
#' rtf_metadata$e0stdflex0gof
#' 
#' #Customized Goodness of Fit table metadata
#' rtf_meta<-fct_rtf_meta(output_suffix=list("all"),
#' rtf_title_pop=c("Subtitle 1", "(Population)"),
#' rtf_footnote_pop=c("Database Cutoff Date: XXX"),
#'  param="Overall Survival",
#'  time_horizon=c("20 Years"),
#'  therapy_data_var = list(name="TRT01P",
#'  name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
#'  models_title="Standard Parametric",
#'  therapy_arm = c("B: Placebo", "A: Drug X"),
#'  output_suffix_update = list("e0stdflex0gof", "e0stdflex0gof", "e0stdflex0gof"),
#'  rtf_param=list("rtf_title[1]","header_label", "csv_output"),
#'  modified_value=list("Goodness of Fit Statistics Resulting from Independent
#'  Standard Parametric Modeling for Overall Survival in Placebo Arm",
#'  "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Mean Rank AIC BIC", "FALSE")
#'                              )    
#'  }
#'  
fct_rtf_meta <- function(output_suffix=list("all"),
                           rtf_title_pop=c("Subtitle 1","(ITT Population)"),
                           rtf_footnote_pop=c("Database Cutoff Date: XXX"),
                           time_horizon=c("20 Years"),
                           therapy_data_var= list(name="TRT01P",name_label="Planned Treatment for Period 01",levels=NULL,labels=NULL),
                           therapy_arm = levels(mkanr::adtte[["TRT01P"]]),
                           param="Overall Survival",
                           models_title="Standard Parametric",
                           output_suffix_update=NULL, 
                           rtf_param=NULL, 
                           modified_value=list(NULL)) {

  # Bulletproof ----
  bullet_proof(
    output_suffix = output_suffix,
    rtf_title_pop = rtf_title_pop,
    rtf_footnote_pop = rtf_footnote_pop,
    time_horizon = time_horizon,
    therapy_data_var = therapy_data_var$name,
    therapy_arm =therapy_arm,
    param = param,
    models_title = models_title,
    output_suffix_update = output_suffix_update,
    rtf_param = rtf_param,
    modified_value = modified_value
  )
  
  # Dynamic spline footnotes
  if(grepl("spline",models_title,ignore.case=TRUE)){
    spline_short_footnote=c("Spline models are fitted on three scales: proportional hazards, proportional odds, and normal/probit.")
  }else{
    spline_short_footnote=c()
  }
  
  # Define list of RTF formats with default parameter values ----
  rtf_meta = list(
    
    ## e0stdflex0gof ----
    e0stdflex0gof=list(rtf_title = c(paste0("Goodness of Fit Statistics Resulting from Independent ",models_title," Modeling for ", param," in ", therapy_arm[1]), rtf_title_pop),
                       rtf_footnote = c("For more details on the survival models please refer to distributions reference vignette: https://cran.r-project.org/web/packages/flexsurv/vignettes/distributions.pdf.",
                                        spline_short_footnote,rtf_footnote_pop),
                       header_label = "Survival Model|AIC|BIC|Mean AIC BIC|Rank AIC|Rank BIC|Rank Mean AIC BIC",
                       rel_col_widths = c(4, rep(1, 6)),
                       col_just = c("l", rep("r", 6)),
                       col_format = c(NA, rep("f", 3),rep("d", 3)),
                       col_digits = c(NA, rep(1, 3), rep(0, 3)),
                       orientation = "portrait",
                       csv_output=TRUE,
                       custom_model_label=FALSE),
    
    
    ## e0stdflex0fig0timeshort ----
    e0stdflex0fig0timeshort=list(rtf_title = c(paste0("Estimated Survival Probability for ", param, " from Independent ",models_title," Models \n Against Empirical Survival Probability from Kaplan-Meier Analysis \n During the Study Period in ",therapy_arm[1]), rtf_title_pop),
                                 rtf_footnote = c("For more details on the survival models please refer to distributions reference vignette: https://cran.r-project.org/web/packages/flexsurv/vignettes/distributions.pdf.",
                                                  spline_short_footnote,
                                                  rtf_footnote_pop
                                 ),
                                 fig_height=4,
                                 fig_width=6,
                                 orientation = "portrait",
                                 custom_model_label=FALSE),
    
    ## e0stdflex0fig0timelong ----
    e0stdflex0fig0timelong=list(rtf_title = c(paste0("Estimated Long Term Survival Probability (",time_horizon,") for ", param, " from Independent ",models_title," Models \n Against Empirical Survival Probability from Kaplan-Meier Analysis \n During the Study Period in ",therapy_arm[1]), rtf_title_pop),
                                rtf_footnote = c("For more details on the survival models please refer to distributions reference vignette: https://cran.r-project.org/web/packages/flexsurv/vignettes/distributions.pdf.",
                                                 spline_short_footnote,
                                                 rtf_footnote_pop
                                ),
                                fig_height=4,
                                fig_width=6,
                                orientation = "portrait",
                                custom_model_label=FALSE)
    
  ) 
  
  # Modify the RTF parameter values based on input lists as per function definition ----
  if (!is.null(output_suffix_update) && !is.null(rtf_param) && !is.null(modified_value)){
    
    ## Define a helper function to update the parameter values in RTF formats list
    update_value <- function(output_suffix_update,
                             rtf_param,
                             modified_value) {
      eval(parse(text=paste0("rtf_meta$",output_suffix_update,"$",rtf_param,"<<-modified_value")))
      return(rtf_meta)
    }
    ## Update selected values of parameters and outputs
    purrr::pmap(list(x=output_suffix_update, y=rtf_param, z=modified_value),
                function(x,y,z) update_value(x,y,z))
  }
  
  # Return list of rtf formattings for the outputs ----
  ## Return entire list of RTF formatting for all outputs
  if (identical(output_suffix, list("all"))){
    rtf_meta
  } ## Return RTF formattings for selected output as per output_suffix
  else {
    rtf_meta_r = lapply(output_suffix, function(output_suffix) eval(parse(text=paste0("rtf_meta$",output_suffix))))
    names(rtf_meta_r)=output_suffix
    rtf_meta_r
  }
  
}