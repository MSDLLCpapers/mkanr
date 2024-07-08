#' Perform Parametric Survival Extrapolation Analyses for Predictive Economic Modeling (Independent Models)
#'
#' This is a wrapper function used to create agreed upon standard analysis. The 
#' wrapper leverages on the verb layer of this package.
#' 
#' 
#'   1.  Goodness of fit (id_analysis= 'e0stdflex0gof')
#'       - table: one rtf, one csv and one RData file (suffix = e0stdflex0gof)
#'   2.  Parametric and Spline Plots with KM data long term (id_analysis= 'e0stdflex0fig0timelong')
#'       - plot: one rtf, one png and one RData file (suffix = e0stdflex0fig0timelong)
#'   3.  Parametric and Spline Plots with KM data short term (id_analysis= 'e0stdflex0fig0timeshort')
#'       - plot: one rtf, one png and one RData file (suffix = e0stdflex0fig0timeshort)
#'
#' Important information:
#' - time variable is assumed to be available in days in observation_from data
#' - RData and csv files will be available in outdata_path folder, figures related
#' files (.png, .rtf) in outgraph_path folder and table related outputs (.rtf) in
#' outtable_path folder
#' - consistency between input parameters is the responsibility of the user
#'
#' @section Specification:
#' \if{latex}{ hta_flexsurv is a wrapper function within mkanr to implement two 
#' standard parametric models within a proportional hazard’s assumption (Exponential 
#' and Gompertz) and five standard parametric models within an accelerated failure 
#' time (AFT) framework (Weibull, Log-Logistic, Log-normal, Generalised Gamma 
#' and Gamma). Further, the function will also allow the implementation of spline 
#' on probit, odds and hazard scales. 
#' The function will rely on the parametrisation schema included within the flexsurv 
#' package (\url{https://cran.r-project.org/web/packages/flexsurv/vignettes/distributions.pdf}) 
#' to implement the standard parametric models.
#' 
#' Logical Specifications:
#' \enumerate{
#' \item	Please ensure that the treatment arm for which the models are to be 
#' fit is filtered in and the remaining arm(s) are filtered out. As no covariate 
#' is involved, the survival object is predicted by "~ 1".
#' \item \strong{Flexsurvreg} and \strong{Flexsurvspline} are recommended for 
#' implementing the parametric and spline models, respectively.
#' \item The following distribution names are to be specific in flexsurv for the 
#' respective models:
#'    \itemize{
#'      \item For implementing the exponential models, choose "exp" flexsurv distribution. 
#'      Next, extract the log(rate) parameter.
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="exp")}
#'      \item For implementing an AFT type of Weibull model, choose "weibull" 
#'      distribution and extract the log(shape) and log(scale) from the res.t object.
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="weibull")}
#'      \item For implementing the Gompertz model, choose the "gompertz" 
#'      distribution and extract the shape and log(rate) parameters.
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="gompertz")}
#'      \item For applying the Log-logistic model, choose the "llogis" distribution 
#'      in flexsurv and extract log(shape) and log(scale).
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="llogis")}
#'      \item For applying the Log-normal models, choose the "lnorm" distribution 
#'      and extract meanlog and log(sdlog).
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="lnorm")}
#'      \item For applying the generalised gamma model, choose the "gengamma" 
#'      distribution and extract mu, log(sigma) and Q parameters. 
#'      \cr Example: \code{flexsurvreg(Surv(time, event) ~ 1, data = data, dist="gengamma")}
#'      }
#' }
#' 
#' Functional Specifications:
#'   \enumerate{
#'   \item Check wrapper level input parameters using \code{\link{bullet_proof}}
#'   \item Preprocess data for analysis with \code{\link{preprocess_data}}
#'         \describe{
#'         \item{note 1}{data named tte for future reference}
#'          }  
#'   \itemize{
#'         \item population_from = \strong{population_from}
#'         \item population_where = \strong{population_where}
#'         \item observation_from = \strong{observation_from}
#'         \item observation_where = \strong{observation_where}
#'         \item analysis_variable = list(name=paste0(\strong{time},"99"),
#'         derivation=paste0(\strong{time},"/", switch( \strong{time_unit}, "D"="1", "W" = "7",
#'          "M" = "30.4367", "Y" = "365.24" )),label='Analysis Variable 99')
#'         \item analysis_variable_unit = list(name=paste0(\strong{time},"99U"),
#'         derivation=paste0("'",switch( \strong{time_unit}, "D"="DAYS", "W" = "WEEKS",
#'          "M" = "MONTHS", "Y" = "YEARS" ),"'"),label='Analysis Variable 99 Unit')
#'         \item therapy_data_var=\strong{therapy_data_var}
#'         }
#'   \item Run models
#'     \enumerate{
#'              \item Run flexsurvreg models with \code{\link{run_models_flexsurvreg}}
#'                \itemize{
#'                    \item preprocessed_data=tte
#'                    \item surv_formula = eval(parse(text=paste0("stats::formula
#'                                    (survival::Surv(",\strong{time},"99, 1-CNSR) ~ 1))))
#'                    \item dist_list = \strong{parametric_list}
#'                    \item na.action=na.omit
#'                    }
#'              \item Run flexsurvspline models with \code{\link{run_models_flexsurvspline}}
#'                \itemize{
#'                    \item preprocessed_data=tte
#'                    \item surv_formula = eval(parse(text=paste0("stats::formula
#'                                    (survival::Surv(",\strong{time},"99, 1-CNSR) ~ 1))))
#'                    \item scalexk_list = \strong{spline_list}
#'                    \item na.action=na.omit
#'                    \item seed_value = 7902
#'                    \item method="Nelder-Mead"
#'                    }
#'              \item fits = append parametric and spline models
#'              }
#'   \item Generate analysis based on \strong{id_analysis}
#'     \enumerate{
#'     \item Goodness of fit table (\strong{id_analysis}= 'e0stdflex0gof')
#'           note: generates 1 rtf, 1 RData file and 1 csv file
#'       \enumerate{
#'              \item Extract gof from fits with \code{\link{extract_gof}}
#'                \itemize{
#'                    \item fits= fits
#'                    \item preprocessed_data=tte
#'                    }
#'              \item Format gof from step above with \code{\link{format_gof}}
#'                \itemize{
#'                    \item gof= result from step above
#'                    \item gof_arrange=NULL,
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0gof")
#'                    }
#'              \item Convert to rtf with \code{\link{rdata_to_rtf}}
#'                \itemize{
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0gof")
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item outtable_path = paste0(\strong{outtable_path}, "/", \strong{output_folder_name})
#'                    \item other rtf formatting come from \strong{rtf_meta} parameter
#'                    }
#'              \figure{survce0onearm0e0stdflex0gof.jpg}
#'              }
#'     \item Short Term Extrapolation Plot (\strong{id_analysis}= 'e0stdflex0fig0timeshort')
#'           note: generates 1 rtf, 1 png file and 1 RData file
#'       \enumerate{
#'              \item Extract estimates from fits with \code{\link{extract_est}}
#'                \itemize{
#'                    \item fits= fits
#'                    \item t= 0:max(tte$paste0(\strong{time},"99"))
#'                    \item ci= FALSE
#'                    \item se= FALSE
#'                    \item B= 1000
#'                    \item cl= 0.95
#'                    \item na.action = na.omit
#'                    }
#'              \item Format estimates from step above with \code{\link{format_est}}
#'                \itemize{
#'                    \item est= result from step above
#'                    \item preprocessed_data= tte
#'                    \item preprocessed_data_km= tte
#'                    \item break_point=0
#'                    \item surv_formula = eval(parse(text=paste0("stats::formula
#'                                    (survival::Surv(",\strong{time},"99, 1-CNSR) ~ 1))))
#'                    \item type= "fleming"
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timeshort")
#'                    }
#'              \item Convert to png with \code{\link{plot_smooth_line}}
#'                \itemize{
#'                    \item data= result from step above
#'                    \item xlab= paste0("Time in ",switch(\strong{time_unit},"W"="Weeks","D"="Days",
#'                      "M"="Months","Y"="Years"))
#'                    \item ylab= "Survival"
#'                    \item xvar= "x_primary"
#'                    \item yvar= "y_primary"
#'                    \item levelvar= "group_primary"
#'                    \item break_point= NULL
#'                    \item levelvar_legend = "Type"
#'                    \item fig_scale_x_continuous="scale_x_continuous(expand=c(0,0))"
#'                    \item fig_scale_y_continuous="scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2), limits=c(0,1))"
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timeshort")
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item outgraph_path = paste0(\strong{outgraph_path}, "/", \strong{output_folder_name})
#'                    \item other rtf formatting come from \strong{rtf_meta} parameter
#'                    }
#'              \item Convert to rtf with \code{\link{png_to_rtf}}
#'                \itemize{
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timeshort")
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item outgraph_path = paste0(\strong{outgraph_path}, "/", \strong{output_folder_name})
#'                    \item other rtf formatting come from \strong{rtf_meta} parameter
#'                    }
#'              \figure{survce0onearm0e0stdflex0fig0timeshort.jpg}
#'              }
#'    \item Long Term Extrapolation Plot (\strong{id_analysis}= 'e0stdflex0fig0timelong')
#'           note: generates 1 rtf, 1 png file and 1 RData file
#'       \enumerate{
#'              \item Extract estimates from fits with \code{\link{extract_est}}
#'                \itemize{
#'                    \item fits= fits
#'                    \item t= \strong{predict_time}
#'                    \item ci= FALSE
#'                    \item se= FALSE
#'                    \item B= 1000
#'                    \item cl= 0.95
#'                    \item na.action = na.omit
#'                    }
#'              \item Format estimates from step above with \code{\link{format_est}}
#'                \itemize{
#'                    \item est= result from step above
#'                    \item preprocessed_data= tte
#'                    \item preprocessed_data_km= tte
#'                    \item break_point=0
#'                    \item surv_formula = eval(parse(text=paste0("stats::formula
#'                                    (survival::Surv(",\strong{time},"99, 1-CNSR) ~ 1))))
#'                    \item type= "fleming"
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timelong")
#'                    }
#'              \item Convert to png with \code{\link{plot_smooth_line}}
#'                \itemize{
#'                    \item data= result from step above
#'                    \item xlab= paste0("Time in ",switch(\strong{time_unit},"W"="Weeks","D"="Days",
#'                      "M"="Months","Y"="Years"))
#'                    \item ylab= "Survival"
#'                    \item xvar= "x_primary"
#'                    \item yvar= "y_primary"
#'                    \item levelvar= "group_primary"
#'                    \item break_point= NULL
#'                    \item levelvar_legend = "Type"
#'                    \item fig_scale_x_continuous="scale_x_continuous(expand=c(0,0))"
#'                    \item fig_scale_y_continuous="scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2), limits=c(0,1))"
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timelong")
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item outgraph_path = paste0(\strong{outgraph_path}, "/", \strong{output_folder_name})
#'                    \item other rtf formatting come from \strong{rtf_meta} parameter
#'                    }
#'              \item Convert to rtf with \code{\link{png_to_rtf}}
#'                \itemize{
#'                    \item rename_output=paste0(\strong{rtf_prefix}, "e0stdflex0fig0timelong")
#'                    \item outdata_path = paste0(\strong{outdata_path}, "/", \strong{output_folder_name})
#'                    \item outgraph_path = paste0(\strong{outgraph_path}, "/", \strong{output_folder_name})
#'                    \item other rtf formatting come from \strong{rtf_meta} parameter
#'                    }
#'              \figure{survce0onearm0e0stdflex0fig0timelong.jpg}
#'              }
#' }}}
#' 
#' \if{html}{
#' The contents of this section are shown in PDF user manual only.
#' }
#'
#' @param population_from path to population SAS data or data.frame
#' @param population_where quosure or logical vector for where clause
#' @param observation_from path to observation SAS data or data.frame
#' @param observation_where quosure or logical vector for where clause
#' @param therapy_data_var {`list`}\cr therapy variable factor metadata with elements:
#' \tabular{lll}{ \code{name} \tab {`character`} \tab  variable name to convert \cr 
#'                 \code{levels} \tab {`character`} \tab preferred order of values taken by variable \cr 
#'                 \code{labels} \tab {`character`} \tab preferred label of values taken by variable \cr
#'                 \code{name_label} \tab {`character`} \tab variable label \cr}
#'                 if levels and labels are set to NULL, sort(unique(name)) will be
#'                 used for both
#'                 see [base::factor()] for details
#' @param time {`character`}\cr variable name with time information 
#' @param time_unit {`character`}\cr run analysis in days('D'), weeks ('W'), months ('M') or years ('Y')
#' @param parametric_list {`list`}\cr named list of distributions of parametric model(s)
#'                to include in analysis 
#'                see dist parameter in flexsurv::flesurvreg for details
#' @param spline_list {`list`}\cr named list of lists of scale, k and knots parameters
#'                of spline models to include in analysis
#'                see scale, k and knots parameters in flexsurv::flexsurvspline for details
#' @param predict_time {`numeric`}\cr time horizon for the extrapolated plot
#' @param rtf_meta {`list`}\cr List of lists with parameters used to format rtf
#'                              outputs as per \code{\link{fct_rtf_meta}} function 
#' @param rtf_prefix {`character`}\cr prefix for all output names
#' @param id_analysis {`character`}\cr vector of request ids to include
#' @param outdata_path {`character`}\cr directory for .RData /.csv files
#' @param outgraph_path {`character`}\cr directory for .png/.rtf figures
#' @param outtable_path {`character`}\cr directory for .rtf tables
#'
#'
#' @return character vector with paths of all .rtf files created
#'
#' @importFrom stringr str_detect
#' @import dplyr
#' @import glue
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' tmp_dir <- tempdir()
#' tmp_dir_all <- file.path(tmp_dir, "all")
#' dir.create(tmp_dir_all, recursive = TRUE)
#'
#' hta_flexsurv(population_from = mkanr::adsl,
#' population_where = (ITTFL == "Y" & TRT01P=="B: Placebo"),
#' observation_from = mkanr::adtte,
#' observation_where = (PARAMCD == "OS"),
#' therapy_data_var = list(name="TRT01P",name_label="Planned 
#' Treatment for Period 01",levels=NULL,labels=NULL),
#' time = "AVAL",
#' time_unit = "W",
#' parametric_list = list(`Exponential`="exp",
#' `Weibull`="weibull",
#' `Log-normal`="lognormal", 
#' `Log-logistic`="llogis",
#' `Gompertz`="gompertz", 
#' `Gamma`="gamma", 
#' `Generalized Gamma`="gengamma"),
#' spline_list = list(`Spline on hazard - 1 knot`=list(scale="hazard",k=1),
#' `Spline on odds - 2 knots`=list(scale="odds",k=2),
#' `Spline on normal - 1 knots`=list(scale="normal",k=1)),
#' redict_time = 0:1040,
#' id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
#' rtf_prefix = "survce0os0arm10",
#' rtf_meta = fct_rtf_meta(output_suffix=list("all"),
#' rtf_title_pop=c("Subtitle 1", "(Population)"),
#' rtf_footnote_pop=c("Database Cutoff Date: XXX"),
#'  param="Overall Survival",
#'  therapy_arm=levels(tte[[therapy_data_var$name]]),
#'  output_suffix_update = list(),
#'  rtf_param=list(),
#'  models_title="Standard Parametric and Flexible Parametric Spline",
#'  modified_value=list()),
#'  outdata_path = tempdir(),
#'  outgraph_path = tempdir(),
#'  outtable_path = tempdir()
#' )
#' }
hta_flexsurv <- function(population_from ,
                               population_where = NULL,
                               observation_from ,
                               observation_where = NULL,
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
                               rtf_meta=fct_rtf_meta(),
                               rtf_prefix = paste0("survce0", "0", "os", "0"),
                               id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'),
                               outdata_path = tempdir(),
                               outgraph_path = tempdir(),
                               outtable_path = tempdir()) {
  
  # Bulletproofing ----
  bullet_proof(outdata_path=outdata_path,
               outgraph_path=outgraph_path,
               outtable_path=outtable_path,
               id_analysis=id_analysis,
               rtf_prefix = rtf_prefix,
               predict_time = predict_time,
               parametric_list=parametric_list,
               spline_list=spline_list,
               time_unit = time_unit,
               time = time)
  
  # rtf_path character vector ----
  rtf_paths <- c()
  
  # Data ----
  ## Preprocess data for piece-wise analysis ----
  tte <- preprocess_data(
    population_from = population_from,
    population_where = !!enquo(population_where),
    observation_from = observation_from,
    observation_where = !!enquo(observation_where),
    analysis_variable = list(name=paste0(time,"99"),
                             derivation=paste0(time,"/", switch( time_unit, "D"="1", "W" = "7", "M" = "30.4367", "Y" = "365.24" )),
                             label='Analysis Variable 2'),
    analysis_variable_unit = list(name=paste0(time,"2U"),
                                  derivation=paste0("'",switch( time_unit, "D"="DAYS", "W" = "WEEKS", "M" = "MONTHS", "Y" = "YEARS" ),"'"),
                                  label='Analysis Variable 2 Unit'),
    therapy_data_var=therapy_data_var)
  
  # Check data has only 1 arm (requirement for subsequent analysis) ----
  arm<-unique(tte %>% dplyr::pull(get(therapy_data_var$name)))
  if (!(length(arm) == 1)) stop("hta_flexsurv : analysis requires only one arm for therapy_data_var in data created by the combination of population_from, population_where, observation_from and observation_where")
  
  # Create list of RTF formats for the outputs
  rtf_meta_list <- eval(substitute(rtf_meta))
  
  # Modelling ----
  ## Create formula for analysis ----
  text_formula=paste0("stats::formula(survival::Surv(",time,"99, 1-CNSR) ~ 1)")
  
  ## Run parametric models ----
  if(length(parametric_list)>0){
    reg_models<-run_models_flexsurvreg(preprocessed_data=tte,
                                       surv_formula = eval(parse(text=text_formula)),
                                       dist_list = parametric_list,
                                       na.action=na.omit)
  }else{
    reg_models<-list()
  }
  
  # Run spline models ----
  if(length(spline_list)>0){
    spline_models<-run_models_flexsurvspline(preprocessed_data=tte,
                                             surv_formula = eval(parse(text=text_formula)),
                                             scalexk_list = spline_list,
                                             seed_value = 7902,
                                             na.action=na.omit,
                                             method = "Nelder-Mead")
    spline_suffix=sapply(spline_models,function(x) if(all(is.null(names(x$knots)))) {"*"}else{""})
    names(spline_models)<-paste0(names(spline_models),spline_suffix)
    if(any(spline_suffix=="*")){
      footnote_custom_spline='* The knot location of these survival models is specified manually.'
    }else{
      footnote_custom_spline=NULL
    }
  }else{
    spline_models<-list()
    footnote_custom_spline=NULL
  }
  
  ## Append both parameteric and spline models ----
  models<-append(reg_models,spline_models)
  
  # Goodness of fit ----
  if ('e0stdflex0gof' %in% id_analysis) {
    ## Extract from fit ----
    gof_data<-extract_gof(fits=models,
                          preprocessed_data=tte,
                          custom_model_label=rtf_meta_list$e0stdflex0gof$custom_model_label)
    
    ## Format ----
    e0stdflex0gof<-format_gof(gof=gof_data,
                              gof_arrange=NULL,
                              outdata_path = outdata_path,
                              rename_output=paste0(rtf_prefix, "e0stdflex0gof"))
    
    ## Convert to rtf ----
    rtf_paths <- c(
      rtf_paths,
      rdata_to_rtf(
        rename_output = paste0(rtf_prefix, "e0stdflex0gof"),
        rtf_title = rtf_meta_list$e0stdflex0gof$rtf_title,
        rtf_footnote = append(rtf_meta_list$e0stdflex0gof$rtf_footnote,footnote_custom_spline,length(rtf_meta_list$e0stdflex0gof$rtf_footnote)-1),
        header_label = rtf_meta_list$e0stdflex0gof$header_label,
        rel_col_widths = rtf_meta_list$e0stdflex0gof$rel_col_widths,
        col_just = rtf_meta_list$e0stdflex0gof$col_just,
        col_format = rtf_meta_list$e0stdflex0gof$col_format,
        col_digits = rtf_meta_list$e0stdflex0gof$col_digits,
        orientation = rtf_meta_list$e0stdflex0gof$orientation,
        csv_output = rtf_meta_list$e0stdflex0gof$csv_output,
        outdata_path = outdata_path,
        outtable_path = outtable_path
      )
    )
  }
  
  # Short and long term extrapolation plots ----
  if ('e0stdflex0fig0timeshort' %in% id_analysis) {
    ## Short term extrapolation ----
    ### Extract from fit ----
    extrap_est<-extract_est(fits=models,
                            t=0:max(tte[paste0(time,"99")]),
                            ci=FALSE,
                            se=FALSE,
                            B=1000,
                            cl = 0.95,
                            na.action = na.omit,
                            custom_model_label=rtf_meta_list$e0stdflex0fig0timeshort$custom_model_label)
    
    ### Format ----
    ready_to_plot<-format_est(est=extrap_est,
                              preprocessed_data=tte,
                              preprocessed_data_km=tte,
                              surv_formula=eval(parse(text=text_formula)),
                              type="fleming",
                              rename_output=paste0(rtf_prefix, "e0stdflex0fig0timeshort"),
                              outdata_path=outdata_path)
    
    ### Convert to png ----
    e0stdflex0fig0timeshort<-plot_smooth_line(
      data = ready_to_plot,
      xlab = paste0("Time in ",switch(time_unit,"W"="Weeks","D"="Days","M"="Months","Y"="Years")), ylab = "Survival",
      xvar = "x_primary",
      yvar = "y_primary",
      levelvar = "group_primary",
      break_point= NULL,
      rename_output = paste0(rtf_prefix, "e0stdflex0fig0timeshort"),
      outgraph_path = outgraph_path,
      outdata_path = outdata_path,
      fig_scale_x_continuous="scale_x_continuous(expand=c(0,0))",
      fig_scale_y_continuous="scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2), limits=c(0,1))",
      fig_res = 300,
      fig_height = 4,
      fig_width = 6.6,
      fig_scale = 1.3,
      levelvar_legend = "Type",
      legend_textsize = 10,
      legend_position = "bottom"
    )
    
    ### Convert to rtf ----
    rtf_paths <- c(
      rtf_paths,
      png_to_rtf(
        outgraph_path = outgraph_path,
        rename_output = paste0(rtf_prefix, "e0stdflex0fig0timeshort"),
        rtf_title  = rtf_meta_list$e0stdflex0fig0timeshort$rtf_title,
        rtf_footnote = append(rtf_meta_list$e0stdflex0fig0timeshort$rtf_footnote,footnote_custom_spline,length(rtf_meta_list$e0stdflex0fig0timeshort$rtf_footnote)-1),
        fig_height = rtf_meta_list$e0stdflex0fig0timeshort$fig_height,
        fig_width = rtf_meta_list$e0stdflex0fig0timeshort$fig_width,
        orientation = rtf_meta_list$e0stdflex0fig0timeshort$orientation
      )
    )
  }
  
  if ('e0stdflex0fig0timelong' %in% id_analysis) {
    ## Long term extrapolation ----
    ### Extract from fit ----
    extrap_est<-extract_est(fits=models,
                            t=predict_time,
                            ci=FALSE,
                            se=FALSE,
                            B=1000,
                            cl = 0.95,
                            na.action = na.omit,
                            custom_model_label=rtf_meta_list$e0stdflex0fig0timelong$custom_model_label)
    
    ### Format ----
    ready_to_plot_long<-format_est(est=extrap_est,
                                   preprocessed_data=tte,
                                   preprocessed_data_km=tte,
                                   surv_formula=eval(parse(text=text_formula)),
                                   type="fleming",
                                   rename_output=paste0(rtf_prefix, "e0stdflex0fig0timelong"),
                                   outdata_path=outdata_path)
    
    ### Convert to png ----
    e0stdflex0fig0timelong<-plot_smooth_line(
      data = ready_to_plot_long,
      xlab = paste0("Time in ",switch(time_unit,"W"="Weeks","D"="Days","M"="Months","Y"="Years")), ylab = "Survival",
      xvar = "x_primary",
      yvar = "y_primary",
      levelvar = "group_primary",
      break_point=NULL,
      rename_output = paste0(rtf_prefix, "e0stdflex0fig0timelong"),
      outgraph_path = outgraph_path,
      outdata_path = outdata_path,
      fig_scale_x_continuous="scale_x_continuous(expand=c(0,0))",
      fig_scale_y_continuous="scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.2), limits=c(0,1))",
      fig_res = 300,
      fig_height = 4,
      fig_width = 6.6,
      fig_scale = 1.3,
      levelvar_legend = "Type",
      legend_textsize = 10,
      legend_position = "bottom"
    )
    
    ### Convert to rtf ----
    rtf_paths <- c(
      rtf_paths,
      png_to_rtf(
        outgraph_path = outgraph_path,
        rename_output = paste0(rtf_prefix, "e0stdflex0fig0timelong"),
        rtf_title  = rtf_meta_list$e0stdflex0fig0timelong$rtf_title,
        rtf_footnote = append(rtf_meta_list$e0stdflex0fig0timelong$rtf_footnote,footnote_custom_spline,length(rtf_meta_list$e0stdflex0fig0timelong$rtf_footnote)-1),
        fig_height = rtf_meta_list$e0stdflex0fig0timelong$fig_height,
        fig_width = rtf_meta_list$e0stdflex0fig0timelong$fig_width,
        orientation = rtf_meta_list$e0stdflex0fig0timelong$orientation
      )
    )
  }
  
  #Return paths to rtf created ----
  return(rtf_paths)
  
}