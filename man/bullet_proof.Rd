% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_bullet_proof.R
\name{bullet_proof}
\alias{bullet_proof}
\title{Centralized bullet proofing}
\usage{
bullet_proof(...)
}
\arguments{
\item{...}{any test arguments to be bullet proofed}
}
\value{
NA
}
\description{
Checks the validity of input parameters
}
\section{Specification}{

\if{latex}{This function checks the validity of the parameters only for the
below given list. Input of any other parameters will not be checked.
  \enumerate{
  \item population_from
  \enumerate{
        \item should be type character and length 1
        \item file should exist (see file.exists())
        \item data should have required variables: USUBJID
        \item data should not be empty (load with haven::read_sas() or data())
        \item data unique key should be ('USUBJID')
        }
  \item observation_from
  \enumerate{
        \item should be type character and length 1
        \item file should exist (see file.exists())
        \item data should have required variables: USUBJID, CNSR, PARAMCD
        \item data should not be empty (load with haven::read_sas() or data())
        \item data unique key should be ('USUBJID', 'PARAMCD')
        }
  \item population_where
  \enumerate{
        \item population_from should also be provided
        \item check variable(s) used are available in data
        \item filtered data should not be empty
        }
  \item observation_where
  \enumerate{
        \item observation_from should also be provided
        \item check variable(s) used are available in data
        \item filtered data should not be empty
        }
  \item therapy_data_var
  \enumerate{
        \item should be type character and length 1
        \item if population_from provided, should exist in population_from
        \item if population_from provided, numeric equivalent (nameN) should exist in population_from
        }
  \item therapy_data_var_list
  \enumerate{
        \item should be list with elements: name, levels and labels
        \item name value should be type character and length 1
        \item levels values should be type character or numeric and length >=1
        \item labels values should be type character and length >=1
        \item levels and labels values should be the same length
        \item levels values should exist in population_from$name values
        }
  \item rename_output
  \enumerate{
        \item should be type character and length 1
        \item should only have lowercase letters and numbers
        }
  \item analysis_variable
  \enumerate{
        \item should be list with elements: name, derivation and label
        \item all values should be type character and length 1
        \item observation_from should be provided
        \item variables used in derivation should be available in data
        }
  \item analysis_variable_unit
  \enumerate{
        \item should be list with elements: name, derivation and label
        \item all values should be type character and length 1
        }
  \item preprocessed_data / preprocessed_data_km
  \enumerate{
        \item should be of type data.frame
        }
  \item surv_formula
  \enumerate{
        \item requires preprocessed_data or preprocessed_data_km to run tests
        \item should be class formula
        \item should have survival::Surv prefix used in left hand side
        \item variables used in right hand side should exist in data
        \item extract endpoint for subsequent tests: surv_formula[[2]][[2]]
        }
  \item outdata_path / outgraph_path /outtable_path
  \enumerate{
        \item should be of type character and length 1
        \item directory should exist (see dir.exists())
        }
  \item fits_flexsurv
  \enumerate{
        \item should be of type list and length >=1
        \item all elements should be named and class flexsurvreg
        }
  \item fits_survreg
  \enumerate{
        \item should be of type list and length >=1
        \item all elements should be named and class survreg
        }
  \item fits
  \enumerate{
        \item should be of type list and length >=1
        \item all elements should be named and class survreg or flexsurvreg
        }
  \item break_point
  \enumerate{
        \item should be type numeric, length 1 and greater or equal to 0
        \item if preprocessed_data_km, preprocessed_data and surv_formula provided
         \enumerate{
         \item endpoint extracted from surv_formula should exist
         \item endpoint should exist in both preprocessed_data and preprocessed_data_km
         \item break_point should be equal to max(preprocessed_data_km$endpoint) - max(preprocessed_data$endpoint)
        }
        }
  \item id_analysis
  \enumerate{
        \item should be type character and length greater or equal to 1
        \item should be subset of c('e0stdflex0gof','e0stdflex0fig0timeshort','e0stdflex0fig0timelong')
        }
 \item xlab / ylab /  col_just /  header_label /  levelvar /  levelvar_legend /  models_title /  palette /  rtf_footnote /  rtf_title /  rtf_title_pop /  rtf_footnote_pop /  therapy_arm /  treatment_label /  time_horizon /  xvar /  yvar
 \enumerate{
       \item should be type character and length greater or equal to 1
       }
\item facetvar
\enumerate{
       \item should be either null or type character and length greater or equal to 1
       }
\item fig_height  / fig_line_size /  fig_res /  fig_scale /  fig_width /  digits /  legend_textsize /  rel_col_widths /  seed_value /  predict_time
\enumerate{
       \item should be type numeric and length greater or equal to 1
       }
\item time_unit
\enumerate{
       \item should be type character and length greater or equal to 1 
       \item should be equal to 'd' for analysis in days, 'w' for analysis in weeks, 'm' for analysis in months or 'y' for analysis in years
       }
\item orientation
\enumerate{
       \item should be type character and length greater or equal to 1 
       \item should be either 'portrait' or 'landscape'
       }
\item legend_position
\enumerate{
       \item should be type character and length greater or equal to 1 
       \item should be either 'none', 'left', 'right', 'bottom' or 'top'
       }
\item gof_arrange
\enumerate{
       \item should be either null or type character and length greater or equal to 1 
       \item should be either 'rank aic', 'rank bic' or 'rank mean aic bic'
       }
\item custom_model_label /  csv_output /  ci /  se /  exp /  boundknots /  fig_grid_x /  fig_grid_y
\enumerate{
      \item should be equal to either true or false
       }
\item output_suffix_update / rtf_param /  scalexk_list
\enumerate{
       \item should be either a list or null
       }
\item modified_value / output_suffix
\enumerate{
      \item should be a list
       }
\item data
\enumerate{
       \item should be a data frame
      }
\item facetvar_legend
\enumerate{
       \item should be of class function (ggplot2::labeller) and length 1
       }
}

Logical Specifications:

\enumerate{
  \item Check input parameters have check available
  \item Run the first loop on the list of parameters especially on
  population_from and observation_from to check and capture the symbols and
  column names
  \item Check other expression element logic based on the name of the element
  \item Return message if all check were successful
}
}

\if{html}{
The contents of this section are shown in PDF user manual only.
}
}

