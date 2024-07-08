# mkanr

## Objective

`mkanr` is a demo R package showcasing the layered package design and has been created in aid of presentation on 'Layered Design for R Package Development: Meeting the Needs of Pharmaceutical R&D Stakeholders' for UseR! conference, Saltzburg, Austria, 2024. This package uses extrapolation of survival probabilities analysis, based on flexsurv package, as an example. For more details please visit: https://userconf2024.sched.com/?iframe=yes&w=100%&sidebar=yes&bg=no#

## Installation

Install the development version from GitHub with:

```{r, eval=FALSE}
remotes::install_github("MSDllcpapers/mkanr")
```

## Use cases

The package is designed to be used in two layers: verb and wrapper. The verb layer is designed to be used by users in need of advanced functionalities while the wrapper layer is designed to generate the pre-defined standard with minimal user input.

### Example 1: Goodness of Fit Table, Short and Long-term Survival Probability

### Wrapper layer
```{r}
hta_flexsurv(population_from = mkanr::adsl,
             population_where = (ITTFL == "Y" & TRT01P=="A: Drug X"),
             observation_from = mkanr::adtte,
             observation_where = (PARAMCD == "OS"),
             id_analysis=c('e0stdflex0gof','e0stdflex0fig0timelong','e0stdflex0fig0timeshort'))
```

### Example 2: Verb layer

```{r}
# Process data
data<-mkanr:::preprocess_data(population_from = mkanr::adsl,
                       population_where = ITTFL == "Y" & TRT01P == "A: Drug X",
                       observation_from = mkanr::adtte,
                       observation_where = PARAMCD == "OS")
                       
# Goodness of Fit Table
data |>
  mkanr:::run_models_flexsurvreg()|> 
  mkanr:::extract_gof()|>
  mkanr:::format_gof()

# Short-term Survival Probability
data |>
  mkanr:::run_models_flexsurvreg()|> 
  mkanr:::extract_est()|>
  mkanr:::format_est()|>
  mkanr:::plot_smooth_line()

# Long-term Survival Probability
data |>
  mkanr:::run_models_flexsurvreg()|> 
  mkanr:::extract_est(t=1:1040)|>
  mkanr:::format_est()|>
  mkanr:::plot_smooth_line()

```
