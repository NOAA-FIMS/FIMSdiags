# FIMSdiags

[![R-CMD-check](https://github.com/NOAA-FIMS/FIMSdiags/actions/workflows/call-r-cmd-check.yml/badge.svg)](https://github.com/NOAA-FIMS/FIMSdiags/actions/workflows/call-r-cmd-check.yml)

FIMSdiags is a companion package to [FIMS](https://github.com/NOAA-FIMS/FIMS). It allows users to take a FIMS model and run common diagnostic analyses on their models. FIMSdiags is in active development and we are continuously adding features and improving current ones. Currently, the available diagnostics in the package are:

-   Retrospective analysis

-   Likelihood profile

## Learning FIMSdiags

Each FIMSdiags function has a vignette describing how to apply them to a FIMS model. These are available in the <a href="https://noaa-fims.github.io/FIMSdiags/articles/index.html" target="_blank">articles</a> section of the [FIMS github.io website](https://noaa-fims.github.io/FIMSdiags/).

The help documentation for each function is available within R via a command like `?FIMSdiags::run_fims_likelihood`.

## Installing FIMSdiags  

Install the most recent version of FIMSdiags using the following code:

``` R
install.packages("remotes")
remotes::install_github("NOAA-FIMS/FIMSdiags")
```
or using {renv}: 
```R
renv::install("NOAA-FIMS/FIMSdiags")
# Update lockfile once package is installed
renv::snapshot()
```
## Usage  

```R
library(FIMS)
library(FIMSdiags)

# Prepare data for FIMS model
data("data1")
data_4_model <- FIMSFrame(data1)

# Create parameters
parameters <- data_4_model |>
  create_default_configurations() |>
  create_default_parameters(data = data_4_model)

# Run the base model
base_model <- parameters |>
  initialize_fims(data = data_4_model) |>
  fit_fims(optimize = TRUE)

# Run a likelihood profile over R0 
like_fit <- run_fims_likelihood(
  model = base_model,
  parameters = parameters,
  data = data1,
  parameter_name = "log_rzero",
  n_cores = 3,
  min = -1,
  max = 1,
  length = 3
)

plot_likelihood(like_fit)
clear()

# Run a retrospective analysis for 5 years
retro_fit <- run_fims_retrospective(
  years_to_remove = 0:5, 
  data = data1, 
  parameters = parameters, 
  n_cores = 3
  )

plot_retrospective(retro_fit, quantity = "spawning_biomass")
clear()
```

## Getting Help 

If you encounter a bug, please open an [issue](https://github.com/NOAA-FIMS/FIMSdiags/issues) and provide a minimal reproducible example. If you have a question or an idea for future development, feel free to post to the [Discussion Board](https://github.com/orgs/NOAA-FIMS/discussions). We welcome all ideas and suggestions. 

## NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. section 105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)