if (FALSE) { #preventing code from running when calling devtools::load_all()
  library(FIMS)

## Code to force usethis to recognize dir as a package 
## for `usethis::use_vignette()`
# Clear any cached project info
options(usethis.quiet = FALSE)

# Try setting the project again with more verbose output
usethis::proj_set(".", force = TRUE)

# clear memory
clear()
## Testing how to remove one year of data and run multiple FIMS models
# Load sample data
data("data1")
# Prepare data for FIMS model
data_4_model <- FIMSFrame(data1)

# Define fleet specifications
fleet1 <- list(
  selectivity = list(form = "LogisticSelectivity"),
  data_distribution = c(
    Landings = "DlnormDistribution",
    AgeComp = "DmultinomDistribution",
    LengthComp = "DmultinomDistribution"
  )
)
survey1 <- list(
  selectivity = list(form = "LogisticSelectivity"),
  data_distribution = c(
    Index = "DlnormDistribution",
    AgeComp = "DmultinomDistribution",
    LengthComp = "DmultinomDistribution"
  )
)

# Create parameters
parameters <- data_4_model |>
  create_default_configurations() |>
  create_default_parameters(data = data_4_model)

# Run the  model with optimization
base_model <- parameters |>
  initialize_fims(data = data_4_model) |>
  fit_fims(optimize = TRUE)


# load all functions
devtools::load_all()

like_fit <- run_fims_likelihood(
  model = base_model,
  parameters = parameters,
  data = data1,
  n_cores = 5,
  min = -1,
  max = 1,
  length = 5
)

plot_likelihood(like_fit)

# Clear memory post-run
clear()

# remove one year of data and create new dataframe
# need to keep catch and weight-at-age data, just remove indices, length comp, and age comps
data2 <- data1 |>
            dplyr::filter(
                !(type %in% c("index", "age", "length", "age-to-length-conversion")) |
                dateend <= max(dateend) - lubridate::years(years_to_remove) #TODO: change dateend to timing and change the years to remove
            )

# Check that run_fims_model() can use same parameters as base model and produce the same output
retro_fit <- run_fims_retrospective(
  years_to_remove = 0:2, 
  data = data1, 
  parameters = parameters, 
  n_cores = 3
  )

library(ggplot2)
retro_fit[["estimates"]] |>
dplyr::filter(label == "spawning_biomass") |> 
dplyr::select(label, year_i, estimated, retro_year) |>
dplyr::group_by(label, retro_year) |>
dplyr::filter(dplyr::row_number() <= dplyr::n() - retro_year) |>
dplyr::ungroup() |> 
ggplot(aes(x = year_i, y = estimated, group = as.factor(retro_year))) +
geom_line(aes(color = as.factor(retro_year)), linewidth = 1.2) +
stockplotr::theme_noaa(discrete = T)

# to debug for json errors: 
#Error:   i In index: 1.
#Caused by error:
#! lexical error: invalid string in json text.
                      #  "uncertainty": nan,
                      #  (right here) ----^
debug(FIMSFit)
    fit <- parameters |>
        initialize_fims(data = data_model) |>
        fit_fims(optimize = TRUE)
# keep hitting n through the browser steps until you get to 
# 459: json_estimates <- reshape_json_estimates(model_output)
# then before going to the next step (n), run `write(model_output, file = "output.json")`

debug(get_estimates)


# new code on dev branch 2025-09-16
# depends on running the vignette first to get length_only_fit
run_fims_likelihood(
  model = length_only_fit,
  parameters = parameters_4_model,
  data = data_4_model
  )


}