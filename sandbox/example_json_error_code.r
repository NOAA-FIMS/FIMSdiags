if(FALSE){
## Example code of running a FIMS model with 1 year missing of index, length, and age data

library(FIMS)
# clear memory
clear()
# Load sample data
data("data1")
# Set number of years of data to remove
years_to_remove = 2

# Prepare data for FIMS model
 data_mod <- data1 |>
            dplyr::filter(
                (type %in% c("age-to-length-conversion", "weight-at-age")) |
                    timing <= max(timing) - years_to_remove 
            )
# convert to FIMSFrame format
data_model <- FIMSFrame(data_mod)

# Create parameters
parameters <- data_model |>
  create_default_configurations() |>
  create_default_parameters(data = data_model)

# Fit model
fit <- parameters |>
        initialize_fims(data = data_model) |>
        fit_fims(optimize = TRUE)

}