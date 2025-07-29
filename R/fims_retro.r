library(FIMS)
library(tidyr)

## Testing how to remove one year of data and run multiple FIMS models
# Load sample data
data("data1")

# Function to prepare data and run FIMS model for a given number of years to remove
#' @param data full dataset used in base model run
#' @param years_to_remove vector of number of years to remove, 
#' i.e. if you want to do 5 retrospective peels 0:5
#' @param params input parameters used in base FIMS model 

run_fims_retro <- function(data, years_to_remove = 0, params) {
    # Remove years from data1
    if (years_to_remove == 0) {
        data_retro <- data
    } else {
        data_retro <- data |>
            dplyr::filter(
                !(type %in% c("index", "age", "length", "age-to-length-conversion")) |
                dateend <= max(dateend) - lubridate::years(years_to_remove)
            )
    }
    data_model <- FIMSFrame(data_retro)
 
    #User supplies parameters from base model
    fit <- params |>
        initialize_fims(data = data_model) |>
        fit_fims(optimize = TRUE)
    return(fit)
}

# Example: remove one year at a time and run 
fit0 <- run_fims_retro(data1, years_to_remove = 0, params = parameters)
fit1 <- run_fims_retro(data1, years_to_remove = 1, params = parameters)

# Example: run models removing 0, 1, and 2 years in parallel
years_to_remove <- 0:2
fits <- furrr::future_map(.x = years_to_remove, .f = run_fims_retro, data = data1)

# get the @estimates slot from each model and rbind them, adding an additional column for the year removed
estimates_list <- lapply(fits, function(fit) fit@estimates)
for (i in seq_along(estimates_list)) {
    estimates_list[[i]]$retro_year <- years_to_remove[i]
}
estimates_df <- do.call(rbind, estimates_list)

# plot the SSB time series from each model
# NOTE: this doesn't work because we don't have a year value.
#       there's a time column but it's empty
library(ggplot2)
ggplot(estimates_df, aes(x = year, y = estimate, color = retro_year)) +
    geom_line() +
    labs(title = "SSB Time Series by Year Removed",
         x = "Year",
         y = "SSB Estimate") +
    theme_minimal()


# get SSB time series from each model
# using head(10) because the range of years is different among models
SSBtable <- cbind(
    fits[[1]]@estimates |>
        dplyr::filter(label == "SSB") |>
        dplyr::pull(estimate) |> head(10),
    fits[[2]]@estimates |>
        dplyr::filter(label == "SSB") |>
        dplyr::pull(estimate) |> head(10),
    fits[[3]]@estimates |>
        dplyr::filter(label == "SSB") |>
        dplyr::pull(estimate) |> head(10)
)

# proof that values are different among models
SSBtable
#         [,1]     [,2]     [,3]
# SSB 6838.698 6825.748 6818.862
# SSB 6839.828 6827.363 6819.676
# SSB 6790.398 6778.632 6769.959
# SSB 6590.985 6580.053 6570.594
# SSB 6334.694 6324.161 6314.831
# SSB 6120.571 6110.624 6103.179
# SSB 6058.196 6047.160 6042.686
# SSB 5568.276 5556.572 5554.346
# SSB 4569.733 4557.078 4556.954
# SSB 4711.758 4698.478 4701.322