library(FIMS)

#' Run likelihood profile for a FIMS model
#'
#' This function runs a likelihood profile for a FIMS model by fixing a specified parameter at a range of values spanning the initial value.
#'
#' @param model Output from [fit_fims()], currently only used to get the estimated value for log_rzero.
#' @param parameters A FIMS parameters object containing the model parameters.
#' @param data A dataframe or tibble containing the model data, or a FIMSFrame object.
#' @param parameter_label NOT YET FUNCTIONAL. A string specifying the parameter to profile over, e.g., "BevertonHoltRecruitment.log_rzero.value".
#' @param min The minimum value for the parameter profile relative to the initial value.
#' @param max The maximum value for the parameter profile relative to the initial value.
#' @param length The number of values to generate between `min` and `max`. An odd number is recommended to include the initial value.
#' @return A list containing the vector of parameter values and the fitted models for each value. TODO: return a dataframe with the fits for each model.
#' @export

run_fims_likelihood <- function(
  model,
  parameters,
  data,
  module_name = NULL,
  parameter_label = "log_rzero",
  min = -2,
  max = 2,
  length = 5
  # TODO: check inputs to make sure they make sense
) {
  # calculate vector
  values = seq(min, max, length = length)

  init <- parameters |>
    tidyr::unnest(cols = data) |>
    dplyr::filter(label == parameter_label) |>
    dplyr::pull(value)

    if (!is.null(module_name)) {
    parameter_row <- parameters |> 
      tidyr::unnest(cols = data) |>
      dplyr::filter(.data$module_name == module_name & label == parameter_label) 

  } else {
    parameter_row <- parameters |>
      tidyr::unnest(cols = data) |> 
      dplyr::filter(label == parameter_label) 
  }

  vec <- values + init
  # report the values
  cli::cli_alert_info(
    "parameter values being profiles over: {paste(vec, collapse = ', ')}"
  )

  # Set number of cores to use 
  n_cores <- parallel::detectCores() - 1
  plan(multisession, workers = n_cores)
  # Ensure cleanup happens 
  on.exit(plan(sequential), add = TRUE)

  # run FIMS in parallel for each of the likelihood profile values
  fits <- furrr::future_map(
    .x = vec,
    .f = run_modified_fims,
    parameter_name = parameter_label, 
    module_name = module_name,
    parameters = parameters,
    data = data
  )

# pull the fits tibble out of each of the FIMSFit S4 objects into a list
fits_list <- lapply(fits, function(fit) fit@fits)

# adding the fixed parameter value to the fits tibble for each of the models
for (i in seq_along(fits_list)) {
  # create a new column name based on the profile parameter
  # this could be extended to profile over multiple dimensions parameters
  colname <- paste0("value_", parameter_label)
  fits_list[[i]][[colname]] <- vec[i]
}
# combine the separate tibbles in the list into one longer tibble
fits_df <- do.call(rbind, fits_list)

  return(list("vec" = vec, "fits" = fits_df))
}
