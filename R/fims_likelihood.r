library(FIMS)

#' Run likelihood profile for a FIMS model
#'
#' This function runs a likelihood profile for a FIMS model by fixing a specified parameter at a range of values spanning the initial value.
#'
#' @param model Output from [estimate_fims()], currently only used to get the estimated value for log_rzero.
#' @param parameters A FIMS parameters object containing the model parameters.
#' @param data A dataframe or tibble containing the model data, or a FIMSFrame object.
#' @param parameter_name A string specifying the parameter to profile over, e.g., "log_rzero".
#' @param min The minimum value for the parameter profile relative to the initial value.
#' @param max The maximum value for the parameter profile relative to the initial value.
#' @param length The number of values to generate between `min` and `max`. An odd number is recommended to include the initial value.
#' @return A list containing the vector of parameter values and a dataframe with the estimates for each model.
#' @export

run_fims_likelihood <- function(
  model,
  parameters,
  data,
  module_name = NULL,
  parameter_name = "log_rzero",
  min = -2,
  max = 2,
  length = 5
  # TODO: check inputs to make sure they make sense
) {
  # calculate vector
  values = seq(min, max, length = length)

  #TODO: can now get this from get_estimates(model) |> dplyr::filter(label == parameter_name) |> pull(estimated)
  init <- parameters |>
    tidyr::unnest(cols = data) |>
    dplyr::filter(label == parameter_name) |>
    dplyr::pull(value)

    #TODO: update these to pull from get_estimates(model) as well
    if (!is.null(module_name)) {
    parameter_row <- parameters |> 
      tidyr::unnest(cols = data) |>
      dplyr::filter(.data$module_name == module_name & label == parameter_name) 

  } else {
    parameter_row <- parameters |>
      tidyr::unnest(cols = data) |> 
      dplyr::filter(label == parameter_name) 
  }

  vec <- values + init
  # report the values
  cli::cli_alert_info(
    "parameter values being profiles over: {paste(vec, collapse = ', ')}"
  )

  # Set number of cores to use 
  n_cores <- parallel::detectCores() - 1

  if (Sys.info()['sysname'] == 'Windows') {
        future::plan(future::multisession, workers = n_cores)
        message("...Running in parallel with multisession")
      } else {
        # Use multicore for Linux/macOS for better performance
        future::plan(future::multicore, workers = n_cores)
        message("...Running in parallel with multicore")
      }

  # Ensure cleanup happens 
  on.exit(future::plan(future::sequential), add = TRUE)

  # run FIMS in parallel for each of the likelihood profile values
  estimates <- furrr::future_map(
    .x = vec,
    .f = run_modified_pars_fims,
    parameter_name = parameter_name, 
    module_name = module_name,
    parameters = parameters,
    data = data
  )

# pull the estimates tibble out of each of the FIMSFit S4 objects into a list
estimates_list <- lapply(estimates, function(estimate) estimate@estimates) #TODO: change to get_estimates

# adding the fixed parameter value to the estimates tibble for each of the models
for (i in seq_along(estimates_list)) {
  # create a new column name based on the profile parameter
  # this could be extended to profile over multiple dimensions parameters
  colname <- paste0("value_", parameter_name)
  estimates_list[[i]][[colname]] <- vec[i]
}
# combine the separate tibbles in the list into one longer tibble
estimates_df <- do.call(rbind, estimates_list)

  return(list("vec" = vec, "estimates" = estimates_df))
}
