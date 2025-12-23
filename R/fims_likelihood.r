#' Run likelihood profile for a FIMS model
#'
#' This function runs a likelihood profile for a FIMS model by fixing a specified parameter at a range of values spanning the initial value.
#'
#' @param model Output from [fit_fims()], currently only used to get the estimated value for log_rzero.
#' @param parameters A FIMS parameters object containing the model parameters.
#' @param data A dataframe or tibble containing the model data, or a FIMSFrame object.
#' @param module_name A string specifying the module the parameter being profiled over is in. Default is NULL.
#' @param parameter_name A string specifying the parameter to profile over, e.g., "log_rzero".
#' @param n_cores The number of cores to use to run likelihood profile in parallel. Default is `parallel::detectCores() - 1`.
#' @param min The minimum value for the parameter profile relative to the initial value.
#' @param max The maximum value for the parameter profile relative to the initial value.
#' @param length The number of values to generate between `min` and `max`. An odd number is recommended to include the initial value.
#' @return A list containing the vector of parameter values and a dataframe with the estimates for each model.
#' @export
#' 
#' @examples 
#' \dontrun{
#'  library(FIMS)
#' # Use built-in dataset from FIMS
#'  data("data1")
#'  data_4_model <- FIMSFrame(data1)
#' # Create a parameters object
#'  parameters <- data_4_model |>
#'    create_default_configurations() |>
#'    create_default_parameters(data = data_4_model)
#' # Run the  model with optimization
#'  base_model <- parameters |>
#'    initialize_fims(data = data_4_model) |>
#'    fit_fims(optimize = TRUE)
#'  like_fit <- run_fims_likelihood(
#'    model = base_model,
#'    parameters = parameters,
#'    parameter_name = "log_rzero",
#'    data = data1,
#'    n_cores = 3,
#'    min = -1,
#'    max = 1,
#'    length = 3
#'   )
#'
#' }

run_fims_likelihood <- function(
  model,
  parameters,
  data,
  module_name = NULL,
  parameter_name = "log_rzero",
  n_cores = NULL,
  min = -2,
  max = 2,
  length = 5
  # TODO: check inputs to make sure they make sense
) {
  # calculate vector
  values = seq(min, max, length = length)

  init <- get_estimates(model) |> 
    dplyr::filter(label == parameter_name) |> 
    dplyr::pull(estimated) #NOTE: input and estimated value are slightly different (even though its fixed) input = 13.8155, estimated = 13.857

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
  if(is.null(n_cores)){
    n_cores <- parallel::detectCores() - 1
  }

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
  estimates_list <- furrr::future_map(
    .x = vec,
    .f = function(value, parameter_name, module_name, parameters, data) {
      # Run the model
      fit <- run_modified_pars_fims(
        new_value = value,
        parameter_name = parameter_name, 
        module_name = module_name,
        parameters = parameters,
        data = data
      )
      # Extract estimates immediately while still in worker
      get_estimates(fit)
    },
    parameter_name = parameter_name, 
    module_name = module_name,
    parameters = parameters,
    data = data,
    .options = furrr::furrr_options(seed = TRUE, globals = TRUE)
  )

  # pull the estimates tibble out of each of the FIMSFit S4 objects into a list
  # moved to inside the parallel function so R session doesn't crash
  #estimates_list <- purrr::map(estimates, get_estimates) 

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
