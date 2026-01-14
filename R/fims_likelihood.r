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
  # checking inputs
  if(length < 1 | as.integer(length) != length) {
      cli::cli_abort("Input length should be a positive integer ")
  }
  
  if(length > 50){
    cli::cli_warn("Input length is {length}, are you sure you want it so large?")
  }
  
  if (min >= max) {
    cli::cli_abort("Input min should be less than max.")
  }

  if(class(model) != "FIMSFit"){
    cli::cli_abort("Input model needs to be a FIMSFit object.")
  }
  # calculate vector
  values = seq(min, max, length = length)
  
  if (!0 %in% values) {
    cli::cli_warn("Inputs min and max don't span 0. Are you sure this is right?")
  }

  init <- FIMS::get_estimates(model) |> 
    dplyr::filter(.data[["label"]] == parameter_name) |> 
    dplyr::pull(.data[["estimated"]]) #NOTE: input and estimated value are slightly different (even though its fixed) input = 13.8155, estimated = 13.857

    if (!is.null(module_name)) {
    module_names <- parameters |> 
      tidyr::unnest(cols = data) |> 
      dplyr::pull(.data[["module_name"]]) |> 
      unique()
      if(!module_name %in% module_names){
        cli::cli_abort("Input module_name not found in parameters tibble.")
      }
    parameter_row <- parameters |> 
      tidyr::unnest(cols = data) |>
      dplyr::filter(.data[["module_name"]] == module_name & .data[["label"]] == parameter_name) 

  } else {
    parameter_row <- parameters |>
      tidyr::unnest(cols = data) |> 
      dplyr::filter(.data[["label"]] == parameter_name) 
  }

  if(nrow(parameter_row) == 0){
    cli::cli_abort("Input parameter_name did not match any rows in parameter tibble.")
  }  
  
  if(nrow(parameter_row) > 1){
    cli::cli_abort("Input parameter_name matched too many rows in parameter tibble: {length(parameter_row)}. Try adding a module_name.")
  }
  
  vec <- values + init
  # report the values
  cli::cli_alert_info(
    "parameter values being profiles over: {paste(vec, collapse = ', ')}"
  )

  # Set number of cores to use 
  if (is.null(n_cores)) {
    n_cores_to_use <- parallel::detectCores() - 1
  } else {
    # Validate n_cores before conversion
    if (!is.numeric(n_cores) || n_cores %% 1 != 0 || n_cores <= 0) {
      cli::cli_abort("n_cores must be a positive integer. Input was {n_cores}")
    }
    n_cores_to_use <- as.integer(n_cores)
  }
  dplyr::case_when (
    n_cores_to_use == 1 ~ future::plan(future::sequential),
    n_cores_to_use > 1 & Sys.info()['sysname'] == 'Windows' ~ future::plan(future::multisession, workers = n_cores_to_use),
    n_cores_to_use > 1 & Sys.info()['sysname'] != 'Windows' ~ future::plan(future::multicore, workers = n_cores_to_use)
  )

  if (n_cores_to_use == 1) {
    cli::cli_alert_info("...Running sequentially on a single core")
  } else {
    cli::cli_alert_info("...Running in parallel on {n_cores_to_use} cores")
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
      FIMS::get_estimates(fit)
    },
    parameter_name = parameter_name, 
    module_name = module_name,
    parameters = parameters,
    data = data,
    .options = furrr::furrr_options(seed = TRUE, globals = TRUE)
  )

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
