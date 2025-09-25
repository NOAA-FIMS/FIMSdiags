#' Modify a parameter input and run a FIMS model
#' This function is called by run_fims_likelihood()
#'
#' @param new_value The new value to be changed in the FIMS model.
#' @param parameter_name The name of the parameter value (as listed in parameters$label) to be modified
#' @param module_name The name of module associated with the parameter to be changed. Default is NULL.
#' @param parameters The tibble of input parameters for a FIMS model
#' @param data A dataframe of input data for FIMS model
#'
#' @return FIMS model fitted to the new parameter input value
#' @export

run_modified_pars_fims <- function(
  new_value, 
  parameter_name,
  module_name = NULL,
  parameters, #TODO: replace parameters and input with fitted FIMS model once tibbles are avaialble in FIMS fit object 
  data) {

  # Need to load packages for each worker for furrr functions
  require(FIMS, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(tidyr, quietly = TRUE)
  require(cli, quietly = TRUE)

 # if parameters is nested, then unnest
  if ("data" %in% names(parameters)) {
    parameters <- parameters |> tidyr::unnest(cols = data)
  }

  # find the parameter
  if (!is.null(module_name)) {
    parameter_row <- parameters |> 
      dplyr::filter(.data$module_name == module_name & label == parameter_name)
    if (nrow(parameter_row) == 0) {
      cli::cli_abort("Parameter with module name {module_name} and label {label} not found in parameters object")
    }
  } else {
    parameter_row <- parameters |> 
      dplyr::filter(label == parameter_name)
    if (nrow(parameter_row) == 0) {
      cli::cli_abort("Parameter with label {parameter_name} not found in parameters object")
    }
    if (nrow(parameter_row) > 1) {
      cli::cli_abort("Multiple parameters with label {parameter_name} found in parameters object, please specify module_name")
    }
  }

  # Update value
  parameter_row$value <- new_value 
  parameters_mod <- parameters |> 
    dplyr::rows_update(
      parameter_row,
      by = c("module_name", "label")
    )

  data_model <- FIMSFrame(data)

  new_fit <- parameters_mod |>
    initialize_fims(data = data_model) |>
    fit_fims(optimize = TRUE)

  clear()
  return(new_fit)
}
