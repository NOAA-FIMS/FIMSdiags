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
  suppressPackageStartupMessages({
    require(FIMS, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)
    require(cli, quietly = TRUE)
  })

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
  parameter_row$estimation_type <- "constant"
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

#' Function to remove a given number of years of data and run FIMS model
#' This function is called by run_fims_retrospective()
#'
#' @param years_to_remove number of years to remove
#' @param data full dataset used in base model run
#' @param parameters input parameters used in base FIMS model
#' @return FIMS model fitted with years of data removed
#' @export

run_modified_data_fims <- function(years_to_remove = 0, data, parameters) {
    # Need to load packages for each worker for furrr functions
    require(FIMS, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(lubridate, quietly = TRUE)
    require(cli, quietly = TRUE)

    # check if the input is a FIMSframe object and if so, extract the data
    # this is to avoid the warning:
    #   no applicable method for 'filter' applied to an object of class "FIMSFrame"
    if ("FIMSFrame" %in% is(data)) {
        data <- data@data
    }

    # Remove years from data
    if (years_to_remove == 0) {
        data_mod <- data
    } else {
        data_mod <- data |>
            dplyr::filter(
                (type %in% c("age-to-length-conversion", "weight-at-age")) |
                    timing <= max(timing) - years_to_remove 
            )
    }
    # convert to FIMSFrame format
    data_model <- FIMSFrame(data_mod)

    # report the year removed being run
    cli::cli_alert_info(
        "running model with {paste(years_to_remove, collapse = ', ')} years of data removed"
    )

    #User supplies parameters from base model
    fit <- parameters |>
        initialize_fims(data = data_model) |>
        fit_fims(optimize = TRUE) #TODO: Error: parse error: after array element, I expect ',' or ']'
                                         #  "-999""uncertainty": [
                                         #  (right here) ------^
    
    return(fit)
}

