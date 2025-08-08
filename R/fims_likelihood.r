library(FIMS)

#' Run likelihood profile for a FIMS model
#'
#' This function runs a likelihood profile for a FIMS model by fixing a specified parameter at a range of values spanning the initial value.
#'
#' @param parameters A FIMS parameters object containing the model parameters.
#' @param data A dataframe or tibble containing the model data, or a FIMSFrame object.
#' @param parameter_label NOT YET FUNCTIONAL. A string specifying the parameter to profile over, e.g., "BevertonHoltRecruitment.log_rzero.value".
#' @param min The minimum value for the parameter profile relative to the initial value.
#' @param max The maximum value for the parameter profile relative to the initial value.
#' @param length The number of values to generate between `min` and `max`. An odd number is recommended to include the initial value.
#' @return A list containing the vector of parameter values and the fitted models for each value. TODO: return a dataframe with the fits for each model.
#' @export

run_fims_likelihood <- function(
  parameters,
  data,
  parameter_label = "BevertonHoltRecruitment.log_rzero.value",
  min = -2,
  max = 2,
  length = 5
  # TODO: check inputs to make sure they make sense
) {
  # calculate vector
  values = seq(min, max, length = length)
  init <- parameters$parameters$recruitment[[parameter_label]]
  vec <- values + init
  # report the values
  cli::cli_alert_info(
    "parameter values being profiles over: {paste(vec, collapse = ', ')}"
  )
  run_modified_fims <- function(new_value, parameters, data) {
    parameters_mod <- parameters |>
      update_parameters(
        modified_parameters = list(
          # TODO: figure out how to specify a generic parameter
          recruitment = list(
            BevertonHoltRecruitment.log_rzero.value = new_value,
            BevertonHoltRecruitment.log_rzero.estimation_type = "constant"
          )
        )
      )
    data_model <- FIMSFrame(data)

    #TODO: replace parameters input with fitted FIMS model and use code below
    # to get final values of all parameters as a starting point
    #like_fit$fits[[1]]@obj$env$last.par.best
    #how to get parameter values with names: FIMS:::get_parameter_names(like_fit$fits[[1]]@obj$env$last.par.best)
    #data object will be avaialble later
    #like_fit$fits[[1]]@obj$env$data or #like_fit$fits[[1]]@obj$data

    new_fit <- parameters_mod |>
      initialize_fims(data = data_model) |>
      fit_fims(optimize = TRUE)
    return(new_fit)
  }

  fits <- furrr::future_map(
    .x = vec,
    .f = run_modified_fims,
    data = data,
    parameters = parameters
  )
  return(list("vec" = vec, "fits" = fits))
}

# call the function above
like_fit <- run_fims_likelihood(
  parameters = parameters,
  data = data1,
  min = -1,
  max = 1,
  length = 3
)

# like_fit[[1]]@fits
#TODO: add this inside the function and have it return fits_df
fits_list <- lapply(like_fit$fits, function(fit) fit@fits)
for (i in seq_along(fits_list)) {
  fits_list[[i]]$profile_parameter_value <- like_fit$vec[i]
}
fits_df <- do.call(rbind, fits_list)
head(fits_df)
table(fits_df$profile_parameter_value)

fits_df |>
  dplyr::group_by(profile_parameter_value) |>
  dplyr::summarise(total_like = sum(log_like))
