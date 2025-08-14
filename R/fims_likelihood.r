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
  init <- parameters$parameters$recruitment[[parameter_label]] #TODO: replace with parameter value from fit model
  vec <- values + init
  # report the values
  cli::cli_alert_info(
    "parameter values being profiles over: {paste(vec, collapse = ', ')}"
  )
  
  # run FIMS in parallel for each of the likelihood profile values
  fits <- furrr::future_map(
    .x = vec,
    .f = run_modified_fims,
    data = data,
    parameters = parameters
  )

# pull the fits tibble out of each of the FIMSFit S4 objects into a list
fits_list <- lapply(fits, function(fit) fit@fits)

# adding the fixed parameter value to the fits tibble for each of the models
for (i in seq_along(fits_list)) {
  fits_list[[i]]$profile_parameter_value <- vec[i]
}
# combine the separate tibbles in the list into one longer tibble
fits_df <- do.call(rbind, fits_list)

  return(list("vec" = vec, "fits" = fits_df))
}

if (FALSE) {
# call the function above
like_fit <- run_fims_likelihood(
  parameters = parameters,
  data = data1,
  min = -1,
  max = 1,
  length = 3
)

# summing total likelihood by parameter value, use later for plotting
 like_fit$fits |>
  dplyr::group_by(profile_parameter_value) |>
  dplyr::summarise(total_like = sum(log_like)) |> 
  dplyr::mutate(total_like_change = total_like - min(total_like))
  

}