library(FIMS)

run_fims_likelihood <- function(parameters, data, parameter_label = "BevertonHoltRecruitment.log_rzero.value", min = -2, max = 2, length = 5
  # TODO: check inputs to make sure they make sense
  ) {
    # calculate vector
    values = seq(min, max, length = length)
    init <- parameters$parameters$recruitment[[parameter_label]]
    vec <- values + init
  # report the values
  cli::cli_alert_info("parameter values being profiles over: {paste(vec, collapse = ', ')}")
run_modified_fims <- function(new_value, parameters, data){
    parameters_mod <- parameters |> 
        update_parameters(modified_parameters = list(recruitment = 
        # TODO: figure out how to specify a generic parameter
        list(BevertonHoltRecruitment.log_rzero.value = new_value,
             BevertonHoltRecruitment.log_rzero.estimation_type = "constant")
             )
             )
    data_model <- FIMSFrame(data)
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
like_fit <- run_fims_likelihood(parameters = parameters, data = data1, min = -1, max = 1, length = 3)

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
