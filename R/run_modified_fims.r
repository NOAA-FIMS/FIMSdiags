run_modified_fims <- function(new_value, parameters, data) { #TODO: Move this outside the likelihood function?
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
    #data object will be available later
    #like_fit$fits[[1]]@obj$env$data or #like_fit$fits[[1]]@obj$data

    new_fit <- parameters_mod |>
      initialize_fims(data = data_model) |>
      fit_fims(optimize = TRUE)
    return(new_fit)
  }