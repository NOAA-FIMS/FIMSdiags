
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
                    dateend <= max(dateend) - lubridate::years(years_to_remove)
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
        fit_fims(optimize = TRUE) #TODO: getting error when removing years of data from json (maybe would be fixed if using the dev-json-caa branch? right now on dev branch)
    return(fit)
}

#' Function runs a retrospective analysis for a FIMS model based on a vector of 
#' years to be removed from the data
#'
#' @param years_to_remove vector of number of years to remove (e.g. if you want to 
#' do 5 peels, years_to_remove = 0:5)
#' @param data full dataset used in base model run
#' @param parameters input parameters used in base FIMS model
#' @return A list containing the vector of parameter values and a dataframe of estimates from each retrospective peel
#' @export

run_fims_retrospective <- function(years_to_remove, data, parameters) {
    # Set up parallel processing
    n_cores <- parallel::detectCores() - 1
    plan(multisession, workers = n_cores)
    on.exit(plan(sequential), add = TRUE)

    # Run retro analyses in parallel
    retro_fits <- furrr::future_map(
        .x = years_to_remove,
        .f = run_modified_data_fims,
        data = data,
        parameters = parameters
    )

    estimates_list <- lapply(retro_fits, function(fit) fit@estimates)

    for (i in seq_along(estimates_list)) {
        estimates_list[[i]]$retro_year <- years_to_remove[i]
    }
    estimates_df <- do.call(rbind, estimates_list)

    return(list("years_to_remove" = years_to_remove, "estimates" = estimates_df))
}
