#' Function runs a retrospective analysis for a FIMS model based on a vector of 
#' years to be removed from the data
#'
#' @param years_to_remove vector of number of years to remove (e.g. if you want to 
#' do 5 peels, years_to_remove = 0:5)
#' @param data full dataset used in base model run
#' @param parameters input parameters used in base FIMS model
#' @param n_cores The number of cores to use to run likelihood profile in parallel. Default is `parallel::detectCores() - 1`.
#' @return A list containing the vector of parameter values and a dataframe of estimates from each retrospective peel
#' @export

run_fims_retrospective <- function(
    years_to_remove, 
    data, 
    parameters, 
    n_cores = NULL) {

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
    on.exit(future::plan(future::sequential), add = TRUE)

    # Run retro analyses in parallel
    retro_fits <- furrr::future_map(
        .x = years_to_remove,
        .f = run_modified_data_fims,
        data = data,
        parameters = parameters,
        .options = furrr::furrr_options(seed = TRUE, globals = TRUE)
    )

    # pull the estimates tibble out of each of the FIMSFit S4 objects into a list
    estimates_list <- purrr::map(estimates, get_estimates)

    for (i in seq_along(estimates_list)) {
        estimates_list[[i]]$retro_year <- years_to_remove[i]
    }
    estimates_df <- do.call(rbind, estimates_list)

    return(list("years_to_remove" = years_to_remove, "estimates" = estimates_df))
}
