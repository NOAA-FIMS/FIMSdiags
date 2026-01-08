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
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#'  library(FIMS)
#' # Use built-in dataset from FIMS
#'  data("data1")
#' # Create a parameters object
#'  parameters <- data_4_model |>
#'    create_default_configurations() |>
#'    create_default_parameters(data = data_4_model)
#'
#'  fit1 <- run_fims_retrospective(
#'    years_to_remove = 1,
#'    data = data1,
#'    parameters = parameters,
#'    n_cores = 1
#'   )
#'
#' }

run_fims_retrospective <- function(
    years_to_remove, 
    data, 
    parameters, 
    n_cores = NULL) {

    # Validate years_to_remove
    if (length(years_to_remove) == 0) {
        cli::cli_abort("years_to_remove must have at least one value")
    }
    
    if (any(years_to_remove < 0)) {
        cli::cli_abort("years_to_remove must contain non-negative values")
    }

    # Set number of cores to use 
    if (is.null(n_cores)) {
        n_cores_to_use <- parallel::detectCores() - 1
    } else {
        # Validate n_cores before conversion
        if (!is.numeric(n_cores) || n_cores != as.integer(n_cores) || n_cores <= 0) {
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
    on.exit(future::plan(future::sequential), add = TRUE)

    # Run retro analyses in parallel
    estimates_list <- furrr::future_map(
        .x = years_to_remove,
        .f = function(years){
            fit <- run_modified_data_fims(
                years_to_remove = years, 
                data = data, 
                parameters = parameters
            )
            FIMS::get_estimates(fit)
        },
        .options = furrr::furrr_options(seed = TRUE, globals = TRUE)
    )

    for (i in seq_along(estimates_list)) {
        estimates_list[[i]]$retro_year <- years_to_remove[i]
    }
    estimates_df <- do.call(rbind, estimates_list)

    return(list("years_to_remove" = years_to_remove, "estimates" = estimates_df))
}
