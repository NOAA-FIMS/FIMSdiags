#' Calculate Mohn's Rho for Retrospective Analysis
#'
#' @title Calculate Mohn's Rho
#' 
#' @description
#' Calculates Mohn's rho statistic for a given quantity from a retrospective
#' analysis. Mohn's rho measures the average relative bias in estimated
#' quantities when retrospectively removing years of data.
#' 
#' @details
#' Mohn's rho is calculated as the average relative difference between the
#' estimate from each retrospective peel and the reference model estimate
#' for the corresponding terminal year. The statistic is commonly used to
#' detect retrospective patterns in stock assessment models.
#' 
#' For each retrospective peel, the relative difference is:
#' \deqn{(estimate_{peel} - estimate_{ref}) / estimate_{ref}}
#' 
#' Mohn's rho is the mean of these relative differences across all peels.
#' Values close to zero indicate no retrospective pattern. Positive values
#' indicate upward revision of estimates, while negative values indicate
#' downward revision.
#' 
#' @param retro_fit A list returned by [run_fims_retrospective()] containing
#'   the retrospective analysis results. Must include:
#'   * `years_to_remove` - Vector of years removed for each peel
#'   * `estimates` - Data frame with model estimates for each peel
#' @param quantity A character string specifying the quantity to calculate
#'   Mohn's rho for. Must match a value in the `label` column of the estimates
#'   data frame. Default is `"spawning_biomass"`. Other options include
#'   `"mortality_F"` or any other estimated quantity in the FIMS model output
#' 
#' @return A numeric scalar giving Mohn's rho for the specified quantity.
#'   The value represents the average relative bias across all retrospective
#'   peels
#' 
#' @references
#' Mohn, R. 1999. The retrospective problem in sequential population analysis:
#' An investigation using cod fishery and simulated data. ICES Journal of
#' Marine Science 56: 473-488.
#' 
#' Hurtado-Ferro, F., Szuwalski, C.S., Valero, J.L., Anderson, S.C.,
#' Cunningham, C.J., Johnson, K.F., Licandeo, R., McGilliard, C.R., Monnahan,
#' C.C., Muradian, M.L., Ono, K., Vert-Pre, K.A., Whitten, A.R., and Punt, A.E.
#' 2015. Looking in the rear-view mirror: bias and retrospective patterns in
#' integrated, age-structured stock assessment models. ICES Journal of Marine
#' Science 72(1): 99-110.
#' 
#' @seealso
#' * [run_fims_retrospective()] for running retrospective analysis
#' * [plot_retrospective()] for visualizing retrospective results
#' 
#' @family diagnostic_functions
#' 
#' @export
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' library(FIMS)
#' # Use built-in dataset from FIMS
#' data("data1")
#' data_4_model <- FIMSFrame(data1)
#' 
#' # Create parameters object
#' parameters <- data_4_model |>
#'   create_default_configurations() |>
#'   create_default_parameters(data = data_4_model)
#' 
#' # Run base model
#' base_model <- parameters |>
#'   initialize_fims(data = data_4_model) |>
#'   fit_fims(optimize = TRUE)
#' 
#' # Run retrospective analysis
#' retro_fit <- run_fims_retrospective(
#'   years_to_remove = 0:5,
#'   data = data1,
#'   parameters = parameters,
#'   n_cores = 1
#' )
#' 
#' # Calculate Mohn's rho for spawning biomass
#' mohn_rho_sb <- calculate_mohns_rho(
#'   retro_fit = retro_fit,
#'   quantity = "spawning_biomass"
#' )
#' 
#' # Calculate Mohn's rho for fishing mortality
#' mohn_rho_f <- calculate_mohns_rho(
#'   retro_fit = retro_fit,
#'   quantity = "mortality_F"
#' )
#' }
calculate_mohns_rho <- function(retro_fit, quantity = "spawning_biomass") {
  
  # Check that retro_fit has the required structure
  if (!"years_to_remove" %in% names(retro_fit) | !"estimates" %in% names(retro_fit)) {
    cli::cli_abort("retro_fit must be a list returned by {.fn run_fims_retrospective} that contains {.field years_to_remove} and {.field estimates}")
  }
  
  # Check that the first model has 0 years peeled
  if (0 != retro_fit[["years_to_remove"]][[1]]) {
    cli::cli_abort("First element of {.field years_to_remove} must be 0 (reference model with no years removed)")
  }
  
  # Filter estimates for the specified quantity
  retro_estimates <- retro_fit[["estimates"]] |>
    dplyr::filter(.data$label %in% quantity)
  
  # Check that the quantity exists in the estimates
  if (nrow(retro_estimates) == 0) {
    cli::cli_abort("Quantity {.val {quantity}} not found in estimates. Check the {.field label} column")
  }
  
  # Initialize vector to store relative differences
  mohn_values <- numeric()
  
  # Calculate vector of ending year for each model
  end_year <- max(retro_estimates$year_i) - retro_fit$years_to_remove
  
  # Calculate relative difference for each peel (excluding reference model)
  for (i in 2:length(retro_fit$years_to_remove)) {
    # Get value for ending year of peeled model
    sb_peel <- retro_estimates |>
      dplyr::filter(.data$year_i == end_year[i]) |>
      dplyr::filter(.data$retro_year == retro_fit$years_to_remove[i]) |>
      dplyr::pull(.data$estimated)
    
    # Get value from the same year for the reference model
    sb_ref <- retro_estimates |>
      dplyr::filter(.data$year_i == end_year[i]) |>
      dplyr::filter(.data$retro_year == retro_fit$years_to_remove[1]) |>
      dplyr::pull(.data$estimated)
    
    # Calculate the relative difference
    # (store in the i-1 position because model 1 is the reference model)
    mohn_values[i - 1] <- (sb_peel - sb_ref) / sb_ref
  }
  
  # Calculate mean rho by averaging across peels (not including the reference year)
  mohn_rho <- sum(mohn_values) / (length(retro_fit$years_to_remove) - 1)
  
  return(mohn_rho)
}
