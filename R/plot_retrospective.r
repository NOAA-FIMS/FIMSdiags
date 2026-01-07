#' Plot retrospective analysis of spawning biomass and fishing mortality

#' @param retro_fit List returned by the [run_fims_retrospective()] function with
#' the years to remove and estimates dataframe from a FIMS model
#' @param quantity A vector of the quantity to plot, "spawning_biomass" and/or "mortality_F". 
#' 
#' @return ggplot of SSB and F for each retrospective peel
#' @export 
#' 
#' @importFrom rlang .data
plot_retrospective <- function(retro_fit, quantity = c("spawning_biomass", "mortality_F")) {

  # filter rows in estimates_df to get spawning_biomass and mortality_F
  retro_df <- retro_fit[["estimates"]] |>
    dplyr::filter(.data$label %in% quantity) |> #right now mortality_F is by year/age, so ignoring it for simplicity
    #TODO: some summarization to get total annual F values? 
    dplyr::select(.data$label, .data$year_i, .data$age_i, 
                  .data$estimated, .data$uncertainty, .data$retro_year) |> 
    dplyr::mutate(lower_CI = .data$estimated - (1.96 * .data$uncertainty),
                  upper_CI = .data$estimated + (1.96 * .data$uncertainty),
                  retro_year = factor(.data$retro_year)) #assuming uncertainty is SE, TODO: check this assumption

  retro_plot <- ggplot2::ggplot(data = retro_df, ggplot2::aes(x = .data$year_i)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_CI, 
                                      ymax = .data$upper_CI, 
                                      fill = .data$retro_year), alpha = .45) +   # should this be conditional?
    ggplot2::geom_line(ggplot2::aes(y = .data$estimated, 
                                    color = .data$retro_year, 
                                    linetype = .data$retro_year)) +
    stockplotr::theme_noaa(discrete = TRUE) 

  if(length(unique(quantity)) > 1){
    retro_plot <- retro_plot + 
      ggplot2::facet_wrap(~.data$label, scales = "free_y", ncol = 1)
  }

  ggplot2::ggsave("retrospective.png")

  return(retro_plot)
}
