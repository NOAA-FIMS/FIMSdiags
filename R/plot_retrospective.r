#' Plot retrospective analysis of spawning biomass and fishing mortality

#' @param estimates_df Dataframe returned by the [run_fims_retrospective()] function with
#' model fits for each peel in a retrospective analysis
#' 
#' @return ggplot of SSB and F for each retrospective peel
#' @export 
plot_retrospective <- function(estimates_df) {

 # get derived quantities for plotting
 # filter rows in estimates_df to get spawning_biomass and mortality_F
 # calculate uncertainty for ssb and F for base model 
 # create a df of retro timeseries
 # create the plot: 
  # plot all the lines
  # x = year_i, y = estimated
  # linecolor = peel (base model line black and thicker)
  # linetype = peel
  # facet = spawning_biomass and mortality_F
  # if uncertainty = TRUE, use geom_ribbon, else no geom_ribbon

}
