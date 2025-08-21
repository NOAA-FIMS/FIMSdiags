#' Plot likelihood profile

#' @param like_fit List returned by the [fims_likelihood()] function with 
#' model fits for each step in a likelihood profile
#' @param type vector of data type to plot likelihood
#' @param fleet vector of fleet(s) to show in plot (TODO: decide on name or number or either as input)
plot_likelihood <- function(like_fit, type = NULL, fleet = NULL) {

  # summing total likelihood by parameter value, use later for plotting
  by_type <- like_fit$fits |>
    dplyr::group_by(profile_parameter_value, label) |>
    dplyr::summarise(total_like = -sum(log_like)) # negative to make negative log likelihood
  total <- like_fit$fits |>   
    dplyr::group_by(profile_parameter_value) |> 
    dplyr::summarise(total_like = -sum(log_like)) |> # negative to make negative log likelihood
    dplyr::mutate(label = "Total") |> 
    dplyr::select(profile_parameter_value, label, total_like)
  # group the data type totals and the overall total and then 
  # for each vector of sums, subtract the minimum across within that vector
  grouped_like <- rbind(by_type, total) |> 
    dplyr::arrange(profile_parameter_value) |>
    dplyr::group_by(label) |> 
    dplyr::mutate(total_like_change = total_like - min(total_like))
  
  # plot all the lines 
  p1 <- grouped_like |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = profile_parameter_value, y = total_like_change, color = label)) 
 #   ggplot2::ggsave("likelihood.png")

  return(p1)
}