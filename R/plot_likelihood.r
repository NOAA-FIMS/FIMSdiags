#' Plot likelihood profile

#' @param like_fit List returned by the [fims_likelihood()] function with
#' model fits for each step in a likelihood profile
#' @param type vector of data type to plot likelihood
#' @param fleet vector of fleet(s) to show in plot (TODO: decide on name or number or either as input)
plot_likelihood <- function(like_fit, type = NULL, fleet = NULL) {
  # get column name for parameter being profiled
  colname <- like_fit$fits |> names() |> grep(pattern = "value_", value = TRUE)
  if (length(colname) > 1) {
    cli::cli_abort("Function doesn't yet support multiple profile parameters")
  }
  if (length(colname) == 0) {
    cli::cli_abort("No column with profile parameter found (column name should contain 'value_')")
  }
  
  
  # summing total likelihood by parameter value, use later for plotting
  by_type <- like_fit$fits |>
    dplyr::group_by(.data[[colname]], label) |>
    dplyr::summarise(total_like = -sum(log_like)) # negative to make negative log likelihood
  total <- like_fit$fits |>
    dplyr::group_by(.data[[colname]]) |>
    dplyr::summarise(total_like = -sum(log_like)) |> # negative to make negative log likelihood
    dplyr::mutate(label = "Total") |>
    dplyr::select(.data[[colname]], label, total_like)
  # group the data type totals and the overall total and then
  # for each vector of sums, subtract the minimum across within that vector
  grouped_like <- rbind(by_type, total) |>
    dplyr::arrange(.data[[colname]]) |>
    dplyr::group_by(label) |>
    dplyr::mutate(total_like_change = total_like - min(total_like))

  # plot all the lines
  p1 <- grouped_like |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = .data[[colname]],
      y = total_like_change,
      color = label,
      linewidth = ifelse(label == "Total", 2, 0.8)
    )) +
    ggplot2::scale_linewidth_identity(guide = "none") +
    ggplot2::labs(
      x = colname |> gsub("value_", "", x = _), # remove "value_" from label
      y = "Change in negative log-likelihood",
      color = "Data Type"
    )
  ggplot2::ggsave("likelihood.png")


  # improve x and y labels
  # make total line black
  # add different line types for data types

  return(p1)
}
