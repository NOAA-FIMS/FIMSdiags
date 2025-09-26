#' Plot likelihood profile

#' @param like_fit List returned by the [fims_likelihood()] function with
#' model fits for each step in a likelihood profile
#' @param group Character string or vector of strings with columns to group by, 
#' e.g., "label" to group by data type, c("label", "fleet_name") to group by data 
#' type and fleet (although fleet_name is not currently in FIMS output).
#' 
#' @return ggplot of likelihood profile
#' @export 
plot_likelihood <- function(like_fit, group = "label") {
 # check that like_fit is actually the output from fims_likelihood()
 if (!"vec" %in% names(like_fit) | !"estimates" %in% names(like_fit)) {
  cli::cli_abort("like_fit needs to be a list returned by `fims_likelihood()` that contains `vec` and `estimates`")
 }
 # get column name for parameter being profiled
  colname <- like_fit$estimates |> names() |> grep(pattern = "value_", value = TRUE)
  if (length(colname) > 1) {
    cli::cli_abort("Function doesn't yet support multiple profile parameters")
  }
  if (length(colname) == 0) {
    cli::cli_abort(
      "No column with profile parameter found (column name should contain 'value_')"
    )
  }
  # TODO: add column to store the grouping variable(s)
  # could use paste() to combine multiple grouping variables into one column
  # then use that column for grouping below
  
  # summing total likelihood by parameter value, use later for plotting
  by_type <- like_fit$estimates |>
    dplyr::filter(!is.na(lpdf)) |>
    dplyr::group_by(.data[[colname]], .data[[group]]) |> # need to group by fleet, but it's not present in tibble at the moment
    dplyr::distinct(lpdf) |>
    dplyr::summarise(total_like = sum(lpdf))  # TODO: make sure lpdf is the value we want to use

  ### add total likelihood across all groups (TODO: turned off for now while we sort out new group column)
  # total <- like_fit$fits |>
  #   dplyr::group_by(.data[[colname]]) |>
  #   dplyr::summarise(total_like = -sum(log_like)) |> # negative to make negative log likelihood
  #   dplyr::mutate(label = "Total") |>
  #   dplyr::select(.data[[colname]], label, total_like)

  # group the data type totals and the overall total and then
  # for each vector of sums, subtract the minimum across within that vector
  grouped_like <- by_type |> # rbind(by_type, total) |>
    dplyr::arrange(.data[[colname]]) |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::mutate(total_like_change = total_like - min(total_like))

  # plot all the lines
  p1 <- grouped_like |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = .data[[colname]],
      y = total_like_change,
      color = .data[[group]] #,
      # 
      # linewidth = ifelse(.data[[group]] == "Total", 2, 0.8)
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
