#' Plot likelihood profile

#' @param like_fit List returned by the [fims_likelihood()] function with
#' model fits for each step in a likelihood profile
#' @param group Character string or vector of strings with columns to group by, 
#' e.g., "label" to group by data type, c("label", "fleet_name") to group by data 
#' type and fleet (although fleet_name is not currently in FIMS output).
#' 
#' @return ggplot of likelihood profile
#' @export 
#' 
#' @examples 
#' \dontrun{
#' like_fit <- run_fims_likelihood(
#'  model = base_model,
#'  parameters = parameters,
#'  data = data1,
#'  n_cores = 3,
#'  min = -1,
#'  max = 1,
#'  length = 3
#'  )
#'plot_likelihood(like_fit)
#' }
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
  
  # summing total likelihood by parameter value and data type, use later for plotting
  by_type <- like_fit$estimates |>
    dplyr::filter(!is.na(lpdf)) |>
    dplyr::group_by(.data[[colname]], .data[[group]])|> # grouping by parameter and data type
    dplyr::distinct(lpdf) |>
    dplyr::summarise(total_like = sum(lpdf))  

  ### add total likelihood across all groups (TODO: turned off for now while we sort out new group column)
  total <- like_fit$estimates |>
    dplyr::filter(!is.na(lpdf)) |>
    dplyr::group_by(.data[[colname]]) |>
    dplyr::distinct(lpdf) |>
    dplyr::summarise(total_like = sum(lpdf)) |> # negative to make negative log likelihood
    dplyr::mutate(label = "Total") |>
    dplyr::select(.data[[colname]], label, total_like)

  # group the data type totals and the overall total and then
  # for each vector of sums, subtract the maximum across within that vector
  grouped_like <- by_type |>  
    dplyr::bind_rows(total) |>
    dplyr::arrange(.data[[colname]]) |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::mutate(total_like_change = max(total_like) - total_like) #based on convo with Andrea (12/3), the C++ code is positive log-like and doesn't look like reshape_json_fits code converts to NLL, so using max(total_like) instead of min()


# Get all unique group values
all_groups_clean <- unique(gsub("_expected", "", grouped_like[[group]]))
other_groups_clean <- setdiff(all_groups_clean, "Total")

# Create color vector: black for Total, default ggplot colors for others
color_values <- c(
  "Total" = "black",
  setNames(
    viridisLite::viridis(length(other_groups_clean), option = "mako", 
                        begin = 0.35, end = 0.9),
    other_groups_clean
  )
)

linetype_values <- c(
  "Total" = "solid", 
  setNames(rep(c("dashed", "dotted", "dotdash", "longdash"), 
              length.out = length(other_groups_clean)), 
          other_groups_clean)
)
  # plot all the lines
  p1 <- grouped_like |>
    dplyr::mutate(
    group_clean = gsub("_expected", "", .data[[group]])
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = .data[[colname]],
      y = total_like_change,
      colour = group_clean, 
      linetype = group_clean
    ), linewidth = 1.2) +
    stockplotr::theme_noaa(discrete = TRUE) +
    ggplot2::scale_color_manual(
    values = color_values,
    name = "Data Type"  
  ) +
  ggplot2::scale_linetype_manual(
    values = linetype_values,
    name = "Data Type"  
  ) +
    ggplot2::labs(
      x = colname |> gsub("value_", "", x = _), # remove "value_" from label
      y = "Change in log-likelihood",
      color = "Data Type"
    ) 
  ggplot2::ggsave("likelihood.png")

  return(p1)
}
