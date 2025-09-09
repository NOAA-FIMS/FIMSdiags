if (FALSE) {

# FIMS Likelihood Profile Custom Stat
# Enables ggplot2-native syntax: ggplot(data) + geom_likelihood()

library(ggplot2)
library(dplyr)


# Utility function to prepare FIMS likelihood profile data
prepare_fims_likelihood_data <- function(fims_likelihood_result, profile_type = "total") {
  # fims_likelihood_result: output from run_fims_likelihood()
  # This contains $vec (parameter values) and $fits (tibble)
  
  if (!is.list(fims_likelihood_result) || 
      !all(c("vec", "fits") %in% names(fims_likelihood_result))) {
    stop("Input must be a list with 'vec' and 'fits' elements from run_fims_likelihood()")
  }
  
  fits_df <- fims_likelihood_result$fits
  
  # Get the parameter column name (should contain "value_")
  param_col <- names(fits_df)[grepl("value_", names(fits_df))]
  if (length(param_col) == 0) {
    stop("No parameter column found (should contain 'value_')")
  }
  param_col <- param_col[1]  # Take first if multiple
  
  # Prepare data based on profile type
  if (profile_type == "total") {
    # Sum all likelihoods by parameter value
    plot_data <- fits_df |>
      dplyr::group_by(.data[[param_col]]) |>
      dplyr::summarise(
        total_nll = -sum(log_like, na.rm = TRUE),  # negative log likelihood
        .groups = "drop"
      ) |>
      dplyr::mutate(
        profile_group = "Total",
        # Calculate relative likelihood (change from minimum)
        relative_nll = total_nll - min(total_nll, na.rm = TRUE)
      ) |>
      dplyr::rename(
        param_value = !!param_col,
        nll = total_nll
      )
  } else if (profile_type == "by_component") {
    # Group by data type/component
    if ("label" %in% names(fits_df)) {
      plot_data <- fits_df |>
        dplyr::group_by(.data[[param_col]], label) |>
        dplyr::summarise(
          total_nll = -sum(log_like, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::group_by(label) |>
        dplyr::mutate(
          relative_nll = total_nll - min(total_nll, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::rename(
          param_value = !!param_col,
          nll = total_nll,
          profile_group = label
        )
      
      # Add total line
      total_data <- fits_df |>
        dplyr::group_by(.data[[param_col]]) |>
        dplyr::summarise(
          total_nll = -sum(log_like, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          profile_group = "Total",
          relative_nll = total_nll - min(total_nll, na.rm = TRUE)
        ) |>
        dplyr::rename(
          param_value = !!param_col,
          nll = total_nll
        )
      
      plot_data <- dplyr::bind_rows(plot_data, total_data)
    } else {
      stop("'label' column not found for component-wise profiling")
    }
  } else {
    stop("profile_type must be 'total' or 'by_component'")
  }
  
  return(plot_data)
}

# Stat for Likelihood Profiles ----
StatLikelihoodProfile <- ggproto("StatLikelihoodProfile", Stat,
  required_aes = c("x", "y"),
  optional_aes = c("group", "colour", "color"),

  setup_params = function(data, params) {
    # Set default parameters
    params$use_relative <- ifelse(is.null(params$use_relative), TRUE, params$use_relative)
    params$smooth <- ifelse(is.null(params$smooth), FALSE, params$smooth)
    #params$smooth_method <- ifelse(is.null(params$smooth_method), "loess", params$smooth_method)
    
    return(params)
  },
  
  setup_data = function(data, params) {
    # Ensure x and y are numeric
    if (!("x" %in% names(data)) || !("y" %in% names(data))) {
      stop("Data must contain 'x' and 'y' aesthetics")
    }
    
    data <- data |>
      dplyr::mutate(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y)
      )
    
    # Use relative likelihood if requested and available
    if (params$use_relative && "relative_nll" %in% names(data)) {
      data$y <- data$relative_nll
    }
    
    # Remove any rows with missing values
    data <- data |>
      dplyr::filter(!is.na(.data$x), !is.na(.data$y))
    
    return(data)
  },
  
  compute_group = function(data, scales, params) {
    # Sort by parameter value for proper line connections
    data <- data |>
      dplyr::arrange(.data$x)
    
    # Apply smoothing if requested
    # if (params$smooth && nrow(data) > 2) {
    #   if (params$smooth_method == "loess" && nrow(data) > 3) {
    #     tryCatch({
    #       smooth_fit <- stats::loess(y ~ x, data = data, span = 0.75)
    #       data$y_smooth <- stats::predict(smooth_fit)
    #       data$y <- data$y_smooth
    #     }, error = function(e) {
    #       # Fallback to simple smoothing if loess fails
    #       data$y <- stats::smooth(data$y)
    #     })
    #   } else {
    #     # Simple moving average for small datasets
    #     data$y <- stats::smooth(data$y)
    #   }
    # }
    
    return(data)
  }
)

# User interface function
stat_likelihood_profile <- function(mapping = NULL, data = NULL, 
                                   geom = "line", position = "identity", 
                                   na.rm = FALSE, show.legend = NA, 
                                   inherit.aes = TRUE, 
                                   use_relative = TRUE,
                                   #smooth = FALSE,
                                   #smooth_method = "loess",
                                   ...) {
  ggplot2::layer(
    stat = StatLikelihoodProfile, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = rlang::list2(
      use_relative = use_relative,
      #smooth = smooth,
      #smooth_method = smooth_method,
      na.rm = na.rm,
      ...
    )
  )
}

# Enhanced plot function using the custom stat
plot_likelihood_profile <- function(like_fit, profile_type = "by_component", 
                                   use_relative = TRUE, #smooth = FALSE,
                                   add_confidence_bands = FALSE) {
  
  # Prepare data using the utility function
  plot_data <- prepare_fims_likelihood_data(like_fit, profile_type)
  
  # Get parameter name for labeling
  param_col <- names(like_fit$fits)[grepl("value_", names(like_fit$fits))][1]
  param_name <- gsub("value_", "", param_col)
  
  # Create base plot
  y_var <- if (use_relative) "relative_nll" else "nll"
  y_label <- if (use_relative) "Change in negative log-likelihood" else "Negative log-likelihood"
  
  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = param_value, y = .data[[y_var]])) +
    stat_likelihood_profile(
      ggplot2::aes(group = profile_group, colour = profile_group),
      use_relative = use_relative,
      smooth = smooth
    ) +
    ggplot2::scale_colour_manual(
      values = c("Total" = "black", 
                setNames(scales::hue_pal()(length(unique(plot_data$profile_group)) - 1),
                        setdiff(unique(plot_data$profile_group), "Total"))),
      guide = ggplot2::guide_legend(title = "Component")
    ) +
    ggplot2::scale_size_manual(
      values = c("Total" = 1.2, 
                setNames(rep(0.8, length(unique(plot_data$profile_group)) - 1),
                        setdiff(unique(plot_data$profile_group), "Total"))),
      guide = "none"
    ) +
    ggplot2::labs(
      x = param_name,
      y = y_label,
      title = "Likelihood Profile"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add confidence bands if requested
  if (add_confidence_bands && use_relative) {
    # Add horizontal lines for 95% confidence (1.92 units above minimum)
    p <- p + 
      ggplot2::geom_hline(yintercept = 1.92, linetype = "dashed", alpha = 0.7, colour = "red") +
      ggplot2::annotate("text", x = -Inf, y = 1.92, label = "95% CI", 
                       hjust = -0.1, vjust = -0.5, size = 3, colour = "red")
  }
  
  ggsave("likelihood_test.png")
  return(p)
}
## Examples: 
plot_data <- prepare_fims_likelihood_data(like_fit, "by_component")

ggplot(plot_data, aes(x = param_value, y = relative_nll, colour = profile_group)) +
  stat_likelihood_profile() +
  theme_minimal()
  ggsave("likelihood_test.png")

plot_likelihood_profile(like_fit)

} # End if (FALSE)