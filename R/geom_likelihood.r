if (FALSE) {

# FIMS Likelihood Profile Custom Geom and Stat
# Enables ggplot2-native syntax: ggplot(data) + geom_likelihood()

library(ggplot2)
library(dplyr)

#' Custom geom for FIMS likelihood profiles
#'
#' This geom creates likelihood profile plots with automatic data processing.
#' Can be used with the natural ggplot2 syntax. Use aesthetic mappings to control
#' grouping by data type, fleet, or other variables.
#'
#' @param mapping Aesthetic mapping - use aes(colour = type) or aes(colour = fleet)
#' @param data Data from run_fims_likelihood() output  
#' @param stat Character string specifying stat: "likelihood" (default)
#' @param position Position adjustment (default: "identity")
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' @param na.rm Logical, whether to remove NAs
#' @param highlight_total Logical, make total line thicker (default: TRUE)
#' @param line_size Numeric, base line size (default: 1)
#' @param alpha Numeric, line transparency (default: 0.8)
#' @param include_total Logical, whether to include total likelihood line (default: TRUE)
#' @param filter_type Character vector, filter to specific data types (optional)
#' @param filter_fleet Character vector, filter to specific fleets (optional)
#' @param ... Other arguments passed to layer
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - lines by data type
#' run_fims_likelihood(model, parameters, data) %>% 
#'   ggplot() + 
#'   geom_likelihood(aes(colour = type))
#'
#' # Lines by fleet for a specific data type
#' run_fims_likelihood(model, parameters, data) %>% 
#'   ggplot() + 
#'   geom_likelihood(aes(colour = fleet), filter_type = "index")
#'
#' # Lines by fleet, faceted by data type
#' run_fims_likelihood(model, parameters, data) %>% 
#'   ggplot() + 
#'   geom_likelihood(aes(colour = fleet)) +
#'   facet_wrap(~type)
#'
#' # Only total likelihood
#' run_fims_likelihood(model, parameters, data) %>% 
#'   ggplot() + 
#'   geom_likelihood(include_total = TRUE, filter_type = character(0))
#' }

geom_likelihood <- function(mapping = NULL, data = NULL, stat = "likelihood",
                           position = "identity", show.legend = NA, 
                           inherit.aes = TRUE, na.rm = FALSE,
                           highlight_total = TRUE, line_size = 1, alpha = 0.8, 
                           include_total = TRUE, filter_type = NULL, 
                           filter_fleet = NULL, ...) {
  
  # Create the layer with our custom stat and geom
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLikelihood,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      highlight_total = highlight_total,
      line_size = line_size,
      alpha = alpha,
      include_total = include_total,
      filter_type = filter_type,
      filter_fleet = filter_fleet,
      ...
    )
  )
}

#' Custom Geom for Likelihood Profiles
#' @export
GeomLikelihood <- ggproto("GeomLikelihood", Geom,
  required_aes = c("x", "y"),
  optional_aes = c("colour", "size", "linetype", "alpha", "group"),
  default_aes = aes(colour = "black", size = 1, linetype = 1, alpha = 0.8),
  
  draw_panel = function(data, panel_params, coord, highlight_total = TRUE, 
                       line_size = 1, alpha = 0.8) {
    
    # Sort data by x for proper line drawing
    data <- data[order(data$x), ]
    
    # Create different line sizes for total vs components
    if (highlight_total && "group" %in% names(data)) {
      # Check if any group corresponds to "Total"
      total_groups <- unique(data$group[data$component == "Total"])
      if (length(total_groups) > 0) {
        data$size[data$group %in% total_groups] <- line_size * 1.5
        data$size[!data$group %in% total_groups] <- line_size * 0.8
      }
    }
    
    # Transform coordinates
    coords <- coord$transform(data, panel_params)
    
    # Create line grobs for each group
    groups <- split(coords, coords$group)
    
    grobs <- lapply(groups, function(group_data) {
      if (nrow(group_data) < 2) return(nullGrob())
      
      grid::polylineGrob(
        x = group_data$x,
        y = group_data$y,
        default.units = "native",
        gp = grid::gpar(
          col = alpha(group_data$colour[1], group_data$alpha[1]),
          lwd = group_data$size[1] * .pt,
          lty = group_data$linetype[1]
        )
      )
    })
    
    do.call(grid::grobTree, grobs)
  }
)

#' Stat for processing FIMS likelihood data
#'
#' @param mapping Aesthetic mapping
#' @param data FIMS likelihood data
#' @param geom Geom to use (default: "likelihood")
#' @param position Position adjustment
#' @param na.rm Remove NAs
#' @param show.legend Show legend
#' @param inherit.aes Inherit aesthetics
#' @param grouping How to group the data: "component", "data_type", "total_only"
#' @param include_total Whether to include total likelihood line
#' @param ... Other arguments
#' @export

stat_likelihood <- function(mapping = NULL, data = NULL, geom = "likelihood",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, grouping = "component", 
                           include_total = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatLikelihood,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      grouping = grouping,
      include_total = include_total,
      ...
    )
  )
}

#' Custom Stat for Likelihood Processing
#' @export
StatLikelihood <- ggproto("StatLikelihood", Stat,
  required_aes = c(),
  optional_aes = c(),
  
  compute_layer = function(data, params, layout) {
    # This function processes the FIMS likelihood data
    # Input data should be the 'fits' component from run_fims_likelihood()
    
    # Check if this is FIMS likelihood data
    required_cols <- c("profile_parameter_value", "log_like")
    if (!all(required_cols %in% names(data))) {
      stop("Data must contain columns: ", paste(required_cols, collapse = ", "),
           "\nThis should be the 'fits' component from run_fims_likelihood() output.")
    }
    
    # Apply filters if specified
    if (!is.null(params$filter_type)) {
      if ("type" %in% names(data)) {
        data <- data[data$type %in% params$filter_type, ]
      }
    }
    
    if (!is.null(params$filter_fleet)) {
      if ("fleet" %in% names(data)) {
        data <- data[data$fleet %in% params$filter_fleet, ]
      }
    }
    
    # Determine grouping variable based on aesthetics
    # Default to 'label' if no aesthetic mapping provided
    group_var <- "label"
    
    # Check if there are aesthetic mappings that suggest different grouping
    if (!is.null(params$mapping)) {
      if ("colour" %in% names(params$mapping)) {
        mapped_var <- as.character(params$mapping$colour)
        if (mapped_var %in% names(data)) {
          group_var <- mapped_var
        }
      }
    }
    
    # Group by parameter value and grouping variable, sum log likelihood
    group_cols <- c("profile_parameter_value", group_var)
    
    by_group <- data %>%
      dplyr::group_by(across(all_of(group_cols))) %>%
      dplyr::summarise(total_like = -sum(log_like, na.rm = TRUE), .groups = "drop")
    
    # Add total likelihood if requested
    if (params$include_total && length(params$filter_type) != 0) {
      total <- data %>%   
        dplyr::group_by(profile_parameter_value) %>% 
        dplyr::summarise(total_like = -sum(log_like, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(!!group_var := "Total") %>% 
        dplyr::select(profile_parameter_value, all_of(group_var), total_like)
      
      combined_data <- rbind(by_group, total)
    } else {
      combined_data <- by_group
    }
    
    # Calculate delta likelihood (change from minimum within each group)
    processed_data <- combined_data %>% 
      dplyr::arrange(profile_parameter_value) %>%
      dplyr::group_by(across(all_of(group_var))) %>% 
      dplyr::mutate(delta_nll = total_like - min(total_like, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    # Create the output data frame for ggplot
    result <- data.frame(
      x = processed_data$profile_parameter_value,
      y = processed_data$delta_nll,
      colour = processed_data[[group_var]],
      group = as.numeric(as.factor(processed_data[[group_var]])),
      component = processed_data[[group_var]],
      PANEL = 1,
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
)

# Alternative stat names for different groupings
# Don't need? 
# stat_data_type <- function(...) {
#   stat_likelihood(grouping = "data_type", ...)
# }

# stat_total_only <- function(...) {
#   stat_likelihood(grouping = "total_only", include_total = TRUE, ...)
# }

# stat_component <- function(...) {
#   stat_likelihood(grouping = "component", ...)
# }

#' Prepare FIMS likelihood data for ggplot2
#'
#' Helper function to extract the fits component from run_fims_likelihood output
#' and prepare it for use with ggplot2 and geom_likelihood
#'
#' @param likelihood_output List from run_fims_likelihood()
#' @return Data frame suitable for ggplot2
#' @export

prepare_fims_data <- function(likelihood_output) {
  if (!is.list(likelihood_output) || !"fits" %in% names(likelihood_output)) {
    stop("Input must be output from run_fims_likelihood() containing 'fits' component")
  }
  
  return(likelihood_output$fits)
}

#' Enhanced ggplot method for FIMS likelihood data
#'
#' S3 method to make ggplot work directly with run_fims_likelihood() output
#'
#' @param data Output from run_fims_likelihood()
#' @param mapping Aesthetic mapping
#' @param ... Other arguments to ggplot
#' @return ggplot object
#' @export

ggplot.fims_likelihood <- function(data, mapping = aes(), ...) {
  # Extract fits data
  plot_data <- data$fits
  
  # Create ggplot with automatic aesthetics
  ggplot(plot_data, mapping, ...)
}

#' Set class for run_fims_likelihood output
#'
#' Helper function to add class for S3 method dispatch
#' This should be added to the run_fims_likelihood function return
#'
#' @param likelihood_output Output from run_fims_likelihood
#' @return Same data with fims_likelihood class
#' @export

as_fims_likelihood <- function(likelihood_output) {
  class(likelihood_output) <- c("fims_likelihood", class(likelihood_output))
  return(likelihood_output)
}

#' Complete example plotting function
#'
#' Shows how to create a complete likelihood profile plot with the new geom
#'
#' @param likelihood_data Output from run_fims_likelihood()
#' @param parameter_name Name for x-axis
#' @param add_reference_lines Add MLE and CI lines
#' @return ggplot object
#' @export

example_likelihood_plot <- function(likelihood_data, 
                                   parameter_name = "Parameter Value",
                                   add_reference_lines = TRUE) {
  
  # Base plot
  p <- likelihood_data$fits %>%
    ggplot() +
    geom_likelihood(aes(colour = after_stat(component))) +
    labs(
      x = parameter_name,
      y = expression(Delta ~ "Negative Log-Likelihood"),
      colour = "Component",
      title = paste("Likelihood Profile:", parameter_name)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (add_reference_lines) {
    # Add MLE line (at minimum of total likelihood)
    total_data <- likelihood_data$fits %>%
      group_by(profile_parameter_value) %>%
      summarise(total_like = -sum(log_like, na.rm = TRUE), .groups = "drop")
    
    mle_value <- total_data$profile_parameter_value[which.min(total_data$total_like)]
    
    p <- p + 
      geom_vline(xintercept = mle_value, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = qchisq(0.95, df = 1) / 2, 
                 linetype = "dotted", alpha = 0.7)
  }
  
  return(p)
}


# # Method 1: Lines by data type (most common)
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(aes(colour = type)) +
#   labs(title = "Likelihood Profile by Data Type")
#
# # Method 2: Lines by fleet for a specific data type
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(aes(colour = fleet), filter_type = "index") +
#   labs(title = "Index Likelihood by Fleet")
#
# # Method 3: Lines by fleet, separate panels for each data type
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(aes(colour = fleet)) +
#   facet_wrap(~type, scales = "free_y") +
#   labs(title = "Likelihood by Fleet and Data Type")
#
# # Method 4: Only total likelihood
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(include_total = TRUE, filter_type = character(0)) +
#   labs(title = "Total Likelihood Profile")
#
# # Method 5: Specific fleets only
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(aes(colour = type), filter_fleet = c("fleet1", "fleet2"))
#
# # Method 6: Custom styling with ggplot2 features
# run_fims_likelihood(model, parameters, data) %>%
#   prepare_fims_data() %>%
#   ggplot() +
#   geom_likelihood(aes(colour = type, linetype = fleet)) +
#   scale_colour_brewer(type = "qual") +
#   theme_minimal() +
#   labs(title = "Multi-dimensional Likelihood Profile")


} # End if (FALSE)