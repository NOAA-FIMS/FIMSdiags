
quantity <- "spawning_biomass"

# check that the the first model has 0 years peeled
if (0 != retro_fit[["years_to_remove"]][[1]]) {
   cli::cli_abort("estimates tibble must contain reference year run (retro_year = 0)")
 }

retro_estimates <- retro_fit[["estimates"]] |>
    dplyr::filter(.data$label %in% quantity) 

retro_fit$years_to_remove

mohn_sb <- numeric()

# calculate vector of ending year for each model
end_year <- max(retro_estimates$year_i) - retro_fit$years_to_remove 

for (i in 2:length(retro_fit$years_to_remove)) {
    # get value for ending year of peeled model
    sb_peel <- retro_estimates |> 
    dplyr::filter(year_i == end_year[i]) |>
    dplyr::filter(retro_year == retro_fit$years_to_remove[i]) |>
    dplyr::pull(estimated)

    # get value from the same year for the reference model
    sb_ref <- retro_estimates |> 
    dplyr::filter(year_i == end_year[i]) |>
    dplyr::filter(retro_year == retro_fit$years_to_remove[1]) |>
    dplyr::pull(estimated)

    # calculate the relative difference 
    # (store in the i-1 position because model 1 is the reference model)
    mohn_sb[i-1] <- (sb_peel - sb_ref)/sb_ref

}

# calculate mean rho by averaging across peels (not including the reference year)
mohn_sb <- sum(mohn_sb) / (length(retro_fit$years_to_remove) - 1)
