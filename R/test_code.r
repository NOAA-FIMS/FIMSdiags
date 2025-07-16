library(FIMS)

# clear memory
clear()
## Testing how to remove one year of data and run multiple FIMS models 
# Load sample data
data("data1")
# Prepare data for FIMS model
data_4_model <- FIMSFrame(data1)

# Define fleet specifications
fleet1 <- list(selectivity = list(form = "LogisticSelectivity"),
data_distribution = c(Landings = "DlnormDistribution", 
AgeComp = "DmultinomDistribution",
LengthComp = "DmultinomDistribution"))
survey1 <- list(selectivity = list(form = "LogisticSelectivity"),
data_distribution = c(Index = "DlnormDistribution", 
AgeComp = "DmultinomDistribution",
LengthComp = "DmultinomDistribution"))

# Create parameters
parameters <- data_4_model |>
  create_default_parameters(fleets = list(fleet1 = fleet1, survey1 = survey1))

  # Run the  model with optimization
fit <- parameters |>
  initialize_fims(data = data_4_model) |>
  fit_fims(optimize = TRUE)

  # Clear memory post-run
clear()

# remove one year of data and create new dataframe
# need to keep catch and weight-at-age data, just remove indices, length comp, and age comps
data2 <- data1 |> dplyr::filter(dateend <= max(dateend)-lubridate::years(1)) 
# Prepare data for FIMS model
data_5_model <- FIMSFrame(data2)

# Define fleet specifications
fleet1 <- list(selectivity = list(form = "LogisticSelectivity"),data_distribution = c(Landings = "DlnormDistribution", AgeComp = "DmultinomDistribution",LengthComp = "DmultinomDistribution"))
survey1 <- list(selectivity = list(form = "LogisticSelectivity"),data_distribution = c(Index = "DlnormDistribution", AgeComp = "DmultinomDistribution",LengthComp = "DmultinomDistribution"))

# Create parameters
parameters <- data_5_model |>
  create_default_parameters(fleets = list(fleet1 = fleet1, survey1 = survey1))

  # Run the  model with optimization
fit2 <- parameters |>
  initialize_fims(data = data_5_model) |>
  fit_fims(optimize = TRUE)

str(fit)
str(fit2)