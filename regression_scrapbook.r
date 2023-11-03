# ------------------------------------------------------------------------------
# Automated Poisson regression reults tables with calculated deviance difference
# ------------------------------------------------------------------------------

# List of response variables
response_variables <- c("daily_deaths", "daily_cases", "daily_hospitalizations", "daily_icu", "daily_test")

# List of country data frames
country_dfs <- list(brazil_df, china_df, india_df, israel_df, italy_df, senegal_df, 
                    sout_africa_df, south_korea_df, united_kingdom_df, united_states_df)

# Create an empty data frame to store results
results_df <- data.frame(Response_Variable = character(), 
                         Country = character(),  # Add a column for the country name
                         Estimate = numeric(),
                         Std_Error = numeric(), 
                         Z_Value = numeric(),
                         Pr_Z = numeric(), 
                         Lower_CI = numeric(), 
                         Upper_CI = numeric(),
                         Deviance = numeric(),
                         Null_Deviance = numeric(),  # Add a column for null deviance
                         Fitted_Deviance = numeric(),  # Add a column for fitted deviance
                         Deviance_Difference = numeric(),  # Add a column for deviance difference
                         stringsAsFactors = FALSE)

# Loop through country data frames
for (country_df in country_dfs) {
  # Get the country name (assuming a "Country" column exists)
  country_name <- unique(country_df$Country)
  
  # Loop through response variables
  for (response_var in response_variables) {
    # Fit Poisson regression model
    poisson_model <- glm(paste(response_var, "~ policy_1 + policy_2 + policy_3", sep = ""),
                         family = poisson, data = country_df)
    
    # Fit null model
    null_model <- glm(paste(response_var, "~ 1", sep = ""), family = poisson, data = country_df)
    
    # Calculate deviance for the null model
    null_deviance <- null_model$deviance
    
    # Calculate deviance for the fitted model
    fitted_deviance <- poisson_model$deviance
    
    # Calculate the difference in deviance
    deviance_difference <- null_deviance - fitted_deviance
    
    # Extract relevant information and store in results_df
    results_df <- rbind(results_df, data.frame(Response_Variable = response_var,
                                               Country = country_name,
                                               Estimate = exp(coef(poisson_model)),
                                               Std_Error = summary(poisson_model)$coefficients[, "Std. Error"],
                                               Z_Value = summary(poisson_model)$coefficients[, "z value"],
                                               Pr_Z = summary(poisson_model)$coefficients[, "Pr(>|z|)"],
                                               Lower_CI = exp(confint(poisson_model))[,"(Intercept)"],
                                               Upper_CI = exp(confint(poisson_model))[,"(Intercept)"],
                                               Deviance = poisson_model$deviance,
                                               Null_Deviance = null_deviance,
                                               Fitted_Deviance = fitted_deviance,
                                               Deviance_Difference = deviance_difference))
  }
}

# Print the results
print(results_df)

# spot check

united_states_full <- read_sheet("https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", sheet = "united_states_full")

rm(poisson_model)
poisson_model <- glm(formula = new_cases ~ contact_tracing, family = poisson, data = filter(united_states_full, contact_tracing == 0))
summary(poisson_model)

poisson_model2 <- glm(new_cases ~ 0 + face_covering, family = poisson, data = filter(united_states_full, face_covering == 1))
summary(poisson_model)

poisson_model3 <- glm(new_cases ~ 0 + face_covering, family = poisson, data = filter(united_states_full, face_covering == 4))
summary(poisson_model)




# ------- MULTIVARIATE POISSON DISTRIBUTION -------- #
# Assuming pand_full is your data frame and iso_code is a column in it
# Assuming the date variable is in Date format

# Load necessary libraries
library(dplyr)

# Assuming pand_full is your data frame and iso_code is a column in it
# Assuming the date variable is in Date format

# Load necessary libraries
library(dplyr)

# List of response variables
response_variable <- "new_cases"

# List of policy variables
policy_variables <- c("testing_policy", "public_info_campaign", "contact_tracing", 
                      "face_covering", "school_closure", "work_closure", 
                      "cancel_public_events", "restrictions_on_gatherings",
                      "closed_public_transport", "stay_at_home", 
                      "restrictions_on_internal_movement", 
                      "international_travel_controls")

# Create an empty data frame to store results
results_united_states <- data.frame(policy_variable = character(),
                                    coefficient = numeric(),
                                    p_value = numeric(),
                                    stringsAsFactors = FALSE)

# Filter data for United States
united_states_data <- filter(pand_full, iso_code == "United States")

# Loop through policy variables
for (policy_variable in policy_variables) {
  tryCatch({
    # Create a formula with one-hot encoding for the categorical variable
    formula <- as.formula(paste(response_variable, "~ 0 +", policy_variable))
    
    # Fit Poisson regression model
    poisson_model <- glm(formula, family = poisson, data = united_states_data)
    
    # Extract coefficients and p-values
    coef_value <- coef(poisson_model)[policy_variable]
    p_value <- summary(poisson_model)$coefficients[policy_variable, "Pr(>|z|)"]
    
    # Append results to the data frame
    results_united_states <- rbind(results_united_states,
                                   data.frame(policy_variable = policy_variable,
                                              coefficient = coef_value,
                                              p_value = p_value,
                                              stringsAsFactors = FALSE))
  }, error = function(e) {
    # Handle errors when the model fails to converge or there are not enough results
    cat("Error for", policy_variable, ":", conditionMessage(e), "\n")
    results_united_states <- rbind(results_united_states,
                                   data.frame(policy_variable = policy_variable,
                                              coefficient = NA,
                                              p_value = NA,
                                              stringsAsFactors = FALSE))
  })
}

print(results_united_states)


library(ggplot2)