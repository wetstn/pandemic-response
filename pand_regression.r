# Load necessary libraries
library(dplyr)
library(stats)
library(googlesheets4)
library(zoo)
library(ggplot2)

# Read sheet
# pand_full <- read_sheet("https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", sheet = "pand_full")

setwd("C:/Users/westo/OneDrive/Desktop/pandemic_response")

case_death <- read.csv("C:/Users/westo/OneDrive/Desktop/pandemic_response/full_data.csv")

#case_death <- read_sheet("https://docs.google.com/spreadsheets/d/12n_473Eg7neaZRAd6zXoD1sZ92mijGckNoABK8kmnr4/edit?usp=sharing", sheet = "full_data")

# List of desired countries
desired_countries <- c("Brazil", "China", "India", "Israel", "Italy", 
                        "Senegal", "South Africa", "South Korea", 
                        "United Kingdom", "United States")


# Filter the data frame
filtered_case_death <- case_death %>%
  filter(location %in% desired_countries)

case_death <- case_death %>% filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Create a list to store the individual data frames
country_dfs <- list()

# Loop through each country
for (country in desired_countries) {
  # Create a data frame for the current country
  country_df <- case_death %>%
    filter(location == country)
  
  # Assign the data frame to a variable with the format location_case_death
  assign(paste(country, "_case_death", sep = ""), country_df)
  
  # Optionally, store the data frame in the list
  country_dfs[[country]] <- country_df
}

# List of desired countries
desired_countries <- c("Brazil", "China", "India", "Israel", "Italy", 
                        "Senegal", "South Africa", "South Korea", 
                        "United Kingdom", "United States")

# Loop writing to the Google drive
for (country in desired_countries) {
  # Get the data frame for the current country
  country_df <- get(paste(country, "_case_death", sep = ""))
  
  # Write the data frame to the Google Sheet
  write_sheet(country_df,
              ss = "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing",
              sheet = paste(country, "_case_death", sep = ""))
}

# Read full set into pand_full

# List of sheet names
sheet_names <- c("brazil_full", "china_full", "india_full", "israel_full", "italy_full", 
                 "senegal_full", "south_africa_full", "south_korea_full", 
                 "united_kingdom_full", "united_states_full")

# Create an empty list to store individual data frames
full_dfs <- list()

# Loop through each sheet name
for (sheet_name in sheet_names) {
  # Read data from the Google Sheet
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", sheet = sheet_name)
  
  # Append the data frame to the list
  full_dfs[[sheet_name]] <- df
}

# Combine the individual data frames into a single data frame
pand_full <- do.call(rbind, full_dfs)

# Print or use the combined data frame
print(pand_full)

write_sheet(pand_full, ss = "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", sheet = "pand_full")
 

# ------------------------------------------------------------------------------
# Multivariate Poisson regression reults
# ------------------------------------------------------------------------------

# Cases

# Fit Poisson regression model
poisson_model_cases <- glm(new_cases_pm ~ iso_code + testing_policy + testing_policy +
              public_info_campaign + contact_tracing + face_covering + school_closure +
              work_closure + cancel_public_events + restrictions_on_gatherings +
              closed_public_transport + stay_at_home + restrictions_on_internal_movement +
              international_travel_controls,
              family = poisson, data = pand_full)

# Summarize the model
model_summary <- summary(poisson_model_cases)

# Extract coefficients, standard errors, z-values, and p-values
coefficients <- coef(model_summary)
std_errors <- coef(model_summary)[, "Std. Error"]
z_values <- coef(model_summary)[, "z value"]
p_values <- coef(model_summary)[, "Pr(>|z|)"]

# Extract variable names
variable_names <- rownames(coef(model_summary))

# Create a data frame
cases_regression_table <- data.frame(
  Variables = variable_names,
  Coefficients = coefficients,
  Standard_Errors = std_errors,
  Z_Values = z_values,
  P_Values = p_values
)

write_sheet(cases_regression_table, ss = "https://docs.google.com/spreadsheets/d/1ptV0vyyI6ZAWA3G0aGSifgi7_LeNuOMNk2a2YexrgAk/edit?usp=sharing", sheet = "cases_regression_table")


# Save residuals plot to a file
residuals_plot_cases <- plot(poisson_model_cases)
dev.copy2pdf(file = "residuals_plot_cases.pdf")

# Deaths

# Fit Poisson regression model
poisson_model_deaths <- glm(new_deaths_pm ~ iso_code + testing_policy + testing_policy +
              public_info_campaign + contact_tracing + face_covering + school_closure +
              work_closure + cancel_public_events + restrictions_on_gatherings +
              closed_public_transport + stay_at_home + restrictions_on_internal_movement +
              international_travel_controls,
              family = poisson, data = pand_full)

# Summarize the model
model_summary <- summary(poisson_model_deaths)

# Extract coefficients, standard errors, z-values, and p-values
coefficients <- coef(model_summary)
std_errors <- coef(model_summary)[, "Std. Error"]
z_values <- coef(model_summary)[, "z value"]
p_values <- coef(model_summary)[, "Pr(>|z|)"]

# Extract variable names
variable_names <- rownames(coef(model_summary))

# Create a data frame
deaths_regression_table <- data.frame(
  Variables = variable_names,
  Coefficients = coefficients,
  Standard_Errors = std_errors,
  Z_Values = z_values,
  P_Values = p_values
)

write_sheet(deaths_regression_table, ss = "https://docs.google.com/spreadsheets/d/1ptV0vyyI6ZAWA3G0aGSifgi7_LeNuOMNk2a2YexrgAk/edit?usp=sharing", sheet = "deaths_regression_table")

# Save residuals plot to a file
residuals_plot_deaths <- plot(poisson_model_deaths)
dev.copy2pdf(file = "residuals_plot_deaths.pdf")

# ------------------------------------------------------------------------------
# Normalized Max-Min Plots Over Time By Country
# ------------------------------------------------------------------------------

# List of countries
countries <- c("Brazil", "China", "India", "Israel", "Italy", "Senegal", "South_Africa", "South_Korea", "United_Kingdom", "United_States")

# Filter data for the specified countries
filtered_pand_full <- pand_full[pand_full$iso_code %in% countries, ]

# New Cases

# Filter data to exclude values more than 3 standard deviations from each country's mean
sd3_filtered_cases_data <- filtered_pand_full %>%
  group_by(iso_code) %>%
  mutate(mean_value = mean(new_cases_pm),
         sd_value = sd(new_cases_pm),
         filtered_value = ifelse(abs(new_cases_pm - mean_value) <= 3 * sd_value, new_cases_pm, NA))

# Normalize the remaining values using min-max scaling
sd3_filtered_cases_data <- sd3_filtered_cases_data %>%
  group_by(iso_code) %>%
  mutate(min_max_scaled = (filtered_value - min(filtered_value, na.rm = TRUE)) / (max(filtered_value, na.rm = TRUE) - min(filtered_value, na.rm = TRUE)))

# Plot the filtered and normalized data using ggplot with only the smoother lines of best fit
sd3_filtered_cases_data_plot <- ggplot(sd3_filtered_cases_data, aes(x = date, y = min_max_scaled, color = iso_code)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +  # Add a smoother line of best fit
  labs(title = "Confirmed Cases Over Time by Country (Normalized)",
       x = "Date",
       y = "Confirmed Cases per Day (Normalized)",
       color = "Country") +
  theme_minimal() +
  theme(
    aspect.ratio = 1/3,  # Set aspect ratio to 1:3
    panel.grid = element_blank(),  # Remove grid
    panel.background = element_rect(color = "black", fill = NA, size = 1),  # Add black border to the plot area
    plot.background = element_rect(fill = "white"),  # Set plot background color
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margin for better visibility of the border
  )

ggsave("sd3_filtered_cases_data_plot.png", plot = sd3_filtered_cases_data_plot, dpi = 300)

# New Deaths

# Reject large outliers > 3SD and apply Min-Max scaling 
# Filter data to exclude values more than 3 standard deviations from each country's mean
sd3_filtered_deaths_data <- filtered_pand_full %>%
  group_by(iso_code) %>%
  mutate(mean_value = mean(new_deaths_pm),
         sd_value = sd(new_deaths_pm),
         filtered_value = ifelse(abs(new_deaths_pm - mean_value) <= 3 * sd_value, new_deaths_pm, NA))

# Normalize the remaining values using min-max scaling
sd3_filtered_deaths_data <- sd3_filtered_deaths_data %>%
  group_by(iso_code) %>%
  mutate(min_max_scaled = (filtered_value - min(filtered_value, na.rm = TRUE)) / (max(filtered_value, na.rm = TRUE) - min(filtered_value, na.rm = TRUE)))

# Plot the filtered and normalized data using ggplot with only the smoother lines of best fit
sd3_filtered_deaths_data_plot <- ggplot(sd3_filtered_deaths_data, aes(x = date, y = min_max_scaled, color = iso_code)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +  # Add a smoother line of best fit
  labs(title = "Deaths Over Time by Country (Normalized)",
       x = "Date",
       y = "Deaths per Day (Normalized)",
       color = "Country") +
  theme_minimal() +
  theme(
    aspect.ratio = 1/3,  # Set aspect ratio to 1:3
    panel.grid = element_blank(),  # Remove grid
    panel.background = element_rect(color = "black", fill = NA, size = 1),  # Add black border to the plot area
    plot.background = element_rect(fill = "white"),  # Set plot background color
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margin for better visibility of the border
  )

print(sd3_filtered_deaths_data_plot)
ggsave("sd3_filtered_deaths_data_plot.png", plot = sd3_filtered_deaths_data_plot, dpi = 300)

# New Hospital Admissions -------------------------------------------------
# Reject large outliers >3 SD and apply Min-Max scaling -----
# Filter data to exclude values more than 3 standard deviations from each country's mean
sd3_filtered_hosp_admit_data <- filtered_pand_full %>%
  group_by(iso_code) %>%
  mutate(mean_value = mean(weekly_new_admissions_pm),
         sd_value = sd(weekly_new_admissions_pm),
         filtered_value = ifelse(abs(weekly_new_admissions_pm - mean_value) <= 3 * sd_value, weekly_new_admissions_pm, NA))

# Normalize the remaining values using min-max scaling
sd3_filtered_hosp_admit_data <- sd3_filtered_hosp_admit_data %>%
  group_by(iso_code) %>%
  mutate(min_max_scaled = (filtered_value - min(filtered_value, na.rm = TRUE)) / (max(filtered_value, na.rm = TRUE) - min(filtered_value, na.rm = TRUE)))

# Plot the filtered and normalized data using ggplot with only the smoother lines of best fit
sd3_filtered_hosp_admit_data_plot <- ggplot(sd3_filtered_hosp_admit_data, aes(x = date, y = min_max_scaled, color = iso_code)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +  # Add a smoother line of best fit
  labs(title = "Hospital Admissions Over Time by Country (Normalized)",
       x = "Date",
       y = "Hospital Admissions per Day (Normalized)",
       color = "Country") +
  theme_minimal() +
  theme(
    aspect.ratio = 1/3,  # Set aspect ratio to 1:3
    panel.grid = element_blank(),  # Remove grid
    panel.background = element_rect(color = "black", fill = NA, size = 1),  # Add black border to the plot area
    plot.background = element_rect(fill = "white"),  # Set plot background color
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margin for better visibility of the border
  )

print(sd3_filtered_hosp_admit_data_plot)
ggsave("sd3_filtered_hosp_admit_data_plot.png", plot = sd3_filtered_hosp_admit_data_plot, dpi = 300)


# Notes

# Create dummy variables for policy_1, policy_2, and policy_3
dummy_matrix <- model.matrix(~ policy_1 + policy_2 + policy_3 - 1, data = df)

# Combine the dummy variables with the response variable
model_data <- cbind(df$response_variable, dummy_matrix)

# Fit logistic Poisson regression
poisson_model <- glm(response_variable ~ ., family = poisson, data = model_data)

# Print the summary
summary(poisson_model)