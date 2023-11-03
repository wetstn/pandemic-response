#-----------------------------------------------------------
# PANDEMIC RESPONSE
#-----------------------------------------------------------
# Install and load required packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("httpuv")
install.packages("googlesheets4")
library(tidyverse)
library(dplyr)
library(httpuv)
library(googlesheets4)

# Set working directory
setwd("C:/Users/westo/OneDrive/Desktop/pandemic_response")

#-----------------------------------------------------------
# CASES
#-----------------------------------------------------------
# New cases per million
new_cases_per_million <-
  read_sheet("https://docs.google.com/spreadsheets/d/1VVoSUsjOcpujf4Dmv_xkbLnvfnDFrrRnbpCexhAO6Xs/edit?usp=sharing", 
                                   sheet = "new_cases_per_million")

# Specify the column numbers to keep
selected_columns <- c(1, #date
                      33, #Brazil
                      47, #China
                      102, #India
                      108, #Israel
                      109, #Italy
                      197, #Senegal
                      207, #South Africa
                      209, #South Korea
                      234, #United Kingdom
                      235 #United States
                      )

# Use select() to filter the data frame by column numbers
filtered_new_cases_per_million <- new_cases_per_million %>%
  select(selected_columns)

# Filter to data between Jan 01 2020 to July 01 2020
filtered_new_cases_per_million <- filtered_new_cases_per_million %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Rename columns
new_cases_pm <- 
  filtered_new_cases_per_million %>%
  rename(BRA_new_cases_pm = BRA,
         CHN_new_cases_pm = CHN,
         IND_new_cases_pm = IND,
         ISR_new_cases_pm = ISR,
         ITA_new_cases_pm = ITA,
         SEN_new_cases_pm = SEN,
         ZAF_new_cases_pm = ZAF,
         KOR_new_cases_pm = KOR,
         GBR_new_cases_pm = GBR,
         USA_new_cases_pm = USA)

# Write sheet to drive
write_sheet(new_cases_pm, ss = 
  "https://docs.google.com/spreadsheets/d/1VVoSUsjOcpujf4Dmv_xkbLnvfnDFrrRnbpCexhAO6Xs/edit?usp=sharing", 
  sheet = "new_cases_pm")

#-----------------------------------------------------------
# Total cases per million
total_cases_per_million <-
  read_sheet("https://docs.google.com/spreadsheets/d/1JGt4DR6GH_lra3WPCnVlxdSDf8I4hciIufKn0-rl6Qk/edit?usp=sharing", 
                                   sheet = "total_cases_per_million")

# Use select() to filter the data frame by column numbers
filtered_total_cases_per_million <- total_cases_per_million %>%
  select(selected_columns)

# Filter to data between Jan 01 2020 to July 01 2020
filtered_total_cases_per_million <- filtered_total_cases_per_million %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Rename columns
total_cases_pm <- 
  filtered_total_cases_per_million %>%
  rename(BRA_total_cases_pm = BRA,
         CHN_total_cases_pm = CHN,
         IND_total_cases_pm = IND,
         ISR_total_cases_pm = ISR,
         ITA_total_cases_pm = ITA,
         SEN_total_cases_pm = SEN,
         ZAF_total_cases_pm = ZAF,
         KOR_total_cases_pm = KOR,
         GBR_total_cases_pm = GBR,
         USA_total_cases_pm = USA)

# Write sheet to drive
write_sheet(total_cases_pm, ss = 
              "https://docs.google.com/spreadsheets/d/1JGt4DR6GH_lra3WPCnVlxdSDf8I4hciIufKn0-rl6Qk/edit?usp=sharing", 
            sheet = "total_cases_pm")

#-----------------------------------------------------------
# DEATHS
#-----------------------------------------------------------
# New deaths per million
new_deaths_per_million <-
  read_sheet("https://docs.google.com/spreadsheets/d/1K6ibzFjXlw7nKOw0sLyC95e70djDhFiNJrqHh-XHDqg/edit?usp=sharing", 
             sheet = "new_deaths_per_million")

# Use select() to filter the data frame by column numbers
filtered_new_deaths_per_million <- new_deaths_per_million %>%
  select(selected_columns)

# Filter to data between Jan 01 2020 to July 01 2020
filtered_new_deaths_per_million <- filtered_new_deaths_per_million %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Rename columns
new_deaths_pm <- 
  filtered_new_deaths_per_million %>%
  rename(BRA_new_deaths_pm = BRA,
         CHN_new_deaths_pm = CHN,
         IND_new_deaths_pm = IND,
         ISR_new_deaths_pm = ISR,
         ITA_new_deaths_pm = ITA,
         SEN_new_deaths_pm = SEN,
         ZAF_new_deaths_pm = ZAF,
         KOR_new_deaths_pm = KOR,
         GBR_new_deaths_pm = GBR,
         USA_new_deaths_pm = USA)

# Write sheet to drive
write_sheet(new_deaths_pm, ss = 
              "https://docs.google.com/spreadsheets/d/1K6ibzFjXlw7nKOw0sLyC95e70djDhFiNJrqHh-XHDqg/edit?usp=sharing", 
            sheet = "new_deaths_pm")

#-----------------------------------------------------------
# Total deaths per million
total_deaths_per_million <-
  read_sheet("https://docs.google.com/spreadsheets/d/1cUpG6ibQ79Wv4rOXCMZAsDYiBVLhOHRw6_6QJbBvomA/edit?usp=sharing", 
             sheet = "total_deaths_per_million")

# Use select() to filter the data frame by column numbers
filtered_total_deaths_per_million <- total_deaths_per_million %>%
  select(selected_columns)

# Filter to data between Jan 01 2020 to July 01 2020
filtered_total_deaths_per_million <- filtered_total_deaths_per_million %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Rename columns
total_deaths_pm <- 
  filtered_total_deaths_per_million %>%
  rename(BRA_total_deaths_pm = BRA,
         CHN_total_deaths_pm = CHN,
         IND_total_deaths_pm = IND,
         ISR_total_deaths_pm = ISR,
         ITA_total_deaths_pm = ITA,
         SEN_total_deaths_pm = SEN,
         ZAF_total_deaths_pm = ZAF,
         KOR_total_deaths_pm = KOR,
         GBR_total_deaths_pm = GBR,
         USA_total_deaths_pm = USA)

# Write sheet to drive
write_sheet(total_deaths_pm, ss = 
              "https://docs.google.com/spreadsheets/d/1cUpG6ibQ79Wv4rOXCMZAsDYiBVLhOHRw6_6QJbBvomA/edit?usp=sharing", 
            sheet = "total_deaths_pm")

#-----------------------------------------------------------
# TESTING
#-----------------------------------------------------------
# Read testing data
# country_testing_all_observations <-read.csv("covid-testing-all-observations.csv")
covid_testing_all_observations <-read_sheet("https://docs.google.com/spreadsheets/d/1z0iZwumdzi4_V1lYmLK74D-SoyQDzjPMLFxTPTEPmsk/edit?usp=sharing", 
                                            sheet = "covid_testing_all_observations")

# Filter to the 10 countries of interest
filtered_country_testing <- 
  covid_testing_all_observations %>%
  filter(iso_code %in% c("BRA", "CHN", "IND", "ISR", "ITA", "SEN", "ZAF", "KOR", "GBR", "USA"))

# Convert tests per thousand to tests per million
filtered_country_testing <- filtered_country_testing %>%
  mutate(cumulative_total_pm = cumulative_total_per_thousand * 1000)

filtered_country_testing <- filtered_country_testing %>%
  mutate(delta_cumulative_total_pm = delta_cumulative_total_per_thousand * 1000)

# filter to keep only columns iso_code, date, cumulative_total_pm, delta_cumulative_total 
filtered_country_testing <- filtered_country_testing %>%
  select(iso_code, date, cumulative_total, delta_cumulative_total,
         cumulative_total_per_thousand, delta_cumulative_total_per_thousand,
         cumulative_total_pm, delta_cumulative_total_pm, short_term_positive_rate)

# Filter to data between Jan 01 2020 to July 01 2020
country_tests <- filtered_country_testing %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Reshape table
country_tests <- country_tests %>%
  pivot_wider(
    id_cols = date,
    names_from = iso_code,
    values_from = c(cumulative_total, delta_cumulative_total,
                    cumulative_total_per_thousand, delta_cumulative_total_per_thousand,
                    cumulative_total_pm, delta_cumulative_total_pm,
                    short_term_positive_rate),
    names_glue = "{iso_code}_{.value}"
  )

# Ensure data prints with dates in ascending order
country_tests <- country_tests %>%
  arrange(date)

# Write to Google Drive
write_sheet(country_tests, ss = 
  "https://docs.google.com/spreadsheets/d/1z0iZwumdzi4_V1lYmLK74D-SoyQDzjPMLFxTPTEPmsk/edit?usp=sharing", 
  sheet = "country_tests")

#-----------------------------------------------------------
# Hospitalizations
#-----------------------------------------------------------
# Read testing data
covid_hospitalizations <-read_sheet("https://docs.google.com/spreadsheets/d/1_B0oEj87fioR7nemcQpLEx04E2Tb-bAlqoY6Y27BNiI/edit?usp=sharing", 
                                            sheet = "covid_hospitalizations")

# Filter to the 10 countries of interest
filtered_covid_hospitalizations <- 
  covid_hospitalizations %>%
  filter(iso_code %in% c("BRA", "CHN", "IND", "ISR", "ITA", "SEN", "ZAF", "KOR", "GBR", "USA"))

# Filter to data between Jan 01 2020 to July 01 2020
filtered_covid_hospitalizations <- filtered_covid_hospitalizations %>%
  filter(date >= as.POSIXct("2020-01-01") & date <= as.POSIXct("2020-07-01"))

# Recode data
filtered_covid_hospitalizations <- 
  filtered_covid_hospitalizations %>%
  mutate(indicator = recode(indicator, 
                            "Daily hospital occupancy" = "daily_hospital_occupancy", 
                            "Daily hospital occupancy per million" = "daily_hospital_occupancy_pm",
                            "Daily ICU occupancy" = "daily_icu_occupancy",
                            "Daily ICU occupancy per million" = "daily_icu_occupancy_pm",
                            "Weekly new hospital admissions" = "weekly_new_admissions",
                            "Weekly new hospital admissions per million" = "weekly_new_admissions_pm",
                            "Weekly new ICU admissions" = "weekly_new_icu_admissions",
                            "Weekly new ICU admissions per million" = "weekly_new_icu_admissions_pm"))

# Reshape the data
country_hospitalizations <- 
  filtered_covid_hospitalizations %>%
  pivot_wider(
    id_cols = date,
    names_from = c(iso_code, indicator),
    values_from = value,
    names_glue = "{iso_code}_{indicator}"
  )

# Write to Google Drive
write_sheet(country_hospitalizations, ss = 
              "https://docs.google.com/spreadsheets/d/1_B0oEj87fioR7nemcQpLEx04E2Tb-bAlqoY6Y27BNiI/edit?usp=sharing", 
            sheet = "country_hospitalizations")

#-----------------------------------------------------------
# Full pandemic response data frame
#-----------------------------------------------------------
# Merge all data sets together and index by date
pand_df <- merge(new_cases_pm, total_cases_pm, by = "date", all = TRUE)
pand_df <- merge(pand_df, country_tests, by = "date", all = TRUE)
pand_df <- merge(pand_df, country_hospitalizations, by = "date", all = TRUE)

# Write to Google Drive
write_sheet(pand_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "pand_df")

#-----------------------------------------------------------
# Pandemic response data frames by country
#-----------------------------------------------------------
# Brazil
brazil_df <- pand_df %>%
  select(
    date,
    BRA_new_cases_pm,
    BRA_total_cases_pm,
    BRA_delta_cumulative_total,
    BRA_cumulative_total,
    BRA_cumulative_total_per_thousand,
    BRA_delta_cumulative_total_per_thousand,
    BRA_cumulative_total_pm,
    BRA_delta_cumulative_total_pm,
    BRA_short_term_positive_rate,
  )

# Add missing variables
brazil_df <- brazil_df %>%
  mutate(
    BRA_weekly_new_admissions = NA,
    BRA_weekly_new_admissions_pm = NA,
    BRA_daily_hospital_occupancy = NA,
    BRA_daily_hospital_occupancy_pm = NA,
    BRA_weekly_new_icu_admissions = NA,
    BRA_weekly_new_icu_admissions_pm = NA,
    BRA_daily_icu_occupancy = NA,
    BRA_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(brazil_df)[2:ncol(brazil_df)] <- 
  substring(colnames(brazil_df)[2:ncol(brazil_df)], 5)

# Add country ISO code
brazil_df <- brazil_df %>%
  mutate(
    iso_code = "Brazil")

#Order columns
brazil_df <- brazil_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(brazil_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "brazil_df")
#-----------------------------------------------------------
# China
china_df <- pand_df %>%
  select(
    date,
    CHN_delta_cumulative_total,
    CHN_cumulative_total,
    CHN_new_cases_pm,
    CHN_total_cases_pm,
    CHN_cumulative_total_per_thousand,
    CHN_delta_cumulative_total_per_thousand,
    CHN_cumulative_total_pm,
    CHN_delta_cumulative_total_pm,
    CHN_short_term_positive_rate,
  )

# Add missing variables
china_df <- china_df %>%
  mutate(
    CHN_weekly_new_admissions = NA,
    CHN_weekly_new_admissions_pm = NA,
    CHN_daily_hospital_occupancy = NA,
    CHN_daily_hospital_occupancy_pm = NA,
    CHN_weekly_new_icu_admissions = NA,
    CHN_weekly_new_icu_admissions_pm = NA,
    CHN_daily_icu_occupancy = NA,
    CHN_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(china_df)[2:ncol(china_df)] <- 
  substring(colnames(china_df)[2:ncol(china_df)], 5)

# Add country ISO code
china_df <- china_df %>%
  mutate(
    iso_code = "China")

#Order columns
china_df <- china_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(china_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "china_df")
#-----------------------------------------------------------
# India
india_df <- pand_df %>%
  select(
    date,
    IND_delta_cumulative_total,
    IND_cumulative_total,
    IND_new_cases_pm,
    IND_total_cases_pm,
    IND_cumulative_total_per_thousand,
    IND_delta_cumulative_total_per_thousand,
    IND_cumulative_total_pm,
    IND_delta_cumulative_total_pm,
    IND_short_term_positive_rate,
  )

# Add missing variables
india_df <- india_df %>%
  mutate(
    IND_weekly_new_admissions = NA,
    IND_weekly_new_admissions_pm = NA,
    IND_daily_hospital_occupancy = NA,
    IND_daily_hospital_occupancy_pm = NA,
    IND_weekly_new_icu_admissions = NA,
    IND_weekly_new_icu_admissions_pm = NA,
    IND_daily_icu_occupancy = NA,
    IND_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(india_df)[2:ncol(india_df)] <- 
  substring(colnames(india_df)[2:ncol(india_df)], 5)

# Add country ISO code
india_df <- india_df %>%
  mutate(
    iso_code = "India")

#Order columns
india_df <- india_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(india_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "india_df")
#-----------------------------------------------------------
# Israel
israel_df <- pand_df %>%
  select(
    date,
    ISR_delta_cumulative_total,
    ISR_cumulative_total,
    ISR_new_cases_pm,
    ISR_total_cases_pm,
    ISR_cumulative_total_per_thousand,
    ISR_delta_cumulative_total_per_thousand,
    ISR_cumulative_total_pm,
    ISR_delta_cumulative_total_pm,
    ISR_short_term_positive_rate,
    ISR_weekly_new_admissions,
    ISR_weekly_new_admissions_pm,
    ISR_daily_hospital_occupancy,
    ISR_daily_hospital_occupancy_pm,
    ISR_weekly_new_icu_admissions,
    ISR_weekly_new_icu_admissions_pm,
  )

# Add missing variables
israel_df <- israel_df %>%
  mutate(
    ISR_daily_icu_occupancy = NA,
    ISR_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(israel_df)[2:ncol(israel_df)] <- 
  substring(colnames(israel_df)[2:ncol(israel_df)], 5)

# Add country ISO code
israel_df <- israel_df %>%
  mutate(
    iso_code = "Israel")

#Order columns
israel_df <- israel_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(israel_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "israel_df")
#-----------------------------------------------------------
# Italy
italy_df <- pand_df %>%
  select(
    date,
    ITA_delta_cumulative_total,
    ITA_cumulative_total,
    ITA_new_cases_pm,
    ITA_total_cases_pm,
    ITA_cumulative_total_per_thousand,
    ITA_delta_cumulative_total_per_thousand,
    ITA_cumulative_total_pm,
    ITA_delta_cumulative_total_pm,
    ITA_short_term_positive_rate,
    ITA_weekly_new_admissions,
    ITA_weekly_new_admissions_pm,
    ITA_daily_hospital_occupancy,
    ITA_daily_hospital_occupancy_pm,
    ITA_daily_icu_occupancy,
    ITA_daily_icu_occupancy_pm
  )

# Add missing variables
italy_df <- italy_df %>%
  mutate(
    ITA_weekly_new_icu_admissions = NA,
    ITA_weekly_new_icu_admissions_pm = NA
  )

# Rename variables
colnames(italy_df)[2:ncol(italy_df)] <- 
  substring(colnames(italy_df)[2:ncol(italy_df)], 5)

# Add country ISO code
italy_df <- italy_df %>%
  mutate(
    iso_code = "Italy")

#Order columns
italy_df <- italy_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(italy_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "italy_df")
#-----------------------------------------------------------
# Senegal
senegal_df <- pand_df %>%
  select(
    date,
    SEN_delta_cumulative_total,
    SEN_cumulative_total,
    SEN_new_cases_pm,
    SEN_total_cases_pm,
    SEN_cumulative_total_per_thousand,
    SEN_delta_cumulative_total_per_thousand,
    SEN_cumulative_total_pm,
    SEN_delta_cumulative_total_pm,
    SEN_short_term_positive_rate,
  )

# Add missing variables
senegal_df <- senegal_df %>%
  mutate(
    SEN_weekly_new_admissions = NA,
    SEN_weekly_new_admissions_pm = NA,
    SEN_daily_hospital_occupancy = NA,
    SEN_daily_hospital_occupancy_pm = NA,
    SEN_weekly_new_icu_admissions = NA,
    SEN_weekly_new_icu_admissions_pm = NA,
    SEN_daily_icu_occupancy = NA,
    SEN_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(senegal_df)[2:ncol(senegal_df)] <- 
  substring(colnames(senegal_df)[2:ncol(senegal_df)], 5)

# Add country ISO code
senegal_df <- senegal_df %>%
  mutate(
    iso_code = "Senegal")

#Order columns
senegal_df <- senegal_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(senegal_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "senegal_df")
#-----------------------------------------------------------
# South Africa
south_africa_df <- pand_df %>%
  select(
    date,
    ZAF_delta_cumulative_total,
    ZAF_cumulative_total,
    ZAF_new_cases_pm,
    ZAF_total_cases_pm,
    ZAF_cumulative_total_per_thousand,
    ZAF_delta_cumulative_total_per_thousand,
    ZAF_cumulative_total_pm,
    ZAF_delta_cumulative_total_pm,
    ZAF_short_term_positive_rate,
    ZAF_weekly_new_admissions,
    ZAF_weekly_new_admissions_pm,
    ZAF_daily_hospital_occupancy,
    ZAF_daily_hospital_occupancy_pm,
    ZAF_daily_icu_occupancy,
    ZAF_daily_icu_occupancy_pm
  )

# Add missing variables
south_africa_df <- south_africa_df %>%
  mutate(
    ZAF_weekly_new_icu_admissions = NA,
    ZAF_weekly_new_icu_admissions_pm = NA
  )

# Rename variables
colnames(south_africa_df)[2:ncol(south_africa_df)] <- 
  substring(colnames(south_africa_df)[2:ncol(south_africa_df)], 5)

# Add country ISO code
south_africa_df <- south_africa_df %>%
  mutate(
    iso_code = "South_Africa")

#Order columns
south_africa_df <- south_africa_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(south_africa_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "south_africa_df")
#-----------------------------------------------------------
# South Korea
south_korea_df <- pand_df %>%
  select(
    date,
    KOR_delta_cumulative_total,
    KOR_cumulative_total,
    KOR_new_cases_pm,
    KOR_total_cases_pm,
    KOR_cumulative_total_per_thousand,
    KOR_delta_cumulative_total_per_thousand,
    KOR_cumulative_total_pm,
    KOR_delta_cumulative_total_pm,
    KOR_short_term_positive_rate,
    KOR_daily_icu_occupancy,
    KOR_daily_icu_occupancy_pm
  )

# Add missing variables
south_korea_df <- south_korea_df %>%
  mutate(
    KOR_weekly_new_admissions = NA,
    KOR_weekly_new_admissions_pm = NA,
    KOR_daily_hospital_occupancy = NA,
    KOR_daily_hospital_occupancy_pm = NA,
    KOR_weekly_new_icu_admissions = NA,
    KOR_weekly_new_icu_admissions_pm = NA,
  )

# Rename variables
colnames(south_korea_df)[2:ncol(south_korea_df)] <- 
  substring(colnames(south_korea_df)[2:ncol(south_korea_df)], 5)

# Add country ISO code
south_korea_df <- south_korea_df %>%
  mutate(
    iso_code = "South_Korea")

#Order columns
south_korea_df <- south_korea_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(south_korea_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "south_korea_df")
#-----------------------------------------------------------
# United Kingom
united_kingdom_df <- pand_df %>%
  select(
    date,
    GBR_delta_cumulative_total,
    GBR_cumulative_total,
    GBR_new_cases_pm,
    GBR_total_cases_pm,
    GBR_cumulative_total_per_thousand,
    GBR_delta_cumulative_total_per_thousand,
    GBR_cumulative_total_pm,
    GBR_delta_cumulative_total_pm,
    GBR_short_term_positive_rate,
    GBR_weekly_new_admissions,
    GBR_weekly_new_admissions_pm,
    GBR_daily_hospital_occupancy,
    GBR_daily_hospital_occupancy_pm,
    GBR_daily_icu_occupancy,
    GBR_daily_icu_occupancy_pm
  )

# Add missing variables
united_kingdom_df <- united_kingdom_df %>%
  mutate(
    GBR_weekly_new_icu_admissions = NA,
    GBR_weekly_new_icu_admissions_pm = NA
  )

# Rename variables
colnames(united_kingdom_df)[2:ncol(united_kingdom_df)] <- 
  substring(colnames(united_kingdom_df)[2:ncol(united_kingdom_df)], 5)

# Add country ISO code
united_kingdom_df <- united_kingdom_df %>%
  mutate(
    iso_code = "United_Kingdom")

#Order columns
united_kingdom_df <- united_kingdom_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(united_kingdom_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "united_kingdom_df")
#-----------------------------------------------------------
# United States
united_states_df <- pand_df %>%
  select(
    date,
    USA_delta_cumulative_total,
    USA_cumulative_total,
    USA_new_cases_pm,
    USA_total_cases_pm,
    USA_cumulative_total_per_thousand,
    USA_delta_cumulative_total_per_thousand,
    USA_cumulative_total_pm,
    USA_delta_cumulative_total_pm,
    USA_short_term_positive_rate
  )

# Add missing variables
united_states_df <- united_states_df %>%
  mutate(
    USA_weekly_new_icu_admissions = NA,
    USA_weekly_new_icu_admissions_pm = NA,
    USA_weekly_new_admissions = NA,
    USA_weekly_new_admissions_pm = NA,
    USA_daily_hospital_occupancy = NA,
    USA_daily_hospital_occupancy_pm = NA,
    USA_daily_icu_occupancy = NA,
    USA_daily_icu_occupancy_pm = NA
  )

# Rename variables
colnames(united_states_df)[2:ncol(united_states_df)] <- 
  substring(colnames(united_states_df)[2:ncol(united_states_df)], 5)

# Add country ISO code
united_states_df <- united_states_df %>%
  mutate(
    iso_code = "United_States")

#Order columns
united_states_df <- united_states_df %>%
  select(
    date,
    iso_code,
    delta_cumulative_total,
    cumulative_total,
    new_cases_pm,
    total_cases_pm,
    cumulative_total_per_thousand,
    delta_cumulative_total_per_thousand,
    cumulative_total_pm,
    delta_cumulative_total_pm,
    short_term_positive_rate,
    weekly_new_admissions,
    weekly_new_admissions_pm,
    daily_hospital_occupancy,
    daily_hospital_occupancy_pm,
    weekly_new_icu_admissions,
    weekly_new_icu_admissions_pm,
    daily_icu_occupancy,
    daily_icu_occupancy_pm
  )

# Write to Google Drive
write_sheet(united_states_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "united_states_df")
#-----------------------------------------------------------
# Combined data frame
global_pandemic_response_df <- bind_rows(
  brazil_df,
  china_df,
  india_df,
  israel_df,
  italy_df,
  senegal_df,
  south_africa_df,
  south_korea_df,
  united_kingdom_df,
  united_states_df
)

write_sheet(global_pandemic_response_df, ss = 
              "https://docs.google.com/spreadsheets/d/1o9Ndo9XWtpy85F4lWTJrHhzfsPzY-duTKvja15FkzxM/edit?usp=sharing", 
            sheet = "global_pandemic_response_df")