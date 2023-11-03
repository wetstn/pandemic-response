#-----------------------------------------------------------
# PANDEMIC RESPONSE
#-----------------------------------------------------------
# Install and load required packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("httpuv")
install.packages("googlesheets4")
install.packages("lmtest")
library(tidyverse)
library(dplyr)
library(httpuv)
library(googlesheets4)
library(lmtest)

# Assuming your data frame is named df
# and you have columns: date, cases (or deaths, tests), policy (binary), and time

# Load necessary library
library(lmtest)

# Fit OLS regression model
model <- lm(cases ~ policy + time, data = df)

# Perform hypothesis tests
test_policy <- coef_test(model, hypothesis = "policy = 0")  # Test the effect of policy
test_time <- coef_test(model, hypothesis = "time = 0")  # Test the effect of time

# View results
summary(model)
print(test_policy)
print(test_time)