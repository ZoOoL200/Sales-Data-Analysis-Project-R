# =========================================================
# Data Cleaning Script
# Project: Retail Sales Analysis
# Author: Ahmed Rabie
# =========================================================


# Load Libraries -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# Define Paths -------------------------------------------------------------
  
raw_data_path <- "./data/raw/train.csv"
processed_path <- "./data/processed/"

# Import Data --------------------------------------------------------------

df_raw <- read_csv(raw_data_path)

# Quick Data Inspection ----------------------------------------------------

# View first rows
head(df_raw)

# Data structure
glimpse(df_raw)

# Dataset dimensions
dim(df_raw)

# How R parsed the columns
spec(df_raw)

# Create Clean Dataset -----------------------------------------------------

df_clean <- df_raw %>%
  clean_names()

names(df_clean)

# Remove Non-Analytical Columns --------------------------------------------

# row_id is only an identifier and does not add analytical value
df_clean <- df_clean %>%
  select(-row_id)

# Check for Duplicates -----------------------------------------------------

num_duplicates <- sum(duplicated(df_clean))
num_duplicates

# Inspect duplicated rows (if needed)
duplicate_rows <- df_clean %>%
  filter(duplicated(.))

# Remove duplicates
df_clean <- df_clean %>%
  distinct()

# Missing Values Inspection ------------------------------------------------

missing_values <- colSums(is.na(df_clean))
missing_values

# Note:
# Missing values are minimal and do not affect key variables,
# therefore no imputation is required.

# Data Standardization ----------------------------------------------------

glimpse(df_clean)

df_clean <- df_clean %>% 
  mutate(
    order_date = dmy(order_date),
    ship_date = dmy(ship_date),
    
    ship_mode = as.factor(ship_mode),
    segment = as.factor(segment),
    region = as.factor(region),
    category = as.factor(category))

# Confirm Structure After Cleaning
glimpse(df_clean)



# Save Cleaned Data -------------------------------------------------------

write_csv(df_clean, "./data/processed/data_cleaned.csv")

saveRDS(df_clean, "./data/processed/data_cleaned.rds")

# Cleaning Completed -------------------------------------------------------

message("Data cleaning completed successfully.")

