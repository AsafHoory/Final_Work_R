# Final Work
# Name of submitter: Asaf Hoory
# ID: 211677174


rm(list = ls())  # Clear workspace

# Load required libraries
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(pROC)
library(knitr)
library(scales)
#### EXPLORATORY DATA ----

# This part of the code preprocesses the data and extracts only the relevant variables.
# WMrep_acc describes accuracy in the main task, PASkey.keys describes the PAS report, and contrast represents the contrast level.
# Each participant completed 720 trials divided into two sessions. Hence, df consists of 14,400 lines.

# Creating data frame of all 20 subjects
folder_path <- "C:/Users/LENOVO/Desktop/R/Final_Work/data"

files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

df <- files |> 
  map_df(~ read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE) |>  
           mutate(file_name = basename(.x)))

# Ensure all required columns exist
required_columns <- c("contrast", "WMresp_acc", "PASkey.keys", "session", "participant")
missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop("Missing columns in dataset: ", paste(missing_columns, collapse = ", "))
}

# Select relevant columns and remove rows with missing values
df <- df |> 
  select(all_of(required_columns)) |> 
  filter(!if_any(everything(), is.na))  

# Fix participant naming
df <- df |> 
  mutate(participant = recode(participant, "P1" = "1")) |>  
  mutate(participant = as.numeric(participant)) |>  # Convert to numeric  
  arrange(participant)  

# Convert multiple columns to numeric
df <- df |> 
  mutate(WMresp_acc = as.numeric(WMresp_acc)) |>  
  mutate(PASkey.keys = as.numeric(PASkey.keys)) |>#PAS temporarily defined as numeric because I wanted to calculate mean and SE in the descriptive part, in the regression it will be change to factor
  mutate(contrast = as.numeric(contrast))  



# No need to remove duplicate rows because in this dataset, 
# identical rows are a natural part of the data structure.
# Each participant completed multiple trials, and some trials 
# may have the exact same values for contrast, WMresp_acc, and PASkey.keys.
# These repetitions do not indicate redundant data but rather reflect 
# the actual design of the experiment.

