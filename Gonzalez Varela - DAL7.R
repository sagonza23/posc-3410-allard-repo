# Title: POSC-3410 DAL 7 Part I####
# Author: Sergio A. Gonzalez Varela
# Author's Email: sagonza@g.clemson.edu
# Date: 2021-04-02

# Purpose: Practice doing statistical tests in R.

# Set up ####
# Libraries
library(tidyverse)
library(stringr)
library(infer)
library(kableExtra)

# Data
load("DALs/Lecuture 13 - Probability IV/data/gss_df.Rdata")
library(readr)
gss_df <- read_csv("gss_df.csv")
View(gss_df)

# Instructions
# I will be using GSS data with the variables nateduc and intspace to run tests.

# GSS Example ####
# Theory: Support level of federal funding for education --> Views on space exploration.

# Theoretical Null Hypothesis: A person's views on the funding for the nation's education system (nateduc) is NOT related to their views on supporting space exploration (intspace)

# Theoretical Alternative Hypothesis: A person's views on the funding for the nation's education system (nateduc) is related to their views on supporting space exploration (intspace)

# Statistical Tests
# nateduc = IV = categorical variable, non-interval = 3 independent groups (Too little, About right, Too much) <- This will require some work.

# intspace = DV = categorical variable, non-interval = 2 or more categories


# Get data in shape for analysis ####

# What do you think we will have to do to the rows? To the columns?

# Do we want something that looks roughly like this?
View(frequency_table_df)
View(chi_sq_df)

# Data Wrangling ####
analysis_df <- gss_df %>%
  # Filter to keep the rows we want
  filter(YEAR == 2018 & !is.na(NATEDUC) & !is.na(INTSPACE) & (NATEDUC == "Too little" | NATEDUC == "About right" | NATEDUC == "Too much") & (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>%
  # Keep only vars we need
  select(INTSPACE, NATEDUC, WTSSALL) %>%
  # Now
  group_by(NATEDUC, INTSPACE) %>%
  summarise(count = sum(WTSSALL))


# Reorder factor variable INTSPACE
analysis_df <- analysis_df %>%
  mutate(INTSPACE = factor(INTSPACE))

# Make NATEDUC and INTSPACE a factor and arrange
analysis_df <- analysis_df %>%
  mutate(INTSPACE = factor(INTSPACE),
         NATEDUC = factor(NATEDUC)) %>%
  arrange(NATEDUC)

# Create a Frequency Table by reshaping Data
frequency_table_df <- analysis_df %>%
  spread(INTSPACE, count) %>%
  select(`Not at all interested`, `Moderately interested`, `Very interested`)

# Add Total Column 
frequency_table_df <- frequency_table_df %>% 
  mutate(
      Total = sum(`Not at all interested`, `Moderately interested`, `Very interested`))



# Add Total row - manually create 
total_df <- tibble(NATEDUC = "Total",
                   `Not at all interested` = 62.007223 + 186.741456 + 8.958473, 
                   `Moderately interested` = 69.31914 + 260.05874 + 16.50392, 
                   `Very interested` = 26.875418 + 150.187779 + 8.486974, 
                   Total = 158.20178 + 596.98798 + 33.94937)

# Add total row to frequency_table_df
frequency_table_df <- bind_rows(frequency_table_df, total_df)

# Round Figures Up
frequency_table_df <- frequency_table_df %>% 
  mutate(`Not at all interested` = round(`Not at all interested`), 
         `Moderately interested` = round(`Moderately interested`),
         `Very interested` = round(`Very interested`), 
         Total = round(Total))




# Kable Extra for frequency_table_df
frequency_table_df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed", "striped", "bordered"))

frequency_table_df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("condensed", "striped", "bordered")) %>% 
  column_spec(c(1, 5), bold = T) %>% 
  row_spec(4, bold=T)

frequency_table_df

# Statistical Testing ####

# Test = Chi-square
# Research Hypotheses for Chi-square
# CV: p < 0.1
# H0: There is no relationship between variables. - Rejected
# Ha: There is a relationship between variables. - Concluded

# Filter values in gss for chi-square test
chi_sq_df <- gss_df %>%
  filter(YEAR == 2018 & !is.na(NATEDUC) & !is.na(INTSPACE) & (NATEDUC =="Too little" | NATEDUC == "About right" | NATEDUC == "Too much") & (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>%
  # Keep only vars we need
  select(INTSPACE, NATEDUC, WTSSALL)

# Chi Square Test
chisq.test(table(chi_sq_df$INTSPACE, chi_sq_df$NATEDUC))



# Copyright (c) Grant Allard, 2021
# End DAL 7.