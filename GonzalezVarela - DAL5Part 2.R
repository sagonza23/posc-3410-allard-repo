# Title: POSC-3410 DAL 5 Part II ####
# Author: Sergio A. Gonzalez Varela
# Author's Email: sagonza@g.clemson.edu
# Date: 2021-03-12

# Purpose: Practice doing statistical tests in R.

# Set up ####
# Libraries
library(tidyverse)
library(stringr)
library(infer)

# Data 
load("DALs/Lecuture 13 - Probability IV/data/gss_df.Rdata")

# Instructions
# We will be practicing exploring the GSS data set. We will work one example each. You are welcome to try tgese same tests on your variables rather than the ones that I chose.

# GSS Example ####
# Theory: Support of science and technology --> Views on space exploration.

# Theoretical Null Hypothesis: A person's views on science and technology gives more opportunities to the next generation (nextgen) are NOT related to their views on supporting space exploration (intspace)

# Theoretical Alternative Hypothesis: A person's views on science and technology gives more opportunities to the next generation (nextgen) are related to their views on supporting space exploration (intspace)

# Statistical Tests
# nextgen = IV = categorical variable, non-interval = 2 independent groups (agree and disagree) <- This will require some work.

# intspace = DV = categorical variable, non-interval = 2 or more categories


# Get data in shape for analysis ####

# What do you think we will have to do to the rows? To the columns?

# Do we want something that looks roughly like this?
View(frequency_table_df)
View(chi_sq_df)

# Data Wrangling ####
analysis_df <- gss_df %>%
  # Filter to keep the rows we want
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN == "Agree" | NEXTGEN == "Strongly agree" | NEXTGEN == "Strongly disagree" | NEXTGEN == "Disagree") & (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>%
  # Keep only vars we need
  select(INTSPACE, NEXTGEN, WTSSALL) %>%
  # Now
  group_by(NEXTGEN, INTSPACE) %>%
  summarise(count = sum(WTSSALL))

# Convert strongly agree/disagree to agree or disagree, then re-aggregate
analysis_df <- analysis_df %>%
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly"),
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>%
  group_by(NEXTGEN, INTSPACE) %>%
  summarise(count = sum(count))

# Reorder factor variable INTSPACE
analysis_df <- analysis_df %>%
  mutate(INTSPACE = factor(INTSPACE))

# Add Data Represented INTSPACE == "Not at all interested"
add_rows<- tribble(
  ~NEXTGEN, ~INTSPACE, ~count,
  "agree", "Not at all interested", 1,
  "disagree", "Not at all interested", 1
)

analysis_df <- bind_rows(analysis_df, add_rows)

# Make NEXTGEN and INTSPACE a factor and arrange
analysis_df <- analysis_df %>%
  mutate(INTSPACE = factor(INTSPACE),
         NEXTGEN = factor(NEXTGEN)) %>%
  arrange(NEXTGEN)

# Create a Frequency Table by reshaping Data
frequency_table_df <- analysis_df %>%
  spread(INTSPACE, count)

# Statistical Testing ####

# Test = Chi-square
# Research Hypotheses for Chi-square
# H0: There is no relationship between variables.
# Ha: There is a relationship between variables.

# Filter values in gss for chi-square test
chi_sq_df <- gss_df %>%
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN =="Agree" | NEXTGEN == "Strongly agree" | NEXTGEN == "Strongly disagree" | NEXTGEN == "Disagree") & (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>%
  # Keep only vars we need
  select(INTSPACE, NEXTGEN, WTSSALL)

# Chi Square Test
chisq.test(table(chi_sq_df$INTSPACE, chi_sq_df$NEXTGEN))


# Copyright (c) Grant Allard, 2021
# End DAL 5 Part II.
