# install necessary packages
library(readr)
library(dplyr)
library(ggplot2)
library(irr)
library(tidyr)

# set working directory
setwd("/Users/mcurielinteros.ai/Documents/UMass/umass_dacss_researchdesign")

# read in data
text_analysis <- read_csv("dacss602_textanalysis.csv")

# pivot data for spreadsheet
text_analysis_pivoted <- text_analysis %>%
  mutate(value_num = 1) %>% #adding numerical column for each observation
  select("Document ID", "Coder ID", "CATEGORY", value_num) %>% #selecting columns of interest
  rename(document_id = "Document ID", coder_id = "Coder ID", category = "CATEGORY") %>% #renaming columns
  mutate(category = tolower(category)) %>% #lowercasing categories
  pivot_wider(names_from = c(category, coder_id) #grouping by category and rater
              ,values_from = value_num #selecting numerical column for each category observation
              ,values_fill = 0 #filling in nulls with 0
              ,values_fn = list(value_num = sum)) %>% #indicating that we want to sum each observation
  select(sort(names(.))) %>% #rearranging columns alphabetically
  select(document_id, everything()) #passing document_id back to the first column

# assess inter-coder reliability
kappam.light(text_analysis_pivoted[, 2:5]) #assessment per category
