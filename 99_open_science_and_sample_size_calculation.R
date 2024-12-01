

# Supplement: Open size and sample size calculation
# Borg DN
# November 2024

# Libraries
library(tidyverse)
library(readxl)
library(janitor)

# Load data
d <- read_xlsx('data/data_open_science.xlsx') %>%
  clean_names()

# Open science
table(d$preregistered)
table(d$reporting_guideline_followed)
table(d$shared_data)
table(d$shared_code)

# Studies with a sample size calculation
table(d$sample_size_calculation)

n_studies <- nrow(d)

d %>%
  group_by(sample_size_calculation) %>%
  count() %>%
  mutate(prop = (n/n_studies)*100)

d_stud <- d %>%
  filter(sample_size_calculation == 'TRUE') %>%
  select(study_id)
paste0(d_stud$study_id) # cite studies with a "sample size calculation" of some description

# Calculations with enough information to be replicated
table(d$sample_size_replicated)

d_rep <-
  d %>%
  filter(sample_size_replicated == 'TRUE') %>%
  select(study_id)
paste0(d_rep$study_id) # cite studies with enough information to replicate the calculation
  

#### End
