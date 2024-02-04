library(tidyverse)
library(alrtools)
# Install alrtools from https://github.com/adamleerich/alrtools
# devtools::install_github('adamleerich/alrtools')


# Variables that might change year-to-year
rds_in  <- 'data/all_responses_clean.Rds'


d <- readr::read_rds(rds_in)
d_info <- alrtools::info(d)


# Add back in a designation field
d$actuarial_credential <- ifelse(
  d$designation_fcas, 'FCAS', ifelse(
    d$designation_acas, 'ACAS', ifelse(
      d$designation_cspa, 'CSPA', 'None') ) ) %>% 
  factor(levels = c('None', 'ACAS', 'FCAS'))


# Number of respondents by year
d %>% 
  group_by(year) %>% 
  summarize(count = n())

# Number of respondents by year by designation
d %>% 
  group_by(year, actuarial_credential) %>% 
  summarize(count = n()) %>% 
  pivot_wider(
    id_cols = year, 
    names_from = actuarial_credential, 
    values_from = count) %>% 
  mutate(Total = None + ACAS + FCAS) %>% 
  mutate(
    prop_None = None / Total,
    prop_ACAS = ACAS / Total,
    prop_FCAS = FCAS / Total)
