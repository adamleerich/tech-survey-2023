library(tidyverse)
library(alrtools)
# Install alrtools from https://github.com/adamleerich/alrtools
# devtools::install_github('adamleerich/alrtools')



# Variables that might change year-to-year
rds_in  <- 'data/all_responses.Rds'
rds_out <- 'data/all_responses_clean.Rds'
csv_out <- 'data/all_responses_clean.csv'



d <- readr::read_rds(rds_in)

# Set all empty strings to NA
for (i in 1:ncol(d)) {
  d[[i]][d[[i]] == ""] <- NA
}

# Helper data/functions
hd <- HFactory(d, ignore.case = FALSE)
d_info <- alrtools::info(d)
unique_by_year <- function(f) {
  f <- f[1]
  d %>% 
    group_by(.data[[f]], year) %>% 
    summarize(count = n(), .groups = 'drop') %>% 
    pivot_wider(
      id_cols = all_of(f), 
      names_from = year, 
      values_from = count) }


# Are there any numeric columns? 
#   year, row, plus a bunch that are not useful
table0(d_info$type)
d_info %>% filter(type != 'character')


# Any blanks in this data set?
sum(d == '', na.rm = TRUE)
sum(is.na(d))
nrow(d) * ncol(d)


# Fix designation column from 2021 and 2022
#   Starting in 2023, each designation has it's own column
d$designation_none[d$designation == 'None yet'] <- 'None yet'
d$designation_acas[d$designation == 'ACAS'] <- 'ACAS'
d$designation_fcas[d$designation == 'FCAS'] <- 'FCAS'
d$designation_cspa[d$designation == 'CSPA'] <- 'CSPA'



# Fix suited other columns
#
#   Combine 2022 suited_Other and 2023 suited_Other_specific
#   There are free text fields
#
#   Have expected suited level set:
#     2023 suited_ratemaking_Other
#     2023 suited_reserving_Other
#     2023 suited_capital_modeling_Other
#
#   Everything else should be NA!
#

# d_suited__long <- d %>% 
#   mutate(ID = 1:nrow(d)) %>% 
#   select(
#     year,
#     ID,
#     suited_Other,
#     suited_ratemaking_Other,
#     suited_reserving_Other,
#     suited_capital_modeling_Other,
#     suited_Other_specific) %>% 
#   pivot_longer(
#     cols = -c('year', 'ID')) %>% 
#   mutate(
#     new_col = paste0(name, '__', year)) %>% 
#   select(-name)
# 
# d_suited__wide <- d_suited__long %>% 
#   pivot_wider(
#     id_cols = c('ID', 'year'), 
#     names_from = new_col,
#     values_from = value)
# 
# d_suited__wide__info <- info(d_suited__wide)
# 
# d_suited__wide__info %>% 
#   filter(unique_values > 0) %>% 
#   select(column, levels)
# 
# d_suited__wide__info %>% 
#   filter(unique_values == 0) %>% 
#   select(column)
# 
# # Check that expected column/year combos are all NA!
# # This should have ZERO rows
# should_be_NA <- d_suited__long %>% 
#   filter(!is.na(value), new_col %in% c(
#     'suited_Other__2021',
#     'suited_ratemaking_Other__2021',
#     'suited_reserving_Other__2021',
#     'suited_capital_modeling_Other__2021',
#     'suited_Other_specific__2021',
#     'suited_ratemaking_Other__2022',
#     'suited_reserving_Other__2022',
#     'suited_capital_modeling_Other__2022',
#     'suited_Other_specific__2022',
#     'suited_Other__2023'))
#            
# stopifnot(nrow(should_be_NA) == 0)

d$suited_Other_specific <- ifelse(
  d$year == 2022, 
  d$suited_Other,
  d$suited_Other_specific)

d$suited_Other <- NA




# Fix "other specific" columns
# In 2021-2022, "other" columns were free-text
# In 2023, the questions were renamed to be "Other_specific"
# The old questions were retained as radio options for freq, usefulness, etc.
# We put the free-form answers from 2021-2022 into the "Other_specific" cols

# d_specific__long <- d %>%
#   mutate(ID = 1:nrow(d)) %>%
#   select(
#     year,
#     ID,
#     how_often_use_Other,
#     how_often_use_Other_specific,
#     how_proficient_Other,
#     how_proficient_Other_specific) %>%
#   pivot_longer(
#     cols = -c('year', 'ID')) %>%
#   mutate(
#     new_col = paste0(name, '__', year)) %>%
#   select(-name)
# 
# d_specific__wide <- d_specific__long %>%
#   pivot_wider(
#     id_cols = c('ID', 'year'),
#     names_from = new_col,
#     values_from = value)
# 
# d_specific__wide__info <- info(d_specific__wide)
# 
# d_specific__wide__info %>%
#   filter(unique_values > 0) %>%
#   select(column, levels)
# 
# d_specific__wide__info %>%
#   filter(unique_values == 0) %>%
#   select(column)
# 
# # Check that expected column/year combos are all NA!
# # This should have ZERO rows
# should_be_NA <- d_specific__long %>%
#   filter(!is.na(value), new_col %in% c(
#     'how_often_use_Other__2021',
#     'how_often_use_Other_specific__2021',
#     'how_proficient_Other__2021',
#     'how_proficient_Other_specific__2021',
#     'how_often_use_Other_specific__2022',
#     'how_proficient_Other_specific__2022'))
# 
# stopifnot(nrow(should_be_NA) == 0)

d$how_proficient_Other_specific <- ifelse(
  d$year <= 2022, 
  d$how_proficient_Other,
  d$how_proficient_Other_specific)

d$how_often_use_Other_specific <- ifelse(
  d$year <= 2022, 
  d$how_often_use_Other,
  d$how_often_use_Other_specific)

d$how_proficient_Other[d$year <= 2022] <- NA
d$how_often_use_Other[d$year <= 2022] <- NA







## Classify each column and make sure we've covered all of them

qs_ID <- c('year', 'row', 'respondent_id')

qs_ignore <- c(
  # No analysis value
  'email_address', 'first_name', 'last_name', 'custom_data_1', 
  'ok_with_being_contacted', 'collector_id', 'ip_address',  
  'start_date', 'end_date', 
  # Exploded in 2023
  'designation',
  # All NAs
  'suited_Other')

## Columns with the following value set:
##   About once a month      (2021 only)
##   About once a week       (2021 only)
##   At least once a day
##   At least once a month   (2022+)
##   At least once a week    (2022+)
##   Less than once a month
##   Not at all
qs_how_often <- c(
  'how_often_use_R', 'how_often_use_Excel', 'how_often_use_GoogleSheets', 
  'how_often_use_SAS', 'how_often_use_Python', 'how_often_use_Tableau', 
  'how_often_use_PowerBI', 'how_often_use_SQL', 'how_often_use_MATLAB',
  'how_often_use_Other')

## Columns with the following value set:
##   Advanced
##   Basic stuff
##   Expert
##   Intermediate
##   Not at all
qs_proficient <- c(
  'how_proficient_R', 'how_proficient_Excel', 'how_proficient_GoogleSheets', 
  'how_proficient_SAS', 'how_proficient_Python', 'how_proficient_Tableau', 
  'how_proficient_PowerBI', 'how_proficient_SQL', 'how_proficient_MATLAB',
  'how_proficient_Other')


## Columns with the following value set:
##   Not at all
##   Somewhat
##   Unsure
##   Very much    (2023 only)
##   Very much so (2021-2022)
qs_suited <- c(
  'suited_ratemaking_R', 'suited_reserving_R', 'suited_capital_modeling_R', 
  'suited_ratemaking_Excel', 'suited_reserving_Excel', 
  'suited_capital_modeling_Excel', 'suited_ratemaking_GoogleSheets', 
  'suited_reserving_GoogleSheets', 'suited_capital_modeling_GoogleSheets', 
  'suited_ratemaking_SAS', 'suited_reserving_SAS', 
  'suited_capital_modeling_SAS', 'suited_ratemaking_Python', 
  'suited_reserving_Python', 'suited_capital_modeling_Python', 
  'suited_ratemaking_Tableau', 'suited_reserving_Tableau', 
  'suited_capital_modeling_Tableau', 'suited_ratemaking_PowerBI', 
  'suited_reserving_PowerBI', 'suited_capital_modeling_PowerBI', 
  'suited_ratemaking_SQL', 'suited_reserving_SQL', 
  'suited_capital_modeling_SQL', 'suited_ratemaking_Other', 
  'suited_reserving_Other', 'suited_capital_modeling_Other', 
  'suited_ratemaking_MATLAB', 'suited_reserving_MATLAB', 
  'suited_capital_modeling_MATLAB')

## Single values: will convert to logical
qs_logical <- c(
  'increase_proficiency_R', 'increase_proficiency_Excel', 
  'increase_proficiency_GoogleSheets', 'increase_proficiency_SAS', 
  'increase_proficiency_Python', 'increase_proficiency_Tableau', 
  'increase_proficiency_PowerBI', 'increase_proficiency_SQL', 
  'barrier_learning_inadequate_staff', 
  'barrier_learning_lack_of_management_support', 
  'barrier_learning_lack_of_it_support', 'barrier_learning_financial_cost', 
  'barrier_learning_not_enough_time', 'barrier_learning_no_perceived_benefit', 
  'barrier_implementation_inadequate_staff', 
  'barrier_implementation_lack_of_management_support', 
  'barrier_implementation_lack_of_it_support', 
  'barrier_implementation_financial_cost', 
  'barrier_implementation_not_enough_time', 
  'barrier_implementation_no_perceived_benefit', 
  'technique_use_linear_models', 'technique_use_credibility', 
  'technique_use_premium_adjustment', 'technique_use_excess_loss_analysis', 
  'technique_use_bayesian', 'technique_use_time_series', 
  'technique_use_aideep_learning', 'technique_use_tree_based_methods', 
  'technique_use_unsupervised_learning', 'technique_use_frequency_severity', 
  'technique_use_triangle_based', 'technique_use_simulation', 
  'designation_none', 'designation_acas', 'designation_fcas', 
  'designation_cspa', 'barrier_technique_inadequate_staff', 
  'barrier_technique_lack_of_management_support', 
  'barrier_technique_lack_of_it_support', 'barrier_technique_financial_cost', 
  'barrier_technique_not_enough_time', 'barrier_technique_no_perceived_benefit', 
  'barrier_technique_lack_of_knowledge', 'learning_plan_self_study', 
  'learning_plan_online', 'learning_plan_in_person', 'increase_proficiency_MATLAB',
  'technique_use_bornhuetter_ferguson', 'technique_use_chain_ladder')


## Unique domains
qs_demo <- c(
  'age', 'where_are_you_located', 'what_type_of_company_do_you_work_for', 
  'years_of_experience', 'actuaries_at_my_organization')


## Free-text fields
qs_freetext <- c(
  'what_other_tools_should_be_added', 'increase_proficiency_other', 
  'barrier_learning_other', 'barrier_implementation_other', 
  'technique_use_other', 'additional_comments', 'located_other', 
  'type_of_company_other', 'how_often_use_Other', 'how_proficient_Other', 
  'barrier_technique_other', 'new_tech_to_adopt', 'should_have_asked', 
  'how_often_use_Other_specific', 'how_proficient_Other_specific', 
  'suited_Other_specific', 'learning_plan_other')


## Check that all columns have been classified
qs_expected <- c(
  qs_ID,
  qs_ignore, 
  qs_how_often,
  qs_proficient,
  qs_suited,
  qs_logical,
  qs_demo,
  qs_freetext)

stopifnot(
  length(setdiff(names(d), qs_expected)) + 
    length(setdiff(qs_expected, names(d))) == 0
)




## CLEAN UP
map_to_factor <- function(v, map_from, map_to) {
  qdiff <- setdiff(v, c(map_from, NA))
  stopifnot(length(qdiff) == 0)
  L <- !is.na(map_to)
  map_from <- map_from[L]
  map_to <- map_to[L]
  factor(v, levels = map_from, labels = map_to)
}


## Step 1: remove ignore columns
d <- d %>% select(-all_of(qs_ignore))

## Step 2: convert single-values to logicals
for (i in 1:length(qs_logical)) {
  q <- qs_logical[i]
  stopifnot(length(table(d[[q]])) == 1)
  d[[q]] <- ifelse(is.na(d[[q]]), FALSE, TRUE)
}

## Step 3: how often columns
map_from <- c(
  "At least once a day",
  "About once a week",
  "At least once a week",
  "About once a month",
  "At least once a month",
  "Less than once a month",
  "Not at all")

map_to <- c(
  'Daily', 'Weekly', 'Weekly', 'Monthly', 'Monthly', 
  'Less', 'Never')

for (i in 1:length(qs_how_often)) {
  q <- qs_how_often[i]
  d[[q]] <- map_to_factor(d[[q]], map_from, map_to)
}


## Step 4: proficient columns
map_from <- c(
  'Advanced',
  'Expert',
  'Intermediate',
  'Basic stuff',
  'Not at all')

map_to <- c(
  'Advanced',
  'Expert',
  'Intermediate',
  'Basic',
  'None')

for (i in 1:length(qs_proficient)) {
  q <- qs_proficient[i]
  d[[q]] <- map_to_factor(d[[q]], map_from, map_to)
}


## Step 5: suited columns
map_from <- c(
  'Very much',
  'Very much so',
  'Somewhat',
  'Not at all',
  'Unsure')

map_to <- c(
  'Very',
  'Very',
  'Somewhat',
  'Not', NA)

for (i in 1:length(qs_suited)) {
  q <- qs_suited[i]
  d[[q]] <- map_to_factor(d[[q]], map_from, map_to)
}


## Step 6: age
map_from <- c(
  'Up to 30 years old',
  '31-40 years old',
  '41-50 years old',
  '51-60 years old',
  '61 or older',
  'Prefer not to say')

map_to <- c(
  '(0, 30]',
  '(30, 40]',
  '(40, 50]',
  '(50, 60]',
  '(60, Inf)', NA)

d$age <- map_to_factor(d$age, map_from, map_to)


## Step 7: where_are_you_located
map_from <- c(
  'United States',
  'Canada',
  'Africa',
  'Asia',
  'Australia',
  'Bermuda',
  'Central or South America',
  'Europe',
  'Other (please specify)',
  'United Kingdom, Ireland')

map_to <- c(
  'United States',
  'Canada',
  'Other',
  'Asia',
  'Other',
  'Other',
  'Other',
  'Other',
  'Other',
  'Other')

d$where_are_you_located <- map_to_factor(
  d$where_are_you_located, map_from, map_to)



## Step 8: what_type_of_company_do_you_work_for
map_from <- c(
  'Insurance company', 
  'Reinsurance company', 
  'Broker or (re)insurance intermediary', 
  'Consulting Firm', 
  'Government or regulatory organization', 
  'Academic institution', 
  'Non-governmental insurance service organization', 
  'Other (please specify)', 
  'Other financial institution', 
  'Retired')

map_to <- c(
  'Insurance', 
  'Reinsurance', 
  'Broker', 
  'Consulting', 
  'Other', 
  'Other', 
  'Other', 
  'Other', 
  'Other', 
  NA)

d$what_type_of_company_do_you_work_for <- map_to_factor(
  d$what_type_of_company_do_you_work_for, map_from, map_to)



## Step 9: years_of_experience
map_from <- c(
  'Less than 5',
  'Between 5 and 10',
  'Between 10 and 15',
  'Between 15 and 20',
  'More than 20')

map_to <- c(
  '(0, 5)',
  '[5, 10)',
  '[10, 15)',
  '[15, 20)',
  '[20, Inf)')

d$years_of_experience <- map_to_factor(
  d$years_of_experience, map_from, map_to)



## Step 10: actuaries_at_my_organization
map_from <- c(
  '1-5',
  '5-Jan',
  '6-10',
  '10-Jun',
  '11-25',
  '25-Nov',
  '26-50',
  '51-200',
  '200+')

map_to <- c(
  '(0, 5]',
  '(0, 5]',
  '(5, 10]',
  '(5, 10]',
  '(10, 25]',
  '(10, 25]',
  '(25, 50]',
  '(50, 200]',
  '(200, Inf)')

d$actuaries_at_my_organization <- map_to_factor(
  d$actuaries_at_my_organization, map_from, map_to)


# info(d) %>% clipr::write_clip()
# 
# ## Check that all levels EXCLUDE NA
# for (i in 1:length(d)) {
#   if (is.factor(d[[i]])) {
#     print(levels(d[[i]]))
#   }
# }


# ## How many answers do we have for each question?
# ##   Can use to figure out new questions and removed questions too
# answer_count_by_year_and_question <- d %>% 
#   select(-row) %>% 
#   mutate_all(as.character) %>% 
#   pivot_longer(cols = !c('year'), values_drop_na = TRUE) %>% 
#   group_by(year, name) %>% 
#   summarize(count = n()) %>% 
#   pivot_wider(
#     id_cols = 'name', 
#     names_from = 'year', 
#     values_from = 'count',
#     values_fill = 0)
# 
# answer_count_by_year_and_question %>% filter(`2021` == 0)
# answer_count_by_year_and_question %>% filter(`2022` == 0)
# answer_count_by_year_and_question %>% filter(`2023` == 0)
# 

readr::write_rds(d, file = rds_out)
readr::write_csv(d, file = csv_out)

