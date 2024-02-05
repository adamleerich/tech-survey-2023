library(tidyverse)
library(alrtools)
# Install alrtools from https://github.com/adamleerich/alrtools
# devtools::install_github('adamleerich/alrtools')



# Variables that might change year-to-year
rds_in <- 'data/all_responses_clean.Rds'
rdata_out <- 'data/wrangled.RData'




## -----------------------------------------------------------------------------
# Load data and create helper objects
tbl_responses <- readr::read_rds(rds_in)
hd <- HFactory(tbl_responses, ignore.case = TRUE)
tbl_responses_info <- alrtools::info(tbl_responses)



## -----------------------------------------------------------------------------
# Check that respondent ID is unique
stopifnot(
  sum(duplicated(tbl_responses$respondent_id)) == 0)



## -----------------------------------------------------------------------------
tbl_demographic <- tbl_responses %>%
  select(
    respondent_id, actuarial_credential, age, where_are_you_located,
    what_type_of_company_do_you_work_for, actuaries_at_my_organization,
    years_of_experience, year)





## -----------------------------------------------------------------------------
str_fix_tools <- function(str_in) {
  str_in %>%
    str_replace("PowerBI", "Power BI") %>%
    str_replace("GoogleSheets", "Google Sheets")
}


tbl_usage <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    starts_with("how_often"),
    -how_often_use_Other_specific) %>%
  pivot_longer(
    cols = starts_with("how_often"), 
    names_to = 'tool', 
    values_to = 'usage_frequency') %>%
  mutate(
    tool = str_remove(tool, 'how_often_use_') %>% str_fix_tools(), 
    uses = usage_frequency != 'Never', 
    ge_week = usage_frequency %in% c('Daily', 'Weekly'), 
    scripter = (
      tool %in% c('R', 'Python', 'MATLAB', 'SAS')) & (ge_week) )






tbl_proficiency <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    starts_with("how_proficient"),
    -how_proficient_Other_specific) %>%
  pivot_longer(
    cols = starts_with("how_proficient"), 
    names_to = 'tool', 
    values_to = 'proficiency') %>%
  mutate(
    tool = str_remove(tool, 'how_proficient_') %>% str_fix_tools()  )





tbl_increase_proficiency <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    starts_with('increase_proficiency')) %>%
  select(-increase_proficiency_other) %>%
  pivot_longer(
    cols = starts_with('increase_proficiency'), 
    names_to = 'tool', 
    values_to = 'increase_proficiency') %>%
  mutate(
    tool = tool %>%
      str_remove('increase_proficiency_') %>%
      str_fix_tools() ) %>%
  inner_join(
    tbl_demographic, 
    by = c('respondent_id', 'year') )







tbl_respondent_tool <- tbl_usage %>%
  inner_join(
    tbl_proficiency, by = c('respondent_id', 'year', 'tool')
  ) %>%
  inner_join(
    tbl_increase_proficiency, by = c('respondent_id', 'year', 'tool')
  ) %>%
  mutate(
    tool = tool %>% as_factor() )



## -----------------------------------------------------------------------------
# Grain and count checks
nrow(tbl_responses)
nrow(tbl_demographic)
nrow(tbl_respondent_tool)
nrow(tbl_increase_proficiency)
nrow(tbl_proficiency)

tbl_respondent_tool %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_respondent_tool %>% 
  filter(!is.na(usage_frequency)) %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_increase_proficiency %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_increase_proficiency %>% 
  filter(increase_proficiency) %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_proficiency %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_proficiency %>% 
  filter(!is.na(proficiency)) %>% 
  group_by(year, tool) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = tool, names_from = year, values_from = count) %>% 
  janitor::adorn_totals('col')

tbl_responses$respondent_id %>% duplicated %>% sum

setdiff(tbl_responses$respondent_id, tbl_demographic$respondent_id)
setdiff(tbl_responses$respondent_id, tbl_respondent_tool$respondent_id)
setdiff(tbl_responses$respondent_id, tbl_proficiency$respondent_id)
setdiff(tbl_responses$respondent_id, tbl_increase_proficiency$respondent_id)

setdiff(tbl_demographic$respondent_id, tbl_responses$respondent_id)
setdiff(tbl_respondent_tool$respondent_id, tbl_responses$respondent_id)
setdiff(tbl_proficiency$respondent_id, tbl_responses$respondent_id)
setdiff(tbl_increase_proficiency$respondent_id, tbl_responses$respondent_id)





## -----------------------------------------------------------------------------
tbl_suitability <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    starts_with("suited"),
    -suited_Other_specific) %>%
  pivot_longer(
    cols = starts_with("suited"), 
    names_to = 'tool_area', 
    values_to = 'suitability') %>%
  mutate(
    tool_area = str_remove(tool_area, 'suited_') %>%
      str_replace_all("capital_modeling", "capital modeling") ) %>%
  separate(
    tool_area, 
    into = c("practice_area", "tool"), 
    sep = "_") %>%
  mutate(
    practice_area = str_to_title(practice_area) ) %>%
  inner_join(
    tbl_respondent_tool, 
    by = c('respondent_id', 'year', 'tool')  )

tbl_suitability %>% 
  group_by(year, practice_area, tool) %>% 
  summarize(count = n()) %>% 
  mutate(year_area = paste0(practice_area, year)) %>% 
  select(-year, -practice_area) %>% 
  pivot_wider(id_cols = year_area, names_from = tool, values_from = count)

tbl_suitability %>% 
  filter(!is.na(suitability)) %>% 
  group_by(year, practice_area, tool) %>% 
  summarize(count = n()) %>% 
  mutate(year_area = paste0(practice_area, year)) %>% 
  select(-year, -practice_area) %>% 
  pivot_wider(id_cols = year_area, names_from = tool, values_from = count)





## -----------------------------------------------------------------------------
tbl_techniques <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    starts_with("technique")) %>%
  select(-technique_use_other) %>%
  pivot_longer(
    cols = starts_with('technique'), 
    names_to = 'technique', 
    values_to = 'use') %>% 
  mutate(
    technique = technique %>% 
      str_remove('technique_use_') %>% 
      str_replace_all("_", " ") %>%
      str_to_title() %>%
      str_replace('Aideep Learning', "AI/deep Learning") %>%
      str_replace("Ladder", "ladder") %>%
      str_replace("Models", "models") %>%
      str_replace(" Ferg", "-Ferg") %>%
      str_replace("Series", "series") %>%
      str_replace("Tree Based Methods", "Tree-based methods") %>%
      str_replace("Learning", "learning") %>%
      str_replace("Excess Loss Analysis", "Excess loss analysis") )
      
tbl_techniques %>% 
  group_by(technique, year) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = technique, names_from = year, values_from = count)

tbl_techniques %>% 
  filter(use) %>% 
  group_by(technique, year) %>% 
  summarize(count = n()) %>% 
  pivot_wider(id_cols = technique, names_from = year, values_from = count)

  




## -----------------------------------------------------------------------------
tbl_barrier <- tbl_responses %>%
  select(
    respondent_id, 
    year, 
    contains("barrier")) %>%
  select(
    -barrier_learning_other, 
    -barrier_implementation_other,
    -barrier_technique_other) %>%
  pivot_longer(
    cols = contains('barrier'), 
    names_to = 'barrier', 
    values_to = 'barrier_exists') %>%
  mutate(
    barrier_type = case_when(
      str_detect(barrier, 'learning') ~ 'learning',
      str_detect(barrier, 'implementation') ~ 'implementation',
      str_detect(barrier, 'technique') ~ 'use',
      TRUE ~ NA) ) %>%
  mutate(
    barrier = barrier %>%
      str_remove('barrier_') %>%
      str_remove('learning_') %>%
      str_remove('implementation_') %>% 
      str_remove('technique_') ) %>%
  pivot_wider(
    names_from = 'barrier_type', 
    values_from = 'barrier_exists' ) %>%
  inner_join(
    tbl_demographic, 
    by = c('respondent_id', 'year') )


tbl_barrier %>% 
  group_by(year, barrier) %>% 
  summarise(count = n()) %>% 
  pivot_wider(id_cols = barrier, names_from = year, values_from = count)

tbl_barrier %>% 
  filter(learning) %>% 
  group_by(year, barrier) %>% 
  summarise(count = n()) %>% 
  pivot_wider(id_cols = barrier, names_from = year, values_from = count)

tbl_barrier %>% 
  filter(implementation) %>% 
  group_by(year, barrier) %>% 
  summarise(count = n()) %>% 
  pivot_wider(id_cols = barrier, names_from = year, values_from = count)

tbl_barrier %>% 
  filter(use) %>% 
  group_by(year, barrier) %>% 
  summarise(count = n()) %>% 
  pivot_wider(id_cols = barrier, names_from = year, values_from = count)








## -----------------------------------------------------------------------------
save(
  tbl_barrier,
  tbl_demographic,
  tbl_respondent_tool,
  tbl_responses,
  tbl_suitability,
  tbl_techniques,
  file = rdata_out)






## -----------------------------------------------------------------------------
tbl_barrier %>%
  write_csv(file = 'data/barriers.csv')

tbl_demographic %>%
  write_csv(file = 'data/demographic.csv')

tbl_respondent_tool %>%
  write_csv(file = 'data/respondent_tool.csv')

tbl_suitability %>%
  write_csv(file = 'data/suitability.csv')

tbl_techniques %>%
  write_csv(file = 'data/techniques.csv')

