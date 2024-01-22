library(tidyverse)
library(alrtools)

d <- readr::read_rds('data/all_responses.Rds')
hd <- HFactory(d, ignore.case = FALSE)








# Set all empty strings to NA
for (i in 1:ncol(d)) {
  d[[i]][d[[i]] == ""] <- NA
}

d_info <- alrtools::info(d)

# Other columns
hd('_other$', TRUE)

# Columns to ignore
cols_ignore <- c(
  "email_address", "first_name", "last_name", "custom_data_1", 
  "ok_with_being_contacted", "collector_id", "ip_address", "row", 
  "respondent_id", "start_date", "end_date", "designation")

# Free text columns
cols_freetext <- c(
  "suited_Other", "how_proficient_Other", "how_often_use_Other", 
  "type_of_company_other", "suited_Other_specific", "learning_plan_other", 
  "technique_use_other", "how_proficient_Other_specific", "increase_proficiency_other", 
  "how_often_use_Other_specific", "should_have_asked", "barrier_implementation_other", 
  "additional_comments", "new_tech_to_adopt", "barrier_technique_other", 
  "barrier_learning_other", "what_other_tools_should_be_added")


d %>% 
  group_by(how_often_use_SAS, year) %>% 
  summarize(count = n()) %>% 
  pivot_wider(
    id_cols = how_often_use_SAS, 
    names_from = year, 
    values_from = count)
  

f2 <- function(f) {
  d %>% 
    group_by({{f}}, year) %>% 
    summarize(count = n()) %>% 
    pivot_wider(
      id_cols = {{f}}, 
      names_from = year, 
      values_from = count)
}

f2(how_often_use_SAS)

f2(how_often_use_R)
f2(how_often_use_Excel)
f2(how_often_use_GoogleSheets)
f2(how_often_use_SAS)
f2(how_often_use_Python)
f2(how_often_use_Tableau)
f2(how_often_use_MATLAB)
f2(how_often_use_PowerBI)
f2(how_often_use_SQL)

f2(how_proficient_R)
f2(how_proficient_Excel)
f2(how_proficient_GoogleSheets)
f2(how_proficient_SAS)
f2(how_proficient_Python)
f2(how_proficient_Tableau)
f2(how_proficient_MATLAB)
f2(how_proficient_PowerBI)
f2(how_proficient_SQL)
f2(suited_ratemaking_R)
f2(suited_reserving_R)
f2(suited_capital_modeling_R)
f2(suited_ratemaking_Excel)
f2(suited_reserving_Excel)
f2(suited_capital_modeling_Excel)
f2(suited_ratemaking_GoogleSheets)
f2(suited_reserving_GoogleSheets)
f2(suited_capital_modeling_GoogleSheets)
f2(suited_ratemaking_SAS)
f2(suited_reserving_SAS)
f2(suited_capital_modeling_SAS)
f2(suited_ratemaking_Python)
f2(suited_reserving_Python)
f2(suited_capital_modeling_Python)
f2(suited_ratemaking_Tableau)
f2(suited_reserving_Tableau)
f2(suited_capital_modeling_Tableau)
f2(suited_ratemaking_MATLAB)
f2(suited_reserving_MATLAB)
f2(suited_capital_modeling_MATLAB)
f2(suited_ratemaking_PowerBI)
f2(suited_reserving_PowerBI)
f2(suited_capital_modeling_PowerBI)
f2(suited_ratemaking_SQL)
f2(suited_reserving_SQL)
f2(suited_capital_modeling_SQL)
f2(what_other_tools_should_be_added)
f2(increase_proficiency_R)
f2(increase_proficiency_Excel)
f2(increase_proficiency_GoogleSheets)
f2(increase_proficiency_SAS)
f2(increase_proficiency_Python)
f2(increase_proficiency_Tableau)
f2(increase_proficiency_MATLAB)
f2(increase_proficiency_PowerBI)
f2(increase_proficiency_SQL)
f2(increase_proficiency_other)
f2(barrier_learning_inadequate_staff)
f2(barrier_learning_lack_of_management_support)
f2(barrier_learning_lack_of_it_support)
f2(barrier_learning_financial_cost)
f2(barrier_learning_not_enough_time)
f2(barrier_learning_no_perceived_benefit)
f2(barrier_learning_other)
f2(barrier_implementation_inadequate_staff)
f2(barrier_implementation_lack_of_management_support)
f2(barrier_implementation_lack_of_it_support)
f2(barrier_implementation_financial_cost)
f2(barrier_implementation_not_enough_time)
f2(barrier_implementation_no_perceived_benefit)
f2(barrier_implementation_other)
f2(technique_use_linear_models)
f2(technique_use_credibility)
f2(technique_use_premium_adjustment)
f2(technique_use_excess_loss_analysis)
f2(technique_use_bayesian)
f2(technique_use_bornhuetter_ferguson)
f2(technique_use_time_series)
f2(technique_use_aideep_learning)
f2(technique_use_chain_ladder)
f2(technique_use_tree_based_methods)
f2(technique_use_unsupervised_learning)
f2(technique_use_other)
f2(additional_comments)
f2(designation)
f2(age)
f2(where_are_you_located)
f2(located_other)
f2(what_type_of_company_do_you_work_for)
f2(type_of_company_other)
f2(actuaries_at_my_organization)
f2(years_of_experience)
f2(how_often_use_Other)
f2(how_proficient_Other)
f2(suited_Other)
f2(barrier_technique_inadequate_staff)
f2(barrier_technique_lack_of_management_support)
f2(barrier_technique_lack_of_it_support)
f2(barrier_technique_financial_cost)
f2(barrier_technique_not_enough_time)
f2(barrier_technique_no_perceived_benefit)
f2(barrier_technique_other)
f2(new_tech_to_adopt)
f2(should_have_asked)
f2(how_often_use_Other_specific)
f2(how_proficient_Other_specific)
f2(suited_ratemaking_Other)
f2(suited_reserving_Other)
f2(suited_capital_modeling_Other)
f2(suited_Other_specific)
f2(learning_plan_self_study)
f2(learning_plan_online)
f2(learning_plan_in_person)
f2(learning_plan_other)
f2(technique_use_frequency_severity)
f2(technique_use_triangle_based)
f2(technique_use_simulation)
f2(barrier_technique_lack_of_knowledge)
f2(designation_none)
f2(designation_acas)
f2(designation_fcas)
f2(designation_cspa)
f2(ok_with_being_contacted)




## Designation
d$designation_none[d$designation == 'None yet'] <- 'None yet'
d$designation_acas[d$designation == 'ACAS'] <- 'ACAS'
d$designation_fcas[d$designation == 'FCAS'] <- 'FCAS'
d$designation_cspa[d$designation == 'CSPA'] <- 'CSPA'


f1 <- function(f) {
  d %>% 
    filter(!is.na({{f}})) %>% 
    group_by({{f}}, year) %>% 
    summarize(count = n()) %>% 
    mutate(len = nchar({{f}})) %>% 
    group_by(year) %>% 
    summarize(
      longest_len = max(len),
      most_answered = max(count),
      unique_answers = n() )
}

hd('specific', TRUE)
f1(suited_Other)
f1(how_proficient_Other) # look at 2023
f1(how_often_use_Other) # look at 2023
f1(type_of_company_other)
f1(suited_Other_specific)
f1(learning_plan_other)
f1(technique_use_other)
f1(how_proficient_Other_specific)
f1(increase_proficiency_other)
f1(how_often_use_Other_specific)
f1(should_have_asked)
f1(barrier_implementation_other)
f1(additional_comments)
f1(new_tech_to_adopt)
f1(barrier_technique_other)
f1(barrier_learning_other)
f1(what_other_tools_should_be_added)


table0(d$how_often_use_Other[d$year == 2023])
table0(d$how_proficient_Other[d$year == 2023])
table0(d$suited_Other[d$year == 2023])

table0(d$how_often_use_Other_specific[d$year == 2023])
table0(d$how_proficient_Other_specific[d$year == 2023])
table0(d$suited_Other_specific[d$year == 2023])




d %>% 
  group_by(suited_Other) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

d %>% 
  group_by(suited_Other_specific) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

d_info %>% 
  filter(unique_values == 1) %>% 
  select(type, column, levels, na_count) %>% 
  clipr::write_clip()



