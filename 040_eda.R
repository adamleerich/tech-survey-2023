library(tidyverse)
library(tidytext)
library(methods)
library(alrtools)
# Install alrtools from https://github.com/adamleerich/alrtools
# devtools::install_github('adamleerich/alrtools')



# Variables that might change year-to-year
rdata_in <- 'data/wrangled.RData'
rdata_out <- 'data/eda.RData'




## ----------------------------------------------------------------------------------------------------------------------
rdata <- alrtools::load_env(rdata_in)

tbl_responses <- rdata$tbl_responses
tbl_demographic <- rdata$tbl_demographic
tbl_respondent_tool <- rdata$tbl_respondent_tool
tbl_suitability <- rdata$tbl_suitability
tbl_techniques <- rdata$tbl_techniques
tbl_barrier <- rdata$tbl_barrier

tbl_responses$designation <- tbl_responses$actuarial_credential
tbl_demographic$designation <- tbl_demographic$actuarial_credential
tbl_respondent_tool$designation <- tbl_respondent_tool$actuarial_credential


## ----------------------------------------------------------------------------------------------------------------------
tbl_respondents <- tbl_responses %>% 
  group_by(year) %>% 
  summarise(
    n_respondents = n()  )

tbl_respondents





## ----------------------------------------------------------------------------------------------------------------------
bar_fill_colors <- c(
  '#FED35D'
  , '#B4A3BB'
  , "#D0E4F4"
  , '#1269FB'
  , '#142345')


## ----------------------------------------------------------------------------------------------------------------------
augment_shifted_stack <- function(plt_in, fill_labels){
  plt_in + 
    geom_col() +
    theme_minimal() +
    coord_flip() +
    geom_hline(yintercept = 0) + 
    scale_fill_manual(
      values = bar_fill_colors
      , limits = fill_labels
    ) +
    labs(x = 'Tool', y = '# of users') +
    scale_y_continuous(labels = abs)
}


## ----------------------------------------------------------------------------------------------------------------------
str_additional_stops <- c(
  'tools', 
  'software', 
  'reserving',
  'modeling',
  'data',
  'risk',
  'capital',
  'excel',
  'actuarial',
  'specific')

tbl_tools_added <- tbl_responses %>% 
  filter(!is.na(what_other_tools_should_be_added)) %>% 
  filter(length(what_other_tools_should_be_added) > 0) %>% 
  mutate(
    what_other_tools_should_be_added = what_other_tools_should_be_added %>% 
      str_to_lower()
  ) %>% 
  select(respondent_id, what_other_tools_should_be_added) %>% 
  unnest_tokens(word, what_other_tools_should_be_added) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!word %in% str_additional_stops) %>% 
  mutate(
    word = str_to_title(word)
    , word = ifelse(word == 'Vba', 'VBA', word)
  )


## ----------------------------------------------------------------------------------------------------------------------
# compare_usage <- function(tbl_in, frequency_in = 'Daily') {
#   
#   tbl_in %>% 
#     select(-respondent_id) %>% 
#     mutate(
#       freq_val = usage_frequency == frequency_in
#     ) %>% 
#     filter(!is.na(freq_val)) %>% 
#     group_by(tool, freq_val) %>% 
#     summarise(
#       n = n()
#     ) %>% 
#     mutate(
#       pct_of_total = n / sum(n)
#     ) %>% 
#     filter(freq_val) %>% 
#     arrange(desc(pct_of_total)) %>% 
#     select(-freq_val) %>% 
#     mutate(
#       usage = frequency_in
#     )
# }


## ----------------------------------------------------------------------------------------------------------------------
summarise_usage <- function(tbl_in) {
  tbl_in %>% 
    group_by(year, tool, usage_frequency) %>% 
    summarise(
      n_total = n()
    ) %>% 
    ungroup() %>% 
    filter(!is.na(usage_frequency)) %>% 
    group_by(year, tool) %>% 
    mutate(
      pct_tool = n_total / sum(n_total)
    ) %>% 
    group_by(year, usage_frequency) %>% 
    mutate(
      pct_usage_frequency = n_total / sum(n_total)
    ) %>% 
    ungroup()
}




## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary <- tbl_respondent_tool %>% 
  summarise_usage()

tbl_usage_summary



## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary %>% 
  filter(tool == 'R') %>% 
  ggplot(aes(year, pct_tool)) + 
  geom_line(aes(color = usage_frequency)) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary %>% 
  filter(usage_frequency == 'Daily') %>%
  filter(!tool %in% c('Excel', 'MATLAB')) %>% 
  ggplot(aes(year, pct_tool)) + 
  geom_line(aes(color = tool)) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary %>% 
  filter(usage_frequency == 'Daily') %>%
  filter(!tool %in% c('Excel', 'MATLAB', 'SQL')) %>% 
  ggplot(aes(year, pct_tool)) + 
  geom_line(aes(color = tool)) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary %>% 
  filter(usage_frequency == 'Daily') %>%
  filter(tool %in% c('Excel', 'SQL')) %>% 
  ggplot(aes(year, pct_tool)) + 
  geom_line(aes(color = tool)) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
tbl_usage_summary %>% 
  filter(usage_frequency == 'Never') %>%
  filter(!tool %in% c('Excel', 'MATLAB', 'SQL')) %>% 
  ggplot(aes(year, pct_tool)) + 
  geom_line(aes(color = tool)) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------

f_usage <- c("Never", "Less", "Monthly", "Weekly", "Daily") %>% 
  as_factor()

plt_usage_summary <- function(tbl_summary_in){
  tbl_summary_in %>% 
    ggplot(aes(tool, n_total, fill = usage_frequency)) %>% 
    augment_shifted_stack(f_usage) +
    labs(fill = "Frequency")
}


## ----------------------------------------------------------------------------------------------------------------------
f_unused_tools <- tbl_usage_summary %>% 
  filter(usage_frequency == 'Never') %>% 
  arrange(desc(pct_tool)) %>% 
  pull(tool) %>% 
  unique() %>% 
  as.character()

# setdiff(tbl_respondent_tool$tool, f_unused_tools)


plt_basic_usage <- tbl_usage_summary %>% 
  mutate(
    n_total = n_total * ifelse(
      usage_frequency == 'Never'
      , -1
      , 1)
  ) %>%
  mutate(
    tool = tool %>% fct_relevel(f_unused_tools)
  ) %>% 
  plt_usage_summary()

plt_basic_usage 




## ----------------------------------------------------------------------------------------------------------------------
tbl_summary_by_age <- map_dfr(levels(tbl_responses$age), function(x){
  tbl_respondent_tool %>% 
    inner_join(tbl_demographic) %>% 
    filter(age == x) %>% 
    summarise_usage() %>% 
    mutate(
      age = x
    )
}) %>% 
  mutate(
    age = age %>% fct_relevel(levels(tbl_responses$age))
    , usage_frequency = fct_rev(usage_frequency)
    , tool = tool %>% 
          as.factor() %>% 
          fct_relevel(f_unused_tools) %>% 
          fct_rev()
  ) %>% 
  filter(!is.na(age))

plot_usage_by_category <- function(plt_in){
  plt_in + 
  geom_bar(stat = 'identity', position = 'fill') + 
  facet_wrap(~ tool) + 
  theme_minimal() +
  geom_hline(yintercept = 0.5) +
  scale_fill_manual(
      values = bar_fill_colors 
      , limits = f_usage 
    ) +
  labs(x = NULL, y = '% of respondents') +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.5, 1))
}

plt_usage_by_age <- tbl_summary_by_age %>%  
  ggplot(aes(age, n_total, fill = usage_frequency)) %>% 
  plot_usage_by_category()
  
plt_usage_by_age + 
  coord_flip()


## ----------------------------------------------------------------------------------------------------------------------
tbl_summary_by_designation <- map_dfr(levels(tbl_responses$designation), function(x){
  tbl_respondent_tool %>% 
    inner_join(tbl_demographic) %>% 
    filter(designation == x) %>% 
    summarise_usage() %>% 
    mutate(
      designation = x
    )
}) %>% 
  mutate(
    designation = designation %>% fct_relevel(levels(tbl_responses$designation))
    , usage_frequency = fct_rev(usage_frequency)
    , tool = tool %>% 
          as.factor() %>% 
          fct_relevel(f_unused_tools) %>% 
          fct_rev()
  ) 

plt_usage_by_designation <- tbl_summary_by_designation %>%  
  ggplot(aes(designation, n_total, fill = usage_frequency)) %>% 
  plot_usage_by_category()

plt_usage_by_designation +
  coord_flip()


## ----------------------------------------------------------------------------------------------------------------------
tbl_polyglot <- tbl_respondent_tool %>% 
  filter(!is.na(usage_frequency)) %>% 
  group_by(respondent_id) %>% 
  summarise(
    n_tools = sum(usage_frequency != 'Never')
  ) %>% 
  group_by(n_tools) %>% 
  summarise(
    total_users = n()
  ) %>% 
  mutate(
    pct_users = total_users / sum(total_users)
  )


## ----------------------------------------------------------------------------------------------------------------------
plt_polyglot <- tbl_polyglot %>% 
  ggplot(aes(n_tools, pct_users)) +
  geom_bar(stat = 'identity', fill = bar_fill_colors[5]) +
  labs(x = "# of tools", y = "% of respondents") +
  scale_x_continuous(breaks = 1:9, labels = 1:9 %>% as.character()) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

plt_polyglot


## ----------------------------------------------------------------------------------------------------------------------
tbl_polyglot_ge_week <- tbl_respondent_tool %>% 
  filter(usage_frequency %in% c('Daily', 'Weekly')) %>%
  filter(!is.na(usage_frequency)) %>% 
  group_by(respondent_id) %>% 
  summarise(
    n_tools = sum(usage_frequency != 'Never')
  ) %>% 
  group_by(n_tools) %>% 
  summarise(
    total_users = n()
  ) %>% 
  mutate(
    pct_users = total_users / sum(total_users)
  )


## ----------------------------------------------------------------------------------------------------------------------
plt_polyglot_ge_week <- tbl_polyglot_ge_week %>% 
  ggplot(aes(n_tools, pct_users)) +
  geom_bar(stat = 'identity', fill = bar_fill_colors[5]) +
  labs(x = "# of tools", y = "% of respondents") +
  scale_x_continuous(breaks = 1:9, labels = 1:9 %>% as.character()) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

plt_polyglot_ge_week

# What percent of people use at least one tool daily or weekly?
tbl_respondent_tool %>% 
  filter(ge_week) %>% 
  group_by(year, respondent_id) %>% 
  summarize() %>% 
  group_by(year) %>% 
  summarize(at_least_one_ge_week = n()) %>% 
  inner_join(tbl_respondents) %>% 
  mutate(prop = at_least_one_ge_week / n_respondents)





## ----------------------------------------------------------------------------------------------------------------------
excel_users <- tbl_respondent_tool$respondent_id[
  tbl_respondent_tool$tool == 'Excel'
  & tbl_respondent_tool$usage_frequency != 'Never'
  & !is.na(tbl_respondent_tool$usage_frequency)
]

tbl_polyglot_excel <- tbl_respondent_tool %>% 
  filter(
    respondent_id %in% excel_users
  ) %>% 
  filter(!is.na(usage_frequency)) %>% 
  group_by(respondent_id) %>% 
  summarise(
    n_tools = sum(usage_frequency != 'Never')
  ) %>% 
  group_by(n_tools) %>% 
  summarise(
    total_users = n()
  )


## ----------------------------------------------------------------------------------------------------------------------
tbl_other_tools <- tibble(
  other_tools = tbl_responses$what_other_tools_should_be_added %>% 
    tolower()
) %>% 
  filter(!is.na(other_tools)) %>% 
  count(other_tools)


## ----------------------------------------------------------------------------------------------------------------------
summarise_proficiency <- function(tbl_in) {
  
  tbl_in %>% 
    group_by(tool, proficiency) %>% 
    summarise(
      n_total = n()
    ) %>% 
    ungroup() %>% 
    filter(!is.na(proficiency)) %>% 
    group_by(tool) %>% 
    mutate(
      pct_tool = n_total / sum(n_total)
    ) %>% 
    group_by(proficiency) %>% 
    mutate(
      pct_proficiency = n_total / sum(n_total)
    ) %>% 
    ungroup()
}


## ----------------------------------------------------------------------------------------------------------------------


f_how_proficient <- c("None", "Basic", "Intermediate", "Expert", "Advanced") %>% 
  as_factor()

plt_proficiency_summary <- function(tbl_summary_in){

  tbl_summary_in %>% 
    ggplot(aes(tool, n_total, fill = proficiency)) %>% 
    augment_shifted_stack(f_how_proficient) +
    labs(fill = "Proficiency")

}


## ----------------------------------------------------------------------------------------------------------------------
tbl_proficiency_summary <- tbl_respondent_tool %>% 
  filter(tool != 'MATLAB') %>% 
  summarise_proficiency()



## ----------------------------------------------------------------------------------------------------------------------
f_not_proficient_tools <- tbl_proficiency_summary %>% 
  filter(proficiency == 'Never') %>% 
  arrange(desc(pct_tool)) %>% 
  pull(tool) %>% 
  as.character()

plt_proficiency <- tbl_proficiency_summary %>% 
  mutate(
    n_total = n_total * ifelse(
      proficiency == 'Never'
      , -1
      , 1)
  ) %>%
  mutate(
    tool = tool %>% fct_relevel(f_not_proficient_tools)
  ) %>% 
  plt_proficiency_summary()

plt_proficiency


## ----------------------------------------------------------------------------------------------------------------------
tbl_proficiency_v_usage <- tbl_respondent_tool %>% 
  filter(
    !is.na(proficiency)
    , !is.na(usage_frequency)
  ) %>% 
  group_by(tool, usage_frequency, proficiency) %>% 
  summarise(
    n_total = n()
  ) 

# %>% 
#   pivot_wider(
#     values_from = n_total
#     , names_from = proficiency
#   )


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## tbl_proficiency_v_usage %>%
##   filter(tool == 'R') %>%
##   ggplot(aes(aes(tool, n_total, fill = usage_frequency))) +
##   geom_bar(stat = 'identity', position = 'fill') +
##   facet_wrap(~ proficiency)


## ----------------------------------------------------------------------------------------------------------------------
compare_proficiency <- function(tbl_in, proficiency_in = 'Expert') {
  
  tbl_in %>% 
    select(-respondent_id) %>% 
    mutate(
      prof_val = proficiency == proficiency_in
    ) %>% 
    filter(!is.na(prof_val)) %>% 
    group_by(tool, prof_val) %>% 
    summarise(
      n = n()
    ) %>% 
    mutate(
      pct_of_total = n / sum(n)
    ) %>% 
    filter(prof_val) %>% 
    arrange(desc(pct_of_total)) %>% 
    select(-prof_val) %>% 
    mutate(
      proficiency = proficiency_in
    )
}


## ----------------------------------------------------------------------------------------------------------------------
plt_proficiency_summary <- function(tbl_summary_in){

  tbl_summary_in %>% 
    mutate(proficiency = fct_rev(proficiency)) %>% 
    ggplot(aes(tool, n_total, fill = proficiency)) + 
    geom_col() +
    theme_minimal() +
    coord_flip() +
    geom_hline(yintercept = 0) + 
    scale_fill_manual(
      values = c("blue", "red", "yellow", "orange", "pink") %>% rev()
    )
}



## ----------------------------------------------------------------------------------------------------------------------
tbl_respondent_tool %>% 
  compare_proficiency("Basic")

tbl_respondent_tool %>% 
  compare_proficiency("None")


## ----------------------------------------------------------------------------------------------------------------------
tbl_suitability_summary <- tbl_suitability %>% 
  group_by(practice_area, tool, suitability) %>% 
  summarise(n_total = n()) %>% 
  group_by(practice_area, tool) %>% 
  mutate(
    pct_tool_practice = n_total / sum(n_total)
  ) %>% 
  ungroup()

# tbl_suitability_summary %>% 
#   group_by(practice_area, suitability) %>% 
#   summarise(
#     n_total = sum(n_total)
#   ) %>% 
#   group_by(practice_area) %>% 
#   mutate(
#     pct_total = n_total / sum(n_total)
#   )


## ----------------------------------------------------------------------------------------------------------------------

f_suited <- c("Unsure", "Not", "Somewhat", "Very") %>% 
  as_factor()




plot_suitability <- function(tbl_in){
  
  f_very_much_so <- tbl_in %>% 
    filter(suitability == 'Very') %>% 
    group_by(tool) %>% 
    summarise(n_total = sum(n_total)) %>% 
    arrange(n_total) %>% 
    pull(tool) %>% 
    as.character()

  tbl_in %>% 
    filter(!is.na(suitability)) %>% 
    mutate(
      n_total = n_total * ifelse(suitability == 'Unsure', -1, 1)
      , suitability = fct_rev(suitability)
      , tool = fct_relevel(tool, f_very_much_so)
    ) %>% 
    ggplot(aes(tool, n_total, fill = suitability)) %>% 
    augment_shifted_stack(f_suited) +
    labs(fill = 'Suitability') +
    scale_y_continuous(lim = c(-1e3, 1250), labels = abs)
}


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_ratemaking <- tbl_suitability_summary %>% 
  filter(practice_area == 'Ratemaking') %>% 
  plot_suitability() +
  labs(title = 'Ratemaking')


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_reserving <- tbl_suitability_summary %>% 
  filter(practice_area == 'Reserving') %>% 
  plot_suitability() +
  labs(title = 'Reserving')


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_capital <- tbl_suitability_summary %>% 
  filter(practice_area == 'Capital Modeling') %>% 
  plot_suitability() +
  labs(title = 'Capital Modeling')


## ----------------------------------------------------------------------------------------------------------------------
plot_suitability_by_practice_area <- function(tbl_in, by_tool = FALSE){
  
  if (!by_tool) {
    tbl_plot <- tbl_in %>% 
      group_by(practice_area, suitability)
  } else {
    tbl_plot <- tbl_in %>% 
      group_by(practice_area, suitability, tool)
  }
  
  tbl_plot %>% 
    summarise(
      n_total = sum(n_total)
    ) %>% 
    ungroup() %>%
    mutate(
      practice_area = practice_area %>% 
        as_factor() %>% 
        fct_relevel(c('Ratemaking', 'Reserving', 'Capital Modeling')) %>% 
        fct_rev()
      , n_total = n_total * ifelse(suitability == 'Unsure', -1, 1)
      , suitability = suitability %>% fct_rev()
    ) %>%
    ggplot(aes(practice_area, n_total, fill = suitability)) %>% 
    augment_shifted_stack(f_suited) +
    labs(fill = 'Suitability') +
    scale_y_continuous(lim = c(-9e3, 9e3), labels = abs)

}


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_by_practice <- tbl_suitability_summary %>% 
  filter(!is.na(suitability)) %>% 
  plot_suitability_by_practice_area() +
  labs(title = 'Suitability by practice area', subtitle = 'Non-responses removed')

plt_suitability_by_practice


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_by_practice_na <- tbl_suitability_summary %>% 
  mutate(suitability = suitability %>% fct_explicit_na('Unsure')) %>% 
  plot_suitability_by_practice_area() +
  labs(title = 'Suitability by practice area', subtitle = "Non-responses grouped with 'Unsure'") 

plt_suitability_by_practice_na


## ----------------------------------------------------------------------------------------------------------------------
plt_suitability_by_practice_excel <- tbl_suitability_summary %>% 
  filter(tool == 'Excel') %>% 
  mutate(
    suitability = fct_explicit_na(suitability, na_level = 'Unsure')
  ) %>% 
  plot_suitability_by_practice_area() +
  labs(title = 'Suitability by practice area for Excel', subtitle = "Non-responses grouped with 'Unsure'") +
  scale_y_continuous(lim = c(-1200, 1200), labels = abs)

plt_suitability_by_practice_excel


## ----------------------------------------------------------------------------------------------------------------------
f_suitable_tools <- tbl_suitability_summary %>% 
  filter(suitability %in% c('Somewhat', 'Very')) %>% 
  group_by(tool) %>% 
  summarise(n_total = sum(n_total)) %>% 
  arrange(desc(n_total)) %>% 
  pull(tool) 

plt_suitability_by_practice_all <- tbl_suitability_summary %>% 
  mutate(
    suitability = fct_explicit_na(suitability, na_level = 'Unsure')
    , tool = fct_relevel(tool, f_suitable_tools)
  ) %>% 
  plot_suitability_by_practice_area(by_tool = TRUE) +
  facet_wrap(~ tool) +
  labs(title = 'Suitability by practice area for all tools', subtitle = "Non-responses grouped with 'Unsure'") +
  scale_y_continuous(lim = c(-1200, 1200), labels = abs)

plt_suitability_by_practice_all


## ----------------------------------------------------------------------------------------------------------------------
compare_suitability <- function(tbl_in, suitability_in = 'Very') {
  
  tbl_in %>% 
    select(-respondent_id) %>% 
    mutate(
      suit_val = suitability == suitability_in
    ) %>% 
    filter(!is.na(suit_val)) %>% 
    group_by(tool, practice_area, suit_val) %>% 
    summarise(
      n = n()
    ) %>% 
    mutate(
      pct_of_total = n / sum(n)
    ) %>% 
    filter(suit_val) %>% 
    arrange(desc(pct_of_total)) %>% 
    select(-suit_val) %>% 
    mutate(
      suitability = suitability_in
    )
}


## ----------------------------------------------------------------------------------------------------------------------
# tbl_responses %>% 
#   compare_suitability() %>% 
#   filter(practice_area == "ratemaking") 
# 
# tbl_responses %>% 
#   compare_suitability() %>% 
#   filter(practice_area == "reserving") 
# 
# tbl_responses %>% 
#   compare_suitability() %>% 
#   filter(practice_area == "capital modeling") 


## ----------------------------------------------------------------------------------------------------------------------
# tbl_responses %>% 
#   compare_suitability("Never") %>% 
#   group_by(practice_area) %>% 
#   arrange(desc(pct_of_total), .by_group = TRUE) %>% 
#   select(practice_area, tool, n, pct_of_total) %>% 
  # View()


## ----------------------------------------------------------------------------------------------------------------------
# tbl_responses %>% 
#   compare_suitability("Never") %>% 
#   filter(practice_area == "ratemaking") 
# 
# tbl_responses %>% 
#   compare_suitability("Unsure") %>% 
#   filter(practice_area == "capital modeling") 
# 
# tbl_responses %>% 
#   compare_suitability() %>% 
#   filter(tool == "Excel") 
# 
# tbl_responses %>% 
#   compare_suitability() %>% 
#   filter(tool == "R") 


## ----------------------------------------------------------------------------------------------------------------------
suitability_tool <- function(tbl_in, tool_in){
  
  tbl_in %>% 
    melt_suitability() %>% 
    filter(tool == tool_in) %>% 
    filter(!is.na(suitability)) %>% 
    group_by(practice_area, suitability) %>% 
    summarise(
      n = n()
    ) %>% 
    group_by(practice_area) %>% 
    mutate(
      pct_of_total = n / sum(n)
    ) %>% 
    arrange(desc(pct_of_total))
  
}


## ----------------------------------------------------------------------------------------------------------------------
# tbl_responses %>% 
#   suitability_tool("R")  


## ----------------------------------------------------------------------------------------------------------------------
# mojo <- inner_join(
#   tbl_responses %>% melt_proficiency()
#   , tbl_responses %>% melt_suitability()
#   , by = c('respondent_id', 'tool')
# ) %>% 
#   filter(!is.na(suitability))


## ----------------------------------------------------------------------------------------------------------------------
summarise_increase_proficiency <- function(tbl_in, vars){
  
  tbl_in %>% 
    summarise(
      n_increase = sum(increase_proficiency)
      , n_total = n()
    ) %>% 
    mutate(
      pct_increase = n_increase / n_total
    ) %>% 
    ungroup()
}


## ----------------------------------------------------------------------------------------------------------------------
tbl_increase_proficiency_summary <- tbl_respondent_tool %>% 
  group_by(tool) %>% 
  summarise_increase_proficiency() %>% 
  arrange(n_increase) %>% 
  mutate(tool = tool %>% as.character() %>% as_factor())


## ----------------------------------------------------------------------------------------------------------------------
plt_increase_proficiency <- tbl_increase_proficiency_summary %>% 
  ggplot(aes(tool, n_increase)) +
  geom_bar(position = 'stack', stat = 'identity', fill = bar_fill_colors[5]) +
  coord_flip() +
  theme_minimal() 

plt_increase_proficiency


## ----------------------------------------------------------------------------------------------------------------------
plt_increase_proficiency_pct <- tbl_increase_proficiency_summary %>% 
  ggplot(aes(tool, pct_increase)) +
  geom_bar(position = 'stack', stat = 'identity', fill = bar_fill_colors[5]) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  coord_flip() +
  theme_minimal() 

plt_increase_proficiency_pct


## ----------------------------------------------------------------------------------------------------------------------
plot_proficiency <- function(tbl_in){

  tbl_in %>% 
    ggplot(aes(category, pct_increase)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(limits = c(0,1)) +
    theme_minimal() +
    facet_wrap(~tool) +
    geom_hline(yintercept = 0.5)

}


## ----------------------------------------------------------------------------------------------------------------------
tbl_respondent_tool %>% 
  filter(!is.na(designation)) %>% 
  group_by(tool, designation) %>% 
  summarise_increase_proficiency() %>% 
  rename(category = designation) %>% 
  plot_proficiency()


## ----------------------------------------------------------------------------------------------------------------------
tbl_respondent_tool %>% 
  filter(!is.na(what_type_of_company_do_you_work_for)) %>% 
  group_by(tool, what_type_of_company_do_you_work_for) %>% 
  summarise_increase_proficiency() %>% 
  rename(category = what_type_of_company_do_you_work_for) %>% 
  plot_proficiency()


## ----------------------------------------------------------------------------------------------------------------------
tbl_respondent_tool %>% 
  filter(!is.na(actuaries_at_my_organization)) %>% 
  group_by(tool, actuaries_at_my_organization) %>% 
  summarise_increase_proficiency() %>% 
  rename(category = actuaries_at_my_organization) %>% 
  plot_proficiency()


## ----------------------------------------------------------------------------------------------------------------------
tbl_respondent_tool %>% 
  filter(!is.na(years_of_experience)) %>% 
  group_by(tool, years_of_experience) %>% 
  summarise_increase_proficiency() %>% 
  rename(category = years_of_experience) %>% 
  plot_proficiency()


## ----------------------------------------------------------------------------------------------------------------------
increase_proficiency_pct <- function(tbl_in, tool_in = 'Excel'){
  
  tbl_in %>% 
    filter(tool == tool_in) %>% 
    filter(!is.na(usage_frequency), !is.na(proficiency)) %>% 
    mutate(
      increase_proficiency = ifelse(increase_proficiency, 'will', 'wont')
    ) %>% 
    group_by(usage_frequency, proficiency, increase_proficiency) %>% 
    summarise(n_total = n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = increase_proficiency, values_from = n_total, values_fill = 0) %>% 
    arrange(desc(usage_frequency), desc(proficiency)) %>% 
    select(usage_frequency, proficiency, will, wont) %>% 
    mutate(
      n_total = will + wont
      , pct_increase = will / n_total
    )

}


## ----------------------------------------------------------------------------------------------------------------------
plot_increase_proficiency <- function(tbl_in, tool_in) {

  str_title <- paste0('# who want to increase their proficiency in ', tool_in)
  
  tbl_in %>% 
    filter(tool == tool_in) %>% 
    filter(!is.na(usage_frequency), !is.na(proficiency)) %>% 
    filter(increase_proficiency) %>% 
    group_by(usage_frequency, proficiency) %>% 
    summarise(n_total = n()) %>% 
    ggplot(aes(proficiency, usage_frequency, fill = n_total)) +
    geom_tile(color = 'white') +
    theme_minimal() +
    theme(
      legend.position = 'bottom'
    ) +
    scale_fill_gradient(low = bar_fill_colors[1], high = bar_fill_colors[5]) +
    labs(
      x = 'Proficiency'
      , y = 'Usage frequency'
      , title = str_title
      , fill = '# of respondents'
    )
  
}

tbl_respondent_tool %>% 
  plot_increase_proficiency('Excel')

tbl_respondent_tool %>% 
  plot_increase_proficiency('R')

tbl_respondent_tool %>% 
  plot_increase_proficiency('SQL')

tbl_respondent_tool %>% 
  plot_increase_proficiency('Python')


## ----------------------------------------------------------------------------------------------------------------------
tbl_barrier_summary <- tbl_barrier %>% 
  group_by(year, barrier) %>% 
  summarise(
    pct_learning = sum(learning, na.rm = TRUE) / sum(!is.na(learning)),
    pct_implementation = sum(implementation, na.rm = TRUE) / sum(!is.na(implementation)),
    pct_use = sum(use, na.rm = TRUE) / sum(!is.na(use))
  ) %>% 
  ungroup() %>% 
  mutate(
    barrier = str_replace_all(barrier, '_', ' ') %>% 
      str_to_title() %>% 
      str_replace_all('It', 'IT') %>% 
      str_replace_all('Of', 'of')
  )
# %>%   mutate(barrier = fct_reorder(barrier, pct_learning))


## ----------------------------------------------------------------------------------------------------------------------
plt_barriers <- tbl_barrier_summary %>% 
  select(barrier, pct_learning, pct_implementation) %>% 
  pivot_longer(names_to = 'barrier_type', values_to = 'pct_yes', cols = -barrier) %>% 
  mutate(barrier_type = str_remove_all(barrier_type, 'pct_') %>% str_to_title()) %>% 
  ggplot(aes(barrier, pct_yes)) +
  geom_bar(aes(fill = barrier_type), stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = bar_fill_colors[c(1,5)]) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme_minimal() +
  labs(
    x = 'Barrier'
    , y = '% of respondents'
    , title = 'Barriers to learning and implementation of tools as reported by respondents'
    , fill = 'Type of barrier'
  )

plt_barriers


## ----------------------------------------------------------------------------------------------------------------------
tbl_no_techniques <- tbl_techniques %>% 
  group_by(year, respondent_id) %>% 
  summarise(
    n_techniques = sum(use)
  ) %>% 
  filter(n_techniques == 0)

tbl_technique_summary <- tbl_techniques %>% 
  anti_join(tbl_no_techniques, by = c('year', 'respondent_id')) %>% 
  group_by(year, technique) %>% 
  summarise(
    n_use = sum(use),
    n_response = n()) %>% 
  mutate(
    pct_use = n_use / n_response
  )

# # %>%
#   group_by(technique) %>% 
#   mutate(
#     pct_use = pct_use / sum(pct_use)
#   ) %>% 
#   pivot_wider(values_from = pct_use, names_from = use) %>% 
#   rename(use_true = `TRUE`, use_false = `FALSE`) %>% 
#   mutate(
#     technique = factor(technique)
#   ) 
# 
# tbl_technique_summary$technique <- fct_reorder(
#   tbl_technique_summary$technique
#   , tbl_technique_summary$use_true) 
# 
# tbl_technique_summary <- tbl_technique_summary%>% 
#   pivot_longer(cols = -technique, names_to = 'use', values_to = 'n_use') %>% 
#   mutate(
#     use = use %>% str_remove_all('use_') %>% 
#       as.logical()
#   )


## ----------------------------------------------------------------------------------------------------------------------
plt_technique_summary <- tbl_technique_summary %>%
  arrange(n_use) %>% 
  mutate(technique = as_factor(technique)) %>% 
  ggplot(aes(technique, pct_use)) +
  geom_bar(position = 'stack', stat = 'identity', fill = bar_fill_colors[5]) +
  geom_hline(yintercept = c(0.1, 0.25, 0.5), color = bar_fill_colors[1]) +
  ylim(c(0,1)) +
  coord_flip() +
  theme_minimal() +
  labs(
    y = '% Use'
    , x = 'Technique'
  ) +
  scale_y_continuous(labels = scales::percent, breaks = c(0.1, 0.25, 0.5))

plt_technique_summary


## ----------------------------------------------------------------------------------------------------------------------
# tbl_technique_summary_scripter <- tbl_techniques %>% 
#   inner_join(tbl_respondent_scripter, by = 'respondent_id') %>% 
#   group_by(technique, scripter, use) %>% 
#   summarise(pct_use = n()) %>%
#   group_by(technique) %>% 
#   mutate(
#     pct_use = pct_use / sum(pct_use)
#   ) %>% 
#   pivot_wider(values_from = pct_use, names_from = use) %>% 
#   rename(use_true = `TRUE`, use_false = `FALSE`) %>% 
#   mutate(
#     technique = factor(technique)
#   ) 
# 
# tbl_technique_summary_scripter$technique <- fct_reorder(
#   tbl_technique_summary_scripter$technique
#   , tbl_technique_summary_scripter$use_true) 
# 
# tbl_technique_summary_scripter <- tbl_technique_summary_scripter%>% 
#   pivot_longer(cols = c('use_false', 'use_true'), names_to = 'use', values_to = 'n_use') %>% 
#   mutate(
#     use = use %>% str_remove_all('use_') %>% 
#       as.logical()
#   )


## ----------------------------------------------------------------------------------------------------------------------
# tbl_technique_summary_scripter %>% 
#   filter(use) %>% 
#   ggplot(aes(technique, n_use, fill = scripter)) + 
#   geom_bar(position = 'fill', stat = 'identity') + 
#   geom_hline(yintercept = 0.5) +
#   coord_flip() +
#   theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
tbl_comment <- tbl_responses %>% 
  filter(!is.na(additional_comments)) %>% 
  mutate(additional_comments = additional_comments %>% str_to_lower()) %>% 
  filter(!additional_comments %in% c('none', '-', 'na', 'n/a')) %>% 
  select(additional_comments)


## ----------------------------------------------------------------------------------------------------------------------
plt_age <- tbl_demographic %>%
  filter(!is.na(age)) %>% 
  ggplot(aes(age)) +
  geom_bar(fill = bar_fill_colors[5]) +
  theme_minimal() +
  labs(
    x = "Age category"
    , y = "# of respondents"
    , title = "Age of respondents"
    , subtitle = "NAs removed"
  )

plt_age


## ----------------------------------------------------------------------------------------------------------------------
plt_designation <- tbl_demographic %>%
  filter(!is.na(designation)) %>% 
  ggplot(aes(designation)) +
  geom_bar(fill = bar_fill_colors[5]) +
  theme_minimal() +
  labs(
    x = "Designation"
    , y = "# of respondents"
    , title = "Designation of respondents"
    , subtitle = "NAs removed"
  )

plt_designation


## ----------------------------------------------------------------------------------------------------------------------
plt_company_size <- tbl_demographic %>%
  filter(!is.na(actuaries_at_my_organization)) %>% 
  ggplot(aes(actuaries_at_my_organization)) +
  geom_bar(fill = bar_fill_colors[5]) +
  theme_minimal() +
  labs(
    x = NULL
    , y = "# of respondents"
    , title = "# of actuaries at my organization"
    , subtitle = "NAs removed"
  )

plt_company_size


## ----------------------------------------------------------------------------------------------------------------------
plt_years_of_experience <- tbl_demographic %>%
  filter(!is.na(years_of_experience)) %>% 
  ggplot(aes(years_of_experience)) +
  geom_bar(fill = bar_fill_colors[5]) +
  theme_minimal() +
  labs(
    x = NULL
    , y = "# of respondents"
    , title = "Years of experience"
    , subtitle = "NAs removed"
  )

plt_years_of_experience


## ----------------------------------------------------------------------------------------------------------------------
plt_location <- tbl_demographic %>%
  filter(!is.na(where_are_you_located)) %>% 
  group_by(where_are_you_located) %>% 
  summarise(n_total = n()) %>% 
  mutate(
    where_are_you_located = where_are_you_located %>% 
      str_remove_all(" \\(please specify\\)") %>% 
      as.factor() %>% 
      fct_reorder(n_total)
  ) %>% 
  ggplot(aes(where_are_you_located, n_total)) +
  geom_bar(fill = bar_fill_colors[5], stat = 'identity') +
  theme_minimal() +
  labs(
    x = NULL
    , y = "# of respondents"
    , title = "Location of respondent"
    , subtitle = "NAs removed"
  )

plt_location


## ----------------------------------------------------------------------------------------------------------------------
plt_type_of_company <- tbl_demographic %>%
  filter(!is.na(what_type_of_company_do_you_work_for)) %>% 
  group_by(what_type_of_company_do_you_work_for) %>% 
  summarise(n_total = n()) %>% 
  mutate(
    what_type_of_company_do_you_work_for = what_type_of_company_do_you_work_for %>% 
      as.factor() %>% 
      fct_reorder(n_total)
  ) %>% 
  ggplot(aes(what_type_of_company_do_you_work_for, n_total)) +
  geom_bar(fill = bar_fill_colors[5], stat = 'identity') +
  theme_minimal() +
  labs(
    x = NULL
    , y = "# of respondents"
    , title = "Type of company of respondent"
    , subtitle = "NAs removed"
  ) +
  coord_flip()

plt_type_of_company


## ----------------------------------------------------------------------------------------------------------------------
tbl_ai_chi <- tbl_techniques %>% 
  filter(technique == 'AI/deep learning') %>% 
  left_join(tbl_demographic, by = 'respondent_id')

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$years_of_experience)
)

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$designation)
)

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$age)
)

# chisq.test(
#   table(tbl_ai_chi$use, tbl_ai_chi$where_are_you_located)
# )

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$what_type_of_company_do_you_work_for)
)

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$actuaries_at_my_organization)
)

chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$years_of_experience)
)

tbl_scripter <- tbl_respondent_tool %>% 
  group_by(respondent_id) %>% 
  summarise(scripter = sum(scripter) %>% pmin(1))

tbl_ai_chi <- tbl_ai_chi %>% 
  left_join(tbl_scripter, by = 'respondent_id')

chi_sq_scripter <- chisq.test(
  table(tbl_ai_chi$use, tbl_ai_chi$scripter)
)

tbl_ai_chi$use %>% table()

64 / (415 + 64)


## ----Save--------------------------------------------------------------------------------------------------------------
tbls <- ls(pattern = 'tbl_')
plts <- ls(pattern = 'plt_')
fcts <- ls(pattern = 'f_')
numbers <- ls(pattern = 'n_')
funcs <- lsf.str()
save(
  file = rdata_out
  , list = c(tbls, plts, funcs, fcts, numbers, 'bar_fill_colors')
)

