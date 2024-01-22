library(tidyverse)
library(alrtools)

# Variables that might change year-to-year
years <- c(2021:2023)
map <- read.csv('raw_data/header-map-4.csv')
raw_data_stem <- 'raw_data/technology_survey_responses_' # &year
rds_out <- 'data/all_responses.Rds'
csv_out <- 'data/all_responses.csv'
questions_years_out <- 'data/questions_years.csv'




#' Reads the first 2 rows of the survey responses
#' and creates a header table to resolve the questions
#' into a `new_name` 
read_header <- function(file) {
  header <- read.csv(
    file = file, 
    nrow = 2, 
    header = FALSE) %>% 
    as.matrix %>% t %>% 
    `colnames<-`(c('h1', 'h2')) %>% 
    as_tibble() %>% 
    mutate(
      h1_noblanks = h1, 
      h3 = NA)
  
  if (coalesce(header$h1[1], '') == '') {
    stop('Expect row 1 of 2-row header to not be blank in the first column')
  }
  
  
  for (i in 2:nrow(header)) {
    if (coalesce(header$h1[i], '') == '') {
      header$h1_noblanks[i] <- header$h1_noblanks[i-1]
    } else {
      header$h1_noblanks[i] <- header$h1[i]
    }
  }
  
  header$h3 <- ifelse(
    coalesce(header$h2, '') == '',
    header$h1_noblanks,
    paste0(header$h1_noblanks, '|', header$h2))
  
  return(header)
}





# Process each year and create one header mapping object
headers <- NULL
for (y in years) {
  file <- paste0(raw_data_stem, y,'.csv')
  dy <- read_header(file)
  dy$year <- y
  dy$col <- 1:nrow(dy)
  if (is.null(headers)) {
    headers <- dy
  } else {
    headers <- rbind(headers, dy)
  }
}

headers$old_name <- headers$h3 %>% 
  alrtools::normalize_quote_characters()

headers2 <- headers %>% 
  left_join(map, by = 'old_name') %>% 
  select(year, col, old_name, new_name) %>% 
  arrange(year, col)




# Do some checking to make sure the map is good

if ((headers2 %>% filter(coalesce(new_name, '') == '') 
     %>% nrow) > 0) {
  headers %>% anti_join(map) %>% 
    select(old_name, year) %>% 
    group_by(year, by = 'old_name') %>% 
    summarize(count = n())
  stop('There are missing mappings!')
}

if ((headers2 %>% 
     group_by(year, new_name) %>% 
     summarize(count = n(), .groups = 'keep') %>% 
     filter(count > 1) %>% nrow) > 0) {
  stop('There are duplicate mappings!')
}



# Build one big table and save as an RDS file
all_data <- NULL
for (y in years) {
  file <- paste0(raw_data_stem, y,'.csv')
  cn <- headers2$new_name[headers2$year == y]
  dy <- read.csv(file, skip = 2, header = FALSE, col.names = cn)
  dy$year <- y
  dy$row <- 1:nrow(dy)
  if (is.null(all_data)) {
    all_data <- dy %>% select(year, row, everything()) %>% as_tibble
  } else {
    all_data <- dplyr::bind_rows(all_data, dy)
  }
}

readr::write_rds(all_data, file = rds_out)
readr::write_csv(all_data, file = csv_out)




# Also save a table showing what questions are applicable in each year

headers2 %>% 
  select(new_name, year) %>% 
  mutate(x = 'x') %>% 
  pivot_wider(
    names_from = 'year', 
    id_cols = 'new_name', 
    values_from = 'x') %>% 
  readr::write_csv(., file = questions_years_out)
