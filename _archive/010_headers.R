library(tidyverse)
library(alrtools)

# Check headers of data files

data_dir <- 'raw_data'
data_file_pattern <- '^technology_survey_responses_([0-9]{4})[.]csv$'

csv_paths <- dir(data_dir, pattern = data_file_pattern)

csvs <- list()
years <- sub(data_file_pattern, '\\1', csv_paths)

for (i in 1:length(csv_paths)) {
  p <- file.path(data_dir, csv_paths[i])
  csvs[[years[i]]] <- read.csv(p, header = FALSE)
}

csvs[[1]] %>% 
  as_tibble %>% 
  slice(3:nrow(.)) %>% 
  alrtools::info()


csvs[[1]] %>% as_tibble

a <- csvs[[1]] %>% 
  head(2) %>% 
  as.matrix %>% t %>% unname %>% 
  as_tibble

for (i in 2:nrow(a)) {
  if (coalesce(a$V1[i], '') == '') {
    a$V1[i] <- a$V1[i - 1]}
}

print(a, n = 20)






