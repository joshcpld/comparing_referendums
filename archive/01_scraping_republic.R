library(tidyverse)
library(rvest)
library(glue)

link <- "https://www.aec.gov.au/elections/referendums/1999_referendum_reports_statistics/summary_republic.htm"

page <- read_html(link)


################################################################################
############################ Web-scraping national data ########################
################################################################################

column_numbers <- seq(1,13, by = 1)

column_names <- c("state", "result", "enrolment", "yes_num", "yes_per_cent", "no_num",
                  "no_per_cent", "formal_num", "formal_per_cent", "informal_num",
                  "informal_per_cent","total_num", "total_per_cent")

retrieve_national_referendum_data <- function(column_number){
  
  html_code <- glue("h2+ table td:nth-child({column_number})")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
  return(column_data)
  
}

national_results <- map(column_numbers, retrieve_national_referendum_data)

national_results <- as.tibble(do.call(cbind, national_results)) %>% 
  set_names(column_names) %>% 
  slice(-n())
  

################################################################################
########################## Web-scraping electorate data ########################
################################################################################


retrieve_electorate_referendum_data <- function(column_number){
  
  html_code <- glue("table+ table td:nth-child({column_number})")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
  return(column_data)
  
}

electorate_results <- map(column_numbers, retrieve_electorate_referendum_data)

electorate_results <- as.tibble(do.call(cbind, electorate_results)) %>% 
  set_names(column_names) %>% 
  rename(electorate = state)



############################Assigning states to electorates#####################


###   Retrieve state assignments for each electorate

electorate_name_table_numbers <-seq(6, 13, by = 1)

state_names <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")

retrieve_electorate_states <- function(electorate_name_table_number){
  
  html_code <- glue("table:nth-child({electorate_name_table_number}) td:nth-child(1)")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
}

electorate_states <- map(electorate_name_table_numbers, retrieve_electorate_states) %>% 
  set_names(state_names)

electorate_state_pairs <- map2_dfr(electorate_states, state_names, ~ data.frame(state = .y, electorate = .x))


####   Pair electorate results with their state


electorate_results <- electorate_results %>% 
  inner_join(electorate_state_pairs, by = "electorate")






################################################################################
############################### Data cleaning ##################################
################################################################################



########################### Cleaning national results ##########################

# Remove whitespace and turn relevant columns to doubles

glimpse(national_results)

cols_to_exclude <- c("state","result")

national_results <- national_results %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), ~str_replace_all(.,"\\s|\\xc2\\xa0", "")) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), as.numeric)
  

########################### Cleaning electorate results ########################

glimpse(electorate_results)

cols_to_exclude <- c("state","result", "electorate")

electorate_results <- electorate_results %>% 
  relocate(state, .before = result) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), ~str_replace_all(.,"\\s|\\xc2\\xa0", "")) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), as.numeric)
  



################################################################################
############################### Saving off data frames##########################
################################################################################

write_csv(national_results, "data/republic_referendum_national_results.csv")

write_csv(electorate_results, "data/republic_referendum_electorate_results.csv")





