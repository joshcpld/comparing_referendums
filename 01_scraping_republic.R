library(tidyverse)
library(rvest)
library(glue)

link <- "https://www.aec.gov.au/elections/referendums/1999_referendum_reports_statistics/summary_republic.htm"

page <- read_html(link)


#Web-scraping national data

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
  





#Web-scraping electorate results


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
  rename(electorate = state) %>% 
  filter(!grepl("Total", electorate))



#Retrieve state assignments for each electorate

electorate_name_table_numbers <-seq(6, 13, by = 1)

state_names <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")



retrieve_electorate_states <- function(electorate_name_table_number){
  
  html_code <- glue("table:nth-child({electorate_name_table_number}) td:nth-child(1)")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
}


NSW_names <- as_tibble(electorate_states[1]) %>% 
  mutate(state = "NSW")

#Create a function that iterates over each element in the list, feeding is state names to mutate