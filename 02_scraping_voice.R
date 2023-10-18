library(tidyverse)
library(rvest)
library(glue)

################################################################################
############################ Web-scraping national data ########################
################################################################################

link <- "https://tallyroom.aec.gov.au/ReferendumNationalResults-29581.htm"

page <- read_html(link)

column_numbers <- seq(1,  6, by = 1)

column_names_national<- c("state", "yes_no", "yes_per_cent", "no_num", "no_per_cent", "informal")


retrieve_national_referendum_data <- function(column_number){
  
  html_code <- glue("#nationalResults td:nth-child({column_number})")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
  return(column_data)
  
}

national_results <- map(column_numbers, retrieve_national_referendum_data)

national_results <- as.tibble(do.call(cbind, national_results)) %>% 
  set_names(column_names_national)




################################################################################
############################ Web-scraping electorate data ######################
################################################################################

state_names <-  c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")

column_names_electorate <- c("electorate", "enrolment", "yes_num", "yes_per_cent", "no_num",
                  "no_per_cent", "informal_num", "informal_per_cent","total_num")

column_numbers <- seq(1, 9, by = 1)

retrieve_electorate_data_by_state <- function(state_name){
  link <- glue("https://tallyroom.aec.gov.au/ReferendumStateResultsByDivision-29581-{state_name}.htm")
  page <- read_html(link)
  
  state_data <- map(column_numbers, ~{
    html_code <- glue("td:nth-child({.x})")
    column_data <- page %>% 
      html_nodes(html_code) %>% 
      html_text() 
    return(column_data)
  }) %>% 
    set_names(column_names_electorate) 
  
  return(as.tibble(do.call(cbind, state_data)))
}

# Use map to retrieve data for all states
electorate_results <- map(state_names, retrieve_electorate_data_by_state) %>% 
  set_names(state_names)



############################Assigning states to electorates#####################

add_state_identifier <- function(state_result, state_name) {
  new_vector <- rep(state_name, nrow(state_result))
  state_result[["state"]] <- new_vector
  return(state_result)
}

electorate_results <- map2(electorate_results, state_names, add_state_identifier) %>% 
  bind_rows()


################################################################################
############################### Data cleaning ##################################
################################################################################

########################### Cleaning national results ##########################

glimpse(national_results)

cols_to_exclude <- c("state")

national_results <- national_results %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), ~str_replace_all(.,",", "")) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), as.numeric)


########################### Cleaning electorate results ########################

glimpse(electorate_results)

cols_to_exclude <- c("state","electorate")

electorate_results <- electorate_results %>% 
  relocate(state, .before = enrolment) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), ~str_replace_all(.,",", "")) %>% 
  mutate_at(vars(-one_of(cols_to_exclude)), as.numeric)


################################################################################
############################### Data cleaning ##################################
################################################################################

write_csv(national_results, "data/voice_referendum_national_results.csv")

write_csv(electorate_results, "data/voice_referendum_electorate_results.csv")




