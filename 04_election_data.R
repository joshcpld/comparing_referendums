library(tidyverse)
library(rvest)
library(glue)


################################################################################
###################### Web-scraping electorate election data ###################
################################################################################

########importing electorate names and stat pairings############################

electorate_names <- read_csv("data/voice_referendum_electorate_results.csv") %>% 
  select(electorate, state)
 

####### Figuring out which electorates correspond with which link numbers ######

link_numbers <- seq(1,400, by = 1)

retrieve_election_electorate_webpage_pairing <- function(link_number) {
  link <- glue("https://results.aec.gov.au/27966/Website/HouseDivisionPage-27966-{link_number}.htm")
  
  # Try to read the HTML content, handle errors
  result <- tryCatch(
    {
      page <- read_html(link)
      data <- page %>% 
        html_nodes("#StandardHeading") %>% 
        html_text()
      return(list(link_number = link_number, data = data))
    },
    error = function(e) {
      cat("Error accessing:", link, "\n")
      # Print or handle the error as needed
      return(NULL)
    }
  )
  
  return(result)
}

electorate_webpage_pairings <- map(link_numbers,retrieve_election_electorate_webpage_pairing)

electorate_webpage_pairings <- electorate_webpage_pairings %>% 
  bind_rows() %>% 
  separate(data, into = c("electorate", "state"), sep = ", ", remove = FALSE) %>% 
  select(-data)


####### scraping first preference count electorate election data ###############

link <- "https://results.aec.gov.au/27966/Website/HouseDivisionPage-27966-197.htm"

page <- read_html(link)

column_numbers <- seq(1,  6, by = 1)

column_names <- c("candidate", "party", "votes", "per_cent", "swing_per_cent", "status")

retrieve_electorate_election_data <- function(column_number){
  
  html_code <- glue("#fp tbody td:nth-child({column_number})")
  
  column_data <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
  return(column_data)
  
}

aston_results <- map(column_numbers, retrieve_electorate_election_data)

aston_results <- as.tibble(do.call(cbind, aston_results)) %>% 
  set_names(column_names) %>% 
  mutate(electorate = "Aston")




data <- page %>% 
  html_nodes("#StandardHeading") %>% 
  html_text()

### first I need to webscrape relationships between names and link numbers before actually
#getting results


link_numbers <- seq(1,400, by = 1)

retrieve_election_electorate_webpage_pairing <- function(link_number) {
  link <- glue("https://results.aec.gov.au/27966/Website/HouseDivisionPage-27966-{link_number}.htm")
  
  # Try to read the HTML content, handle errors
  result <- tryCatch(
    {
      page <- read_html(link)
      data <- page %>% 
        html_nodes("#StandardHeading") %>% 
        html_text()
      return(list(link_number = link_number, data = data))
    },
    error = function(e) {
      cat("Error accessing:", link, "\n")
      # Print or handle the error as needed
      return(NULL)
    }
  )
  
  return(result)
}

electorate_webpage_pairings <- map(link_numbers,retrieve_election_electorate_webpage_pairing)

electorate_webpage_pairings <- electorate_webpage_pairings %>% 
  bind_rows() %>% 
  separate(data, into = c("electorate", "state"), sep = ", ", remove = FALSE) %>% 
  select(-data)
