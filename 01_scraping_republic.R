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

retrieve_referendum_data <- function(column_number, column_name){
  
  html_code <- glue("h2+ table td:nth-child({column_number})")
  
  column_name <- page %>% 
    html_nodes(html_code) %>% 
    html_text()
  
  return(column_name)
  
}

test <- map(column_numbers, column_names, retrieve_referendum_data)

