library(tidyverse)
library(rvest)
library(glue)


################################################################################
###################### Web-scraping electorate election data ###################
################################################################################

########importing electorate names and state pairings###########################

electorate_names <- read_csv("data/voice_referendum_electorate_results.csv") %>% 
  select(electorate, state) %>% 
  filter(electorate != "Total")
 

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

generate_url <- function(number) {
  glue("https://results.aec.gov.au/27966/Website/HouseDivisionPage-27966-{number}.htm")
}

column_numbers <- seq(1, 6, by = 1)
column_names <- c("candidate", "party", "votes", "per_cent", "swing_per_cent", "status")

retrieve_electorate_election_data <- function(link, number) {
  page <- read_html(link)
  
  data <- map_dfc(column_numbers, function(column_number) {
    html_code <- glue("#fp tbody td:nth-child({column_number})")
    column_data <- page %>% 
      html_nodes(html_code) %>% 
      html_text()
    return(column_data)
  })
  
  colnames(data) <- column_names
  data$number <- number  
  
  return(data)
}

scrape_election_electorate_urls <- function(numbers) {
  links_vector <- map(numbers, ~generate_url(.x))
  result_df <- map2_dfr(links_vector, numbers, retrieve_electorate_election_data)
  return(result_df)
}

electorate_first_preferences_votes <- scrape_election_electorate_urls(electorate_webpage_pairings$link_number)



################################################################################
###################### Cleaning electorate election data #######################
################################################################################

electorate_election_data <- electorate_first_preferences_votes %>% 
  select(party,per_cent,link_number = number) %>% 
  inner_join(electorate_webpage_pairings, by = "link_number") %>% 
  select(electorate,state,party,per_cent) %>% 
  mutate_at(vars(one_of("per_cent")), ~str_replace_all(.,",", "")) %>% 
  mutate_at(vars(one_of("per_cent")), as.numeric) 


################## Splitting up parties into more succinct groups ##############

electorate_election_data <- electorate_election_data %>% 
  mutate(party_simple = case_when(
    
    str_detect(party, "Labor") ~ "Labor",
    party == "A.L.P." ~ "Labor",
    party %in% c("Liberal", "Liberal National Party of Queensland", "National Party", 
                 "National Party of Australia", "National Party of Australia - N.S.W.",
                 "The Nationals") ~ "LNP",
    party %in% c("The Greens", "Queensland Greens", "The Greens (WA)") ~ "Greens",
    TRUE ~ "independent"
    )) %>% 
  select(electorate,state,party,party_simple,per_cent) %>% 
  group_by(electorate,party_simple) %>% 
  summarise(per_cent = sum(per_cent))

write_csv(electorate_election_data, "data/electorate_election_data.csv")




























