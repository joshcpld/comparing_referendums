library(tidyverse)
library(rvest)
library(glue)


################################################################################
###################### Web-scraping electorate election data ###################
################################################################################

########importing electorate names and stat pairings############################

electorate_names <- read_csv("data/voice_referendum_electorate_results.csv") %>% 
  select(electorate, state)



####### scraping first preference count electorate election data ###############

link_numbers <- seq(100,200, by = 1)

column_names <- c("candidate", "party", "votes", "per_cent", "swing_per_cent", "status")



### first I need to webscrape relationships between names and link numbers before actually
#getting results