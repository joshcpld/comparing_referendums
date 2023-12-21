library(tidyverse)
library(readxl)
library(glue)
library(janitor)
library(visdat)


################################################################################
############################ Importing all SA1 SEIFA data#######################
################################################################################
getwd()
# setwd("C:/Users/joshc/OneDrive/Desktop/git/voice_referendum/")


seifas <- read_excel("data/inputs/seifa/SA1_indexes.xlsx", sheet = "Table 1")

col_names <- c("sa1_code", "relative_disadvantage_index", "relative_disadvantage_decile",
               "relative_(dis)advantage_index","relative_(dis)advantage_decile",
               "economic_resources_index","economic_resources_decile",
               "education_occupation_index", "education_occupation_decile",
               "usual_resident_population")

names(seifas) <- col_names

seifas <- seifas %>% 
  select(sa1_code, contains("index")) %>% 
  mutate(sa1_code = as.numeric(sa1_code))
  slice(6:n())


################################################################################
##################### Importing all SA1 electorate pairings ####################
################################################################################

# First need to import 2016 SA1 pairings  

state_sheets <- c("nsw-Feb-2021-2016SA1.xlsx", "vic-July-2021-2016SA1.xlsx", 
                  "qld-Feb-2021-2016SA1.xlsx", "wa-August-2021-2016SA1.xlsx", 
                  "sa-Feb-2021-2016SA1.xlsx", "tas-Feb-2021-2016SA1.xlsx",
                  "nt-Feb-2021-2016SA1.xlsx","act-Feb-2021-2016SA1.xlsx")

import_electorate_pairings <- function(state_sheet){
  
  data <- read_excel(glue("data/inputs/seifa/electorate_sa1_pairings/{state_sheet}")) %>% 
    select(1:2) %>% 
    clean_names() %>% 
    select(electorate = division, sa1_code = sa1_code_2016_sa1s) %>% 
    mutate(electorate = str_to_title(tolower(electorate))) %>% 
    arrange(electorate) %>% 
    mutate(sa1_code = as.numeric(sa1_code)) %>% 
    na.omit()
  
  return(data)
  
}

electorate_sa1_pairings <- map(state_sheets, import_electorate_pairings) %>% 
  bind_rows()


# Convert to 2021 SA1 codes

old_to_new_census <- read_csv("data/inputs/seifa/2016_sa1_to_2021_sa1.csv") %>% 
  select(sa1_code_2016 = SA1_MAINCODE_2016, sa1_code = SA1_CODE_2021) %>% 
  mutate(sa1_code = as.numeric(sa1_code))


# Combine together

electorate_sa1_pairings <- electorate_sa1_pairings %>% 
  inner_join(old_to_new_census)


################################################################################
##################### Joining SA1 SEIFA codes with electorates #################
################################################################################

seifa_data <- seifas %>% 
  inner_join(electorate_sa1_pairings, by = "sa1_code")
