library(tidyverse)
library(readxl)

################################################################################
############################ Importing all SA1 SEIFA data#######################
################################################################################

# This data was received on special request from the ABS

seifas <- read_csv("data/inputs/seifa/CEDs_merged_output.csv") %>% 
  na.omit() %>% 
  select(SA1_code = SA1_CODE_2021, electorate_name = CED_NAME_2021, ends_with("AUS_PERCENTILE")) 
  

electorate_seifas <- seifas %>% 
  group_by(electorate)