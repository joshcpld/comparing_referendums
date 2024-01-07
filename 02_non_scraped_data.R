library(tidyverse)
library(readxl)

################################################################################
################################ SEIFA data ####################################
################################################################################

# This data was received on special request from the ABS

seifas <- read_csv("data/inputs/seifa/CEDs_merged_output.csv") %>% 
  na.omit() %>% 
  select(SA1_code = SA1_CODE_2021, electorate = CED_NAME_2021, ends_with("AUS_PERCENTILE")) %>% 
  mutate(electorate = case_when(
    
    electorate == "North Sydne" ~ "North Sydney",
    TRUE ~ electorate
    
  ))
  

electorate_seifas <- seifas %>% 
  group_by(electorate) %>% 
  summarise(electorate_irsad_seifa = median(IRSAD_AUS_PERCENTILE),
            electorate_irsd_seifa = median(IRSD_AUS_PERCENTILE),
            electorate_ieo_seifa = median(IEO_AUS_PERCENTILE),
            electorate_ieo_seifa = median(IER_AUS_PERCENTILE)) 

write_csv(electorate_seifas, "data/electorate_seifas.xlsx")


################################################################################
################################ Geography data ################################
################################################################################


geography_classifications <- read_excel("data/inputs/AEC_geography_classifications.xlsx")

write_csv(geography_classifications, "data/geography_classifications.csv")


