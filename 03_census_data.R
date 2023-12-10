library(tidyverse)
library(readxl)

#Objective of this script is to develop statistics for each Commonwealth electorate:
# 
#   - Age: median age of persons
#   - Income: median total family income
#   - Indigenous: share of indigenous population
#   - Ethnicity: share of population born overseas
#   - Education: share of population with a masters

################################################################################
################################## Creating data ###############################
################################################################################



################################## Age #########################################


age <- read_csv("data/census_data/2021Census_G02_AUST_CED.csv") %>% 
  select(CED_CODE_2021, median_age = Median_age_persons)



################################## Income ######################################


income <- read_csv("data/census_data/2021Census_G02_AUST_CED.csv") %>% 
  select(CED_CODE_2021, median_family_income = Median_tot_fam_inc_weekly)


# Turning income series into a series which identifies each electorate as: low, middle
# or high income.

income_group <- income %>% 
  mutate(percentile = rank(median_family_income, na.last = "keep") / max(rank(median_family_income, na.last = "keep")) * 100) %>% 
  mutate(percentile = round(percentile, 1)) %>% 
  mutate(income_group = case_when(
    
    percentile <= 33.3 ~ "Low income",
    percentile > 33.3 & percentile <= 66.6 ~ "Middle income",
    percentile > 66.6 ~ "High Income"
    
  )) %>% 
  group_by(income_group) %>% 
  select(CED_CODE_2021, income_group)



################################## Indigenous ##################################


indigenous <- read_csv("data/census_data/2021Census_G07_AUST_CED.csv") %>% 
  select(CED_CODE_2021, Tot_Indigenous_P,Tot_Tot_P) %>% 
  mutate(share_indigenous = (Tot_Indigenous_P / Tot_Tot_P) * 100) %>% 
  mutate(share_indigenous = round(share_indigenous,2)) %>% 
  select(CED_CODE_2021, share_indigenous)
  


################################## Ethnicity ###################################


ethnicity <- read_csv("data/census_data/2021Census_G09F_AUST_CED.csv") %>%
  inner_join(read_csv("data/census_data/2021Census_G09H_AUST_CED.csv"), by = "CED_CODE_2021") %>% 
  select(CED_CODE_2021, P_Australia_Tot, P_Tot_Tot) %>% 
  mutate(share_Australia = (P_Australia_Tot / P_Tot_Tot) * 100) %>%
  mutate(share_Australia = round(share_Australia, 2)) %>% 
  select(CED_CODE_2021, share_Australia)



################################## Education ###################################


education <- read_csv("data/census_data/2021Census_G49B_AUST_CED.csv") %>% 
  select(CED_CODE_2021, P_PGrad_Deg_Total, P_BachDeg_Total, P_Tot_Total) %>% 
  mutate(share_post_grad = (P_PGrad_Deg_Total / P_Tot_Total) * 100) %>% 
  mutate(share_post_grad = round(share_post_grad, 2)) %>%
  mutate(share_under_grad = (P_BachDeg_Total / P_Tot_Total) * 100) %>%
  mutate(share_under_grad = round(share_under_grad, 2)) %>%
  select(CED_CODE_2021, share_post_grad, share_under_grad)


  
################################################################################
########################## Compiling into one dataframe ########################
################################################################################

census_data <- age %>% 
  inner_join(education, by = "CED_CODE_2021") %>% 
  inner_join(ethnicity, by = "CED_CODE_2021") %>% 
  inner_join(indigenous, by = "CED_CODE_2021") %>% 
  inner_join(income, by = "CED_CODE_2021") %>% 
  inner_join(income_group, by = "CED_CODE_2021")

####################### Assigning codes to electorate names ####################

electorate_name_code_pairs <- read_excel("data/census_data/2021Census_geog_desc_1st_2nd_3rd_release.xlsx",
                                         sheet = "2021_ASGS_Non_ABS_Structures") %>% 
  filter(str_detect(ASGS_Structure, "CED")) %>% 
  select(CED_CODE_2021 = Census_Code_2021, electorate = Census_Name_2021)


census_data <- census_data %>% 
  inner_join(electorate_name_code_pairs, by = "CED_CODE_2021") %>% 
  relocate(electorate, .before = median_age)
  

################################################################################
########################## Saving off dataframes ###############################
################################################################################

write_csv(census_data, "data/census_data.csv")
