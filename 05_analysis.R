library(tidyverse)

################################################################################
############################ Creating a single dataframe########################
################################################################################

voice_referendum_results <- read_csv("data/voice_referendum_electorate_results.csv")

census_data <- read_csv("data/census_data.csv")

################## Combine census_data with referendum #########################

referendum_results <- voice_referendum_results %>% 
  inner_join(census_data, by = "electorate") %>% 
  select(electorate, state, yes = yes_per_cent, median_age, median_family_income, 
         share_indigenous, share_Australia, share_post_grad) %>% 
  mutate(state_group = case_when(
    
    state == "NSW" ~ "NSW",
    state == "VIC" ~ "VIC",
    state == "QLD" ~ "QLD",
    TRUE ~ "Other states"
    
  )) %>% 
  mutate(state_group = as.factor(state_group)) %>% 
  mutate(state_group = fct_relevel(state_group, "NSW", "VIC", "QLD", "Other states"))



################################################################################
######################## Exploratory data analysis #############################
################################################################################


# Distributions of key variables

theme_set(theme_minimal())

ggplot(referendum_results, aes(x = yes, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + geom_vline(xintercept = 50, linetype = "dotted", color = "grey70", size = 1) + ggtitle("Distribution of yes voting rates")

ggplot(referendum_results, aes(x = median_age, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of median age")

ggplot(referendum_results, aes(x = median_family_income, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of median weekly family income")

ggplot(referendum_results, aes(x = share_indigenous, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of indigenous shares of electorate populations")

ggplot(referendum_results, aes(x = share_Australia, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of share of electorate population born in Australia")

ggplot(referendum_results, aes(x = share_post_grad, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of share of electorate population with post-grad degree")



# Scatter plots between yes vote % and census variables

ggplot(referendum_results, aes(x = yes, y = median_age, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and median age")

ggplot(referendum_results, aes(x = yes, y = median_family_income, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and median family weekly income")

ggplot(referendum_results, aes(x = yes, y = share_indigenous, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and indigenous status")

ggplot(referendum_results, aes(x = yes, y = share_Australia, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and Australian birthplace")

ggplot(referendum_results, aes(x = yes, y = share_post_grad, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and post-grad degrees")


#Some of the relationships look non-linear

ggplot(referendum_results, aes(x = yes, y = log(share_post_grad), colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and post-grad degrees")

ggplot(referendum_results, aes(x = yes, y = log(share_indigenous), colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed") + ggtitle("Relationship between yes vote and indigenous status")




################################################################################
################################# Modelling  ###################################
################################################################################

yes_vote_model <- lm(yes ~ median_family_income + share_indigenous + share_Australia + share_post_grad,
                     data = referendum_results)

summary(yes_vote_model)

yes_vote_model_2 <- lm(yes ~ share_Australia + log(share_post_grad) + median_family_income,
                       data = referendum_results)

summary(yes_vote_model_2)
