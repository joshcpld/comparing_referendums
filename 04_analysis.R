library(tidyverse)
library(readxl)

################################################################################
###################### Import data into 1 df ###################################
################################################################################

# Referendum data ##############################################################

voice_data <- read_csv("data/voice_referendum_electorate_results.csv") %>% 
  select(electorate, state, yes_per_cent, no_per_cent)

# Election data ################################################################

election_data <- read_csv("data/electorate_election_data.csv") %>%
  pivot_wider(names_from = party_simple, values_from = per_cent) %>% 
  rename_at(vars(-electorate), ~ paste0("election_", .))

# Electorate SEIFA data ########################################################

seifa_data <- read_csv("data/electorate_seifas.csv")

# Geography data ###############################################################

geography_data <- read_csv("data/geography_classifications.csv")


# Combine into 1 df ############################################################

data <- voice_data %>% 
  inner_join(election_data , by = "electorate") %>% 
  inner_join(seifa_data, by = "electorate") %>% 
  inner_join(geography_data, by = "electorate") %>% 
  mutate(state_group = case_when(
    
    state == "NSW" ~ "NSW",
    state == "VIC" ~ "VIC",
    state == "QLD" ~ "QLD",
    TRUE ~ "Other states"
    
  )) %>% 
  mutate(state_group = as.factor(state_group)) %>% 
  mutate(state_group = fct_relevel(state_group, "NSW", "VIC", "QLD", "Other states")) %>% 
  filter(electorate != "Melbourne")



################################################################################
###################### Initial data exploration ################################
################################################################################

ggplot(data, aes(yes_per_cent, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + geom_vline(xintercept = 50, linetype = "dotted", color = "grey70", size = 1) + ggtitle("Distribution of yes voting rates")

# Election data ################################################################

ggplot(data, aes(election_Labor, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of Labor voting rates")

ggplot(data, aes(election_LNP, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of LNP voting rates")

ggplot(data, aes(election_Greens, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of LNP voting rates")

ggplot(data, aes(election_independent, fill = state)) + geom_histogram(bins = 20) + facet_wrap(~state_group, scales = "fixed") + ggtitle("Distribution of LNP voting rates")

# SEIFA data ###################################################################

ggplot(data, aes(electorate_irsad_seifa, fill = state)) + geom_histogram(bins = 20)

ggplot(data, aes(electorate_irsd_seifa, fill = state)) + geom_histogram(bins = 20)

ggplot(data, aes(electorate_ieo_seifa, fill = state)) + geom_histogram(bins = 20)

################################################################################
###################### Checking relationships with yes votes ###################
################################################################################

# Election data ################################################################

# Yes

ggplot(data, aes(yes_per_cent, election_Labor, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_LNP, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_Greens, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_independent, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

# No

ggplot(data, aes(no_per_cent, election_Labor, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_LNP, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_Greens, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_independent, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

# Relationship between Labor and Greens voting behaviour

ggplot(data, aes(election_Greens, election_Labor, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

cor(data$election_Greens, data$election_Labor)

vif_values <- car::vif(yes_vote_model)

print(vif_values)

# SEIFA data ###################################################################

# Yes

ggplot(data, aes(yes_per_cent, electorate_irsad_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, electorate_irsd_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, electorate_ieo_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

# No

ggplot(data, aes(no_per_cent, electorate_irsad_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, electorate_irsd_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, electorate_ieo_seifa, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")


################################################################################
###################### Modelling yes votes #####################################
################################################################################

# Yes ##########################################################################

yes_vote_model <- lm(yes_per_cent ~ election_Greens + electorate_irsad_seifa + inner_metro, data = data)

summary(yes_vote_model)

crPlots(yes_vote_model)

plot(yes_vote_model, which = 1)


# Is it reasonable to include Labor and Greens in the same model?

# Relationship between Labor and Greens voting behaviour

ggplot(data, aes(election_Greens, election_Labor, colour = state)) + geom_jitter() 

cor(data$election_Greens, data$election_Labor)

vif_values <- car::vif(yes_vote_model)

print(vif_values)


# No

no_vote_model <- lm(no_per_cent ~ election_LNP + electorate_irsad_seifa + outer_metro, data = data)

summary(no_vote_model)

crPlots(no_vote_model)

plot(no_vote_model, which = 1)
