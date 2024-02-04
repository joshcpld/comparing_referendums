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

# Note these data represent electorate percentiles

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

ggplot(data, aes(yes_per_cent, election_Labor, colour = state)) + geom_jitter()

ggplot(data, aes(yes_per_cent, election_LNP, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_LNP, colour = state)) + geom_jitter()

ggplot(data, aes(yes_per_cent, election_Greens, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_Greens, colour = state)) + geom_jitter()

ggplot(data, aes(yes_per_cent, election_independent, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(yes_per_cent, election_independent, colour = state)) + geom_jitter()


  # No

ggplot(data, aes(no_per_cent, election_Labor, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_LNP, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_Greens, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

ggplot(data, aes(no_per_cent, election_independent, colour = state)) + geom_jitter() + facet_wrap(~state_group, scales = "fixed")

# Relationship between Labor and Greens voting behaviour




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

yes_vote_model <- lm(yes_per_cent ~ election_LNP + electorate_ieo_seifa + outer_metro, data = data)

summary(yes_vote_model)

crPlots(yes_vote_model)

plot(yes_vote_model, which = 1)



################################################################################
########################### ARTICLE CHARTS #####################################
################################################################################

# Political chart

political_chart_data <- data %>% 
  select(electorate, yes_per_cent, election_LNP, election_Labor, election_Greens, election_independent) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "party", values_to = "first_pref_per_cent")

political_chart_data_colours <- c("election_LNP" = "blue", "election_Labor" = "red", "election_Greens" = "darkgreen", "election_independent" = "black")

ggplot(political_chart_data, aes(yes_per_cent, first_pref_per_cent, colour = party)) + geom_jitter() + facet_wrap(~party, scales = "fixed") + theme(legend.position = "none") + scale_color_manual(values = political_chart_data_colours)


# Socio-economic chart

socio_economic_chart_data <- data %>% 
  select(electorate, yes_per_cent, electorate_irsad_seifa, electorate_irsd_seifa, electorate_ieo_seifa) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "index", values_to = "percentile")

ggplot(socio_economic_chart_data, aes(yes_per_cent, percentile, colour = index)) + geom_jitter() + facet_wrap(~index) + theme(legend.position = "none")

# Geographic chart 

geographic_chart_data <- data %>% 
  select(electorate, yes_per_cent, inner_metro, outer_metro, provincial, rural, metro, non_metro) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "classification", values_to = "true_false") %>% 
  filter(true_false == 1) %>%
  select(-true_false)

geographic_chart_data_1 <- geographic_chart_data %>% 
  filter(classification %in% c("metro", "non_metro"))

geographic_chart_data_2 <- geographic_chart_data %>% 
  filter(classification %in% c("inner_metro", "outer_metro", "provincial", "rural"))

ggplot(geographic_chart_data_1, aes(yes_per_cent, fill = classification)) + geom_histogram(bins = 20) + facet_wrap(~classification, scales = "fixed") + theme(legend.position = "none")

ggplot(geographic_chart_data_2, aes(yes_per_cent, fill = classification)) + geom_histogram(bins = 20) + facet_wrap(~classification, scales = "fixed") + theme(legend.position = "none")
