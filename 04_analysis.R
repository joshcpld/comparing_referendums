library(tidyverse)
library(readxl)
library(broom)
library(knitr)
library(sjPlot)

theme_set(theme_light())

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
  filter(electorate != "Melbourne") %>% 
  na.omit()



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

yes_vote_model <- lm(yes_per_cent ~ election_LNP + electorate_ieo_seifa, data = data)

summary(yes_vote_model)

################################################################################
########################### ARTICLE OUTPUTS ####################################
################################################################################

# Political chart

political_chart_data <- data %>% 
  select(electorate, yes_per_cent, election_LNP, election_Labor, election_Greens, election_independent) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "party", values_to = "first_pref_per_cent") %>% 
  mutate(party = str_remove(party, "election_"))

political_chart_data_colours <- c("LNP" = "blue", "Labor" = "red", "Greens" = "darkgreen", "independent" = "black")

political_chart <- ggsave(filename = "charts/political_chart.png", 
                          plot = ggplot(political_chart_data, aes(yes_per_cent, first_pref_per_cent, colour = party)) 
                          + geom_jitter() + facet_wrap(~party, scales = "fixed") + theme(legend.position = "none") 
                          + scale_color_manual(values = political_chart_data_colours) 
                          + labs(x = "Yes voting rate (%)", y = "2022 election first pref. voting rate (%)")
                          + ggtitle("Chart 1: Relationship between election and referendum voting") 
                          + scale_x_continuous(limits = c(1, 100))
                          + theme(plot.title = element_text(size = 10))) 


# Socio-economic chart

socio_economic_chart_data <- data %>% 
  select(electorate, yes_per_cent, electorate_irsad_seifa, electorate_irsd_seifa, electorate_ieo_seifa) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "index", values_to = "percentile") %>% 
  mutate(index = case_when(
    
    index == "electorate_ieo_seifa" ~ "IEO",
    index == "electorate_irsad_seifa" ~ "IRSAD",
    index == "electorate_irsd_seifa" ~ "IRSD"
    
  ))

socio_economic_chart <- ggsave(filename = "charts/socio_economic_chart.png",
                               plot = ggplot(socio_economic_chart_data, aes(yes_per_cent, percentile, colour = index)) 
                               + geom_jitter() + facet_wrap(~index) 
                               + theme(legend.position = "none")
                               + labs(x = "Yes voting rate (%)", y = "Electorate SEIFA percentile")
                               + ggtitle("Chart 2: Relationship between referedum voting and socio-economic status") 
                               + scale_x_continuous(limits = c(1, 100))
                               + theme(plot.title = element_text(size = 9)))

ggplot(socio_economic_chart_data, aes(yes_per_cent, percentile, colour = index)) + geom_jitter() + facet_wrap(~index) + theme(legend.position = "none")

# Geographic chart 

geographic_chart_data <- data %>% 
  select(electorate, yes_per_cent, inner_metro, outer_metro, provincial, rural, metro, non_metro) %>% 
  pivot_longer(-c(yes_per_cent, electorate), names_to = "classification", values_to = "true_false") %>% 
  filter(true_false == 1) %>%
  select(-true_false)

geographic_chart_data_1 <- geographic_chart_data %>% 
  filter(classification %in% c("metro", "non_metro")) %>% 
  mutate(classification = case_when(
    
    classification == "metro" ~ "Metropolitan",
    classification == "non_metro" ~ "Regional"
    
  ))

geographic_chart_data_2 <- geographic_chart_data %>% 
  filter(classification %in% c("inner_metro", "outer_metro", "provincial", "rural"))

ggsave(filename = "charts/geographic_chart.png", plot = ggplot(geographic_chart_data_1, aes(yes_per_cent, fill = classification)) 
       + geom_histogram(bins = 20) 
       + facet_wrap(~classification, scales = "fixed") 
       + theme(legend.position = "none")
       + labs(x = "Yes voting rate (%)", y = "")
       + ggtitle("Chart 3: Distribution of yes votes by geography") 
       + scale_x_continuous(limits = c(1, 100))
 )

ggplot(geographic_chart_data_1, aes(yes_per_cent, fill = classification)) + geom_histogram(bins = 20) + facet_wrap(~classification, scales = "fixed") + theme(legend.position = "none")

ggplot(geographic_chart_data_2, aes(yes_per_cent, fill = classification)) + geom_histogram(bins = 20) + facet_wrap(~classification, scales = "fixed") + theme(legend.position = "none")



# Endogeneity test chart

ggsave(filename = "charts/endogeneity_test.png", plot =  ggplot(data, aes(election_LNP, electorate_ieo_seifa)) 
       + geom_jitter()
       + labs(x = "LNP first preference rate (%)", y = "IEO SEIFA (percentile)")
       + ggtitle("Chart 4: Relationship between yes vote model ind. variable")
       + theme(plot.title = element_text(size = 10)))

cor(data$election_LNP, data$electorate_ieo_seifa)

# Linear regression table

tab_model(yes_vote_model,
          pred.labels = c("Intercept", "LNP first preference rate (%)", "IEO SEIFA (percentile)"),
          dv.labels = "Yes voting rate (%)",
          p.style = "stars")

# Alternative linear regression models

yes_vote_metro_model <- lm(yes_per_cent ~ election_LNP + electorate_ieo_seifa + metro, data = data)

summary(yes_vote_metro_model)

yes_vote_non_metro_model <- lm(yes_per_cent ~ election_LNP + electorate_ieo_seifa + non_metro, data = data)

summary(yes_vote_non_metro_model)

yes_vote_irsad_model <- lm(yes_per_cent ~ election_LNP + electorate_irsad_seifa, data = data)

summary(yes_vote_irsad_model)

yes_vote_irsd_model <- lm(yes_per_cent ~ election_LNP + electorate_irsd_seifa, data = data)

summary(yes_vote_irsd_model)

tab_model(yes_vote_irsad_model, yes_vote_irsd_model, yes_vote_metro_model, yes_vote_non_metro_model,
          pred.labels = c("Intercept", "LNP first preference rate (%)","IRSAD SEIFA (percentile)", "IRSD SEIFA (percentile)", "IEO SEIFA (percentile)", "Metropolitan electorate", "Regional electorate"),
          dv.labels = c("Model 2", "Model 3", "Model 4", "Model 5"),
          p.style = "stars")
