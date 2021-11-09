# Load libraries
library(tidyverse)
library(hrbrthemes)
library(gridExtra)
library(COVID19)

# Remove scientific notation
options(scipen = 999)

# Load data
covid_today <- covid19(start = Sys.Date() - 1)

################################
# 10 countries with more cases #
################################

# Order countries
covid_top10 <- covid_today %>% arrange(-confirmed)
covid_top10 <- covid_top10[1:10, ]
covid_top10 <- covid_top10 %>% arrange(deaths)

# Create barplot
top10 <- covid_top10 %>%
  ggplot(aes(x = deaths, y = factor(administrative_area_level_1, levels = administrative_area_level_1))) +
  geom_bar(stat = 'identity', show.legend = FALSE, fill = '#DC143C') +
  labs(
    y = '',
    x = 'Deaths',
    title = 'Top 10 countries with the\nmost COVID19 cases arranged by deaths',
    subtitle = Sys.Date()
  ) +
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90))

####################################
# Deaths and recovered proportions #
####################################

# Factor
situation <- c(rep('Deaths',10),rep('Recovered',10))

# Duplicate country names
country <- c(rep(covid_top10$administrative_area_level_1, 2))

# Deaths and recovered
data_situation <- c(covid_top10$deaths, covid_top10$recovered)

# Create dataframe
proportion <- data.frame(country,situation,data_situation)

# Create proportion barplot
top10_prop <- proportion %>%
  filter(!is.na(data_situation)) %>%
  ggplot(aes(x = country, y = data_situation, fill = situation)) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() +
  labs(
    y = 'Percentage',
    x = '',
    caption = 'Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub",\nWorking paper, doi: 10.13140/RG.2.2.11649.81763.'
  ) +
  scale_fill_manual(name = 'Situation', values = c('#DC143C','#ADFF2F')) +
  theme_ft_rc()

# Combine graphs
grid.arrange(top10, top10_prop, nrow = 1)