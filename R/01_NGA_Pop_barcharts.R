##  PROJECT: Poverty and Population classes
##  AUTHOR:  Patrick Gault, Tim Essam | USAID
##  PURPOSE: Plottin' stuff
##  LICENCE: MIT
##  DATE:    2020-04-24


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(scales)

# LOAD DATA ---------------------------------------------------------------

# Read in tables
  wp_df<- read_csv("Input/nga_zonalStats_worldPop_classes_totalPop_povertyPop.csv") %>% 
    rename("Population" = TotalPopSUM, 
      "pop_poverty" = PovertySUM, 
      "Urbaness.Class" = WorldPopCLASS) %>% 
    mutate(label = percent(PctPoverty %>% round(2)))
  
  ghs_df <- read_csv("Input/nga_zonalStats_GHS_classes_totalPop_povertyPop.csv") %>% 
    rename("Population" = TotalPopSUM,
      "pop_poverty" = PovertySUM,
      "Urbaness.Class" = MunicipalLevelType) %>% 
    mutate(label = percent(PctPoverty %>% round(2)))


# MUNGE  ------------------------------------------------------------------
# No need to reshape

# PLOT --------------------------------------------------------------------


## Create Bar Charts
# WorldPop
  wp_df %>% 
    select(-Population) %>% 
    # right now this is sorted on the pop_poverty, below. Change as needed.
    ggplot(aes(x = fct_reorder(Urbaness.Class, pop_poverty))) +
    geom_col(data = wp_df, aes(y = Population),
      fill = "#a6cee3") +
    coord_flip() +
    theme_minimal() + 
    scale_y_continuous(labels = scales::comma) +
    geom_col(aes(y = pop_poverty), fill = "#1f78b4") +
    geom_text(aes(y = pop_poverty, label = label),
      hjust = -0.25, size = 3) +
    theme(panel.grid.major.y = ggplot2::element_blank()
    ) +
    labs(x = "GHS Urbaness Class", y = NULL, 
      title = "Nigeria: GHS 2010 Population in Poverty")

ggsave("Awesome_plot0.pdf", plot = last_plot(),
  height = 4,
  width = 8, 
  useDingbats = FALSE)


# PLOT GHS

  ghs_df %>% 
    select(-Population) %>% 
    ggplot(aes(x = fct_reorder(Urbaness.Class, pop_poverty))) +
    geom_col(data = ghs_df, aes(y = Population),
      fill = "#a6cee3") +
    coord_flip() +
    theme_minimal() + 
    scale_y_continuous(labels = scales::comma) +
    geom_col(aes(y = pop_poverty), fill = "#1f78b4") +
    geom_text(aes(y = pop_poverty, label = label),
      hjust = -0.25, size = 3) +
    theme(panel.grid.major.y = ggplot2::element_blank()
    ) +
    labs(x = NULL, y = NULL, 
      title = "Nigeria: GHS 2010 Population in Poverty")

ggsave("Awesome_plot1.pdf", plot = last_plot(),
  height = 4,
  width = 8, 
  useDingbats = FALSE)
