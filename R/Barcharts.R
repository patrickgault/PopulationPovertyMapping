library(tidyverse)

# Read in tables
worldPopViz.data <- read_csv("Input/nga_zonalStats_worldPop_classes_totalPop_povertyPop.csv")
ghsViz.data <- read_csv("Input/nga_zonalStats_GHS_classes_totalPop_povertyPop.csv")

# Rename Fields
worldPopViz.data <- rename(worldPopViz.data,
                           "Population" = TotalPopSUM, 
                           "Population in Poverty" = PovertySUM, 
                           "Urbaness.Class" = WorldPopCLASS)

ghsViz.data <- rename(ghsViz.data,
                      "Population" = TotalPopSUM,
                      "Population in Poverty" = PovertySUM,
                      "Urbaness.Class" = MunicipalLevelType)

## Create Long Tables and Sort Based on Zone ID
# WorldPop
worldPopViz.data.long <- worldPopViz.data %>% 
  pivot_longer(
    cols = -c(Urbaness.Class, PctPoverty, ZoneID),
    names_to = "Type",
    values_to = "Sum"
  )
worldPopViz.data.long %>% arrange((ZoneID))

# GHS
ghsViz.data.long <- ghsViz.data %>% 
  pivot_longer(
    cols = -c(Urbaness.Class,Category, PctPoverty, ZoneID),
    names_to = "Type",
    values_to = "Sum"
  )

ghsViz.data.long %>% arrange((ZoneID))

## Create Bar Charts
# WorldPop
worldPopViz <- ggplot(data = worldPopViz.data.long, aes(x = reorder(Urbaness.Class,ZoneID), y = Sum, fill = Type)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(
    data = worldPopViz.data.long %>% filter(Type == "Population"),
    aes(label = sprintf("%1.f%%", PctPoverty*100)), hjust = -1) + # I really don't know what I'm doing here but I rounded the values and got a % sign to show up
  labs(y = "2010 Population Count", x = "GHS Urbaness Class", fill = "", title = "Nigeria: GHS 2010 Population in Poverty") +
  theme_update(plot.title = element_text(hjust = 0.5)) + # to center title, prolly an easier way
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = ggplot2::element_blank()
  )
show(worldPopViz)

# GHS
ghsViz <- ggplot(data = ghsViz.data.long, aes(x = reorder(Urbaness.Class,ZoneID), y = Sum, fill = Type)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(
    data = ghsViz.data.long %>% filter(Type == "Population"),
    aes(label = sprintf("%1.f%%", PctPoverty*100)), hjust = -1) + # I really don't know what I'm doing here but I rounded the values and got a % sign to show up
  labs(y = "2010 Population Count", x = "GHS Urbaness Class", fill = "", title = "Nigeria: GHS 2010 Population in Poverty") +
  theme_update(plot.title = element_text(hjust = 0.5)) + # to center title, prolly an easier way
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = ggplot2::element_blank()
  )
show(ghsViz)
