## ---------------------------
## Script name: european_energy.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-08-04
## Purpose:
## Tidy Tuesday Week 32
## ---------------------------
## Notes:
##   
## ---------------------------

library(tidyverse)
library(ggtext)

# Read in data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

# Calculate percentage of total energy production from clean sources per country for 2016 and 2018
energy_types_wrangled <- energy_types %>% 
  mutate(country_name = replace_na(country_name, "United Kingdom")) %>% # Add missing country name
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "net_production") %>%
  mutate(type_group = if_else(type == "Conventional thermal", "conventional", "clean")) %>%
  filter(level == "Level 1") %>%  # Remove Level 2 data which is counted in Level 1
  group_by(country, country_name, year, type_group) %>%
  summarize(sum_net_production = sum(net_production)) %>%
  ungroup() %>%
  pivot_wider(names_from = "type_group", names_prefix = "prod_", values_from = "sum_net_production") %>%
  mutate(prop_clean = prod_clean/(prod_clean + prod_conventional)) %>%
  select(-prod_clean, -prod_conventional) %>%
  filter(year != 2017) %>% # Choose 2016 and 2018 data only
  group_by(country) %>% 
  mutate(increase_since_2016 = case_when(prop_clean[year == 2018] > prop_clean[year == 2016] ~ "Yes",
                                         prop_clean[year == 2018] < prop_clean[year == 2016] ~ "No",
                                         TRUE ~ "No change"))
# This feels like it could have been written more efficiently...


# For plot caption: How much did the proportion of clean energy in Europe overall change from 2016 to 2018?
energy_types %>% 
  mutate(country_name = replace_na(country_name, "United Kingdom")) %>% # Add missing country name
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "net_production") %>%
  mutate(type_group = if_else(type == "Conventional thermal", "conventional", "clean")) %>%
  filter(level == "Level 1", year != 2017) %>% 
  group_by(year, type_group) %>% 
  summarize(sum_production = sum(net_production)) %>% 
  group_by(year) %>% 
  summarize(prop_clean = sum_production[type_group == "clean"]/sum(sum_production))

# Define factor levels for plotting (sort countries by percentage of clean energy produced in 2018)
country_levels <- energy_types_wrangled %>% 
  filter(year == 2018) %>% 
  arrange(prop_clean) %>% 
  pull(country_name)

# Plot
ggplot(data = energy_types_wrangled, aes(x = prop_clean*100, y = factor(country_name, levels = country_levels), group = country, color = increase_since_2016)) +
  geom_vline(xintercept = 50, color = "gray80", linetype = "dashed") +
  geom_path(size = 0.7, lineend = "round", arrow = arrow(length = unit(0.8, "mm"), type = "closed", angle = 40)) +
  annotate("point", x = 100.5, y = 37, color = "gray") + # Just the hackiest way ever to cover up Albania's arrow with a point
  scale_color_manual(values = c("Yes" = "#749B58FF", "No" = "darkblue", "No change" = "gray")) +
  labs(title = "Changes in national clean energy production in Europe\nfrom 2016-2018",
       subtitle = "Overall, European countries increased the total proportion of clean energy<br>produced between 2016 and 2018 by about 2 percentage points. At the<br>national scale, 23 countries <b style='color:#749B58FF'>increased</b> their percentages of clean energy<br>and 13 <b style='color:darkblue'>decreased</b> their percentages. One country, Albania, showed<br><b style='color:gray'>no change,</b> with 100% of their energy produced from clean sources in<br>both 2016 and 2018. Here, 'clean' refers to renewable and/or nuclear energy.",
       x = "Percentage of clean energy",
       y = NULL,
       caption = "Arrows begin at the 2016 value and point to the 2018 value.\n \nData: Eurostat\nFigure by @dm_ferraro for #TidyTuesday") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.subtitle = element_markdown())

# Save
ggsave(here::here("2020-08-04", "european_energy.png"), height = 9, width = 6, dpi = 300)
