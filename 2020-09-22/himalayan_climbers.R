## ---------------------------
## Script name: himalayan_climbers.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-09-22
## Purpose:
## Tidy Tuesday Week 39
## ---------------------------
## Notes:
## Tried to use hrbrthemes::theme_ft_rc() but had font issues
## ---------------------------

library(tidyverse)
library(paletteer)
library(ggdark)

# Load data
members <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# Let's just look at the top n peaks with the most climbers
top_peaks <- members %>% 
  count(peak_name, sort = TRUE) %>% 
  slice(1:20) %>% 
  pull(peak_name)

# Calculate proportion of female climbers per peak per year
(prop_women<- members %>% 
  filter(peak_name %in% top_peaks) %>% 
  count(peak_name, year, sex) %>% 
  pivot_wider(names_from = sex, values_from = n, names_prefix = "num_") %>% 
  replace_na(list(num_F = 0)) %>% 
  mutate(prop_F = num_F/(num_M + num_F),
         total_peeps = num_M + num_F) %>% 
  arrange(year)
)

# Set palette
cols <- paletteer_d("wesanderson::Zissou1")

# Plot
ggplot(prop_women, aes(x = year, y = factor(peak_name, levels = rev(top_peaks)))) +
  geom_point(aes(size = total_peeps, color = prop_F), alpha = 0.7) +
  scale_color_stepsn(colors = cols, n.breaks = 4, labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10)) +
  labs(x = NULL,
       y = NULL,
       size = "Total expedition\nmembers",
       color = "Percentage of\nwomen",
       title = "Women on Himalayan climbing expeditions",
       subtitle = "Each point represents the number of expedition members and percentage of women on that peak for that year",
       caption = "Data: The Himalayan Database\nFigure by @dm_ferraro for #TidyTuesday") +
  # theme_ipsum_rc() +
  dark_theme_gray(base_size = 14, base_family = "ArialNarrow") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 22)
  )
NULL

# Save
ggsave(here::here("2020-09-22", "himalayan_climbers.jpg"), width = 10.5, height = 7, dpi = 300)
