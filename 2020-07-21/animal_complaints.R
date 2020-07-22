## ---------------------------
## Script name: animal_complaints.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-07-21
## Purpose:
## Tidy Tuesday Week 30
## ---------------------------
## Notes:
##   
## ---------------------------

library(tidyverse)

# Read in data and icon for plot
animal_complaints <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv') %>% 
  janitor::clean_names()

dog <- png::readPNG(here::here("2020-07-21", "dog.png"))
dog <- grid::rasterGrob(dog)

# Which month has the most dog complaints?
animal_complaints %>% 
  filter(animal_type == "dog") %>% 
  separate(date_received, into = c("month", "year"), sep = " ") %>%
  count(month) %>% 
  arrange(desc(n)) # June!

# Create month key with month abbrevations
month_key <- data.frame(cbind(month.name, month.abb))
month_key <- rename(month_key, month_short = month.abb)

# Filter to dogs only, and group by month and complaint type
animal_complaints_data <- animal_complaints %>% 
  filter(animal_type == "dog") %>% 
  separate(date_received, into = c("month", "year"), sep = " ") %>%
  left_join(month_key, by = c("month" = "month.name")) %>% 
  mutate(month_short = factor(month_short, levels = month.abb)) %>% 
  count(month_short, complaint_type)

# Plot
ggplot(data = animal_complaints_data, aes(x = month_short, y = n, color = complaint_type, group = complaint_type)) + 
  geom_line(size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "gray50") +
  geom_curve(x = 6, xend = 9, y = 1100, yend = 1120, angle = 20, color = "gray50", arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 9.25, y = 1120, hjust = 0, color = "gray50", label = str_wrap("The highest total number of dog-related complaints was made in June.", 30)) +
  annotation_custom(dog, xmin = 10.6, xmax = 11.5, ymin = 1010, ymax = 1085) +
  scale_color_paletteer_d(palette = "LaCroixColoR::PeachPear") +
  labs(x = NULL,
       y = "Number of complaints", 
       color = "Complaint type",
       title = "Seasonal dog complaints in Australia",
       caption = "Data: Townsville City Council Animal Complaints\nFigure by @dm_ferraro for #TidyTuesday") +
  cowplot::theme_minimal_hgrid() +
  theme(axis.line.x = element_blank(),
        plot.caption = element_text(hjust = 0))

# Save
ggsave(here::here("2020-07-21", "animal_complaints_plot.png"), width = 10, height = 6,units = 'in')
