## ---------------------------
## Script name: scooby_doo.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2021-07-16
## Purpose:
## Tidy Tuesday Week 29
## ---------------------------
## Notes:
## Trying out a waffle plot   
## ---------------------------

# Load packages
library(tidyverse)
library(showtext) # For fonts
library(waffle)
# Note: had to install {waffle} via below line.
# install.packages("waffle", repos = "https://cinc.rud.is")
# devtools::install_github("hrbrmstr/waffle") returned an error

# Read in data
scoobydoo <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
head(scoobydoo)

# Get number of caught monsters per character
catches_per_character <- scoobydoo %>% 
  select(-caught_not) %>% # Remove data where the monster wasn't caught
  mutate(across(starts_with("caught"), ~na_if(.x, "NULL")),
         across(starts_with("caught"), as.logical)) %>% 
  pivot_longer(starts_with("caught"), names_to = "catcher", values_to = "caught") %>% 
  group_by(catcher) %>% 
  summarize(n_caught = sum(caught, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(catcher = word(catcher, 2, sep = "_"), # Extract name only
         catcher = str_to_title(catcher)) %>% # Capitalize name 
  arrange(desc(n_caught))

# Make color palette
pal <- c("#128a84",
         "#79af30",
         "#ffe97f",
         "#bb5c37",
         "#4b0055",
         "#8e6345")

# Plotty plot - let's waffle

## The basic way
waffle(deframe(catches_per_character), 
       title = "Zoinks! Who caught the most monsters on Scooby Doo?",
       rows = 15,
       colors = pal
      )

## The ggplot way, with some flair via fonts
ggplot(data = catches_per_character, aes(fill = fct_relevel(catcher, levels = catcher), values = n_caught)) +
  geom_waffle(n_rows = 15, size = 0.33, color = "white") +
  coord_equal() +
  scale_fill_manual(values = pal) +
  labs(title = "Zoinks!",
       subtitle = "Who caught the most monsters on Scooby Doo?",
       fill = NULL) +
  theme_void(base_size = 14) +
  theme_enhance_waffle() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(0,0,5,0),
                                  family = "Shrikhand",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                  margin = margin(0,0,15,0))) +
  guides(fill = guide_legend(nrow = 1))

# Save
ggsave(here::here("2021-07-16", "scooby_doo.png"), height = 4, width = 6)
