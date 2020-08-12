## ---------------------------
## Script name: avatar.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-08-11
## Purpose:
## Week 32
## ---------------------------
## Notes:
## Really just wanted a reason to use gganimate.   
## ---------------------------

library(tidyverse)
library(gganimate)

# Read in data
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

# I think I will attempt to plot the number of words per character over time

# Wrangle data

length(unique(avatar$character))

top_chars <- avatar %>% 
  filter(character != "Scene Description") %>% 
  count(character, sort = TRUE) %>% 
  slice(1:5) %>% 
  pull(character) # There are a lot of characters, so just look at the 5 with the most lines

avatar_wrangled <- avatar %>% 
  filter(character %in% top_chars) %>% 
  separate_rows(character_words, sep = " ") %>% # Split character's line into individual words
  count(book, chapter_num, character)

# Build static plot
(static <- ggplot(data = avatar_wrangled, aes(x = chapter_num, y = n, color = character, group = character)) +
    geom_point(size = 2) +
    geom_line(size = 1.1) +
    labs(x = "Chapter",
         y = "Number of words spoken",
         color = NULL,
         title = "Which 'Avatar: The Last Airbender' character is the chattiest?",
         caption = "Data: 'appa' R package created by Avery Robbins\nFigure by @dm_ferraro for #TidyTuesday") +
    facet_wrap(~book, ncol = 1) +
    paletteer::scale_color_paletteer_d(palette = "jcolors::pal7", direction = -1) +
    ggthemes::theme_tufte(base_size = 20, base_family = "Helvetica") +
    theme(strip.text = element_text(hjust = 0.1),
          legend.position = "top")
)

# Use gganimate::transition_reveal() to animate the plot
dynamic <- static + transition_reveal(chapter_num)

# Save
animate(dynamic, nframes = 100, end_pause = 45, width = 1000, height = 700, res = 75, renderer = gifski_renderer(here::here("2020-08-11", "avatar.gif")))
 


  