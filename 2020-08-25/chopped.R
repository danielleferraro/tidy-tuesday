## ---------------------------
## Script name: chopped.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-08-25
## Purpose:
## Tidy Tuesday Week 35
## ---------------------------
## Notes:
##   
## ---------------------------

library(tidyverse)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

# Filter to episodes with descriptions and identify episodes with (explicitly stated) themes
chopped_filtered <- chopped %>% 
  filter(!is.na(episode_notes)) %>% 
  mutate(has_theme = if_else(str_detect(episode_notes, "theme"), "Theme", "No Theme"))

# PLOT 1: Dodged bar plot of number of themed/non-themed episodes per month
(eps_per_month <- chopped_filtered %>% 
    mutate(air_date = lubridate::mdy(air_date)) %>% 
    separate(air_date, sep = "-", into = c("year", "month", "day"), remove = FALSE) %>% 
    group_by(month) %>% 
    mutate(total_eps = n()) %>% 
    group_by(month, has_theme, total_eps) %>% 
    summarize(n = n()) %>% 
    mutate(pct = n/total_eps) %>% # Calculate % of total episodes
    ungroup() %>% 
    # Plot
    ggplot(aes(x = month, y = n, fill = has_theme)) +
    geom_col(position = position_dodge()) +
    labs(title = "Chopped: Themed Episodes",
         subtitle = str_wrap("20% of all Chopped episodes have a theme contestants must follow when making their dishes. Not suprisingly, most of the themed episodes fall around US holidays.", 140),
         x = NULL,
         y = "Number of episodes", 
         fill = NULL) +
    scale_x_discrete(labels = as.factor(month.abb)) +
    paletteer::scale_fill_paletteer_d(palette = "wesanderson::Moonrise2") +
    cowplot::theme_minimal_hgrid(18) +
    theme(legend.position = "top",
          plot.title = element_text(size = 28))
)

# PLOT 2: Boxplot and jittered scatterplot of ratings for themed and non-themed episodes
(ratings <- chopped_filtered %>% 
    ggplot(aes(x = has_theme, y = episode_rating)) +
    geom_jitter(aes(color = has_theme), alpha = 0.2) + 
    stat_summary(mapping = aes(color = has_theme),
                 fun = "median", geom = "point", size = 4) +
    stat_summary(mapping = aes(color = has_theme),
                 fun = "median", geom = "errorbar", size = 1, width = 0.2,
                 fun.max = function(x) median(x) + sd(x),
                 fun.min = function(x) median(x) - sd(x)) +
    labs(title = "Themed episodes do not have higher ratings",
         subtitle = str_wrap("However, two of the highest rated episodes had themes: 'Good Food That's Good for You' and 'Budget Baskets' both received a 9.2 rating.", 70),
         y = "IMDB Rating (0-10 scale)",
         x = NULL) +
    paletteer::scale_color_paletteer_d(palette = "wesanderson::Moonrise2") +
    cowplot::theme_minimal_hgrid(18) +
    theme(legend.position = "none",
          plot.title = element_text(size = 20))
)

# PLOT 3: Word cloud of ingredients in Halloween-themed episodes
halloween_ingredients <- chopped_filtered %>% 
  filter(str_detect(episode_notes, "halloween|Halloween")) %>% 
  select(appetizer, entree, dessert) %>% 
  unite(col = "ingredients", sep = ", ") %>% 
  separate_rows(ingredients, sep = ", ") %>% 
  count(ingredients, sort = TRUE) 

# library(wordcloud)
# wordcloud <- wordcloud(words = halloween_ingredients$ingredients, freq = halloween_ingredients$n, scale = c(0.2,1))
# Decided to use {ggwordcloud} instead to work better with {patchwork}

library(ggwordcloud)
wordcloud <- ggplot(halloween_ingredients, aes(label = ingredients, size = n)) +
  geom_text_wordcloud_area(eccentricity = 0.9, 
                           grid_margin = 0.1,
                           max_grid_size = 10,
                           rm_outside = TRUE, 
                           color = paletteer::paletteer_d("wesanderson::Moonrise2", n = 2)[[2]]) +
  scale_size_area(max_size = 5) +
  labs(title = "Ingredients in Halloween-themed episodes") +
  cowplot::theme_map(14) +
  theme(plot.title = element_text(size = 20))

# ggwordcloud(halloween_ingredients$ingredients, halloween_ingredients$angle, scale = c(2, 0.5))

# FINAL PLOT: Stitch plots together
library(patchwork)
eps_per_month / (ratings + wordcloud) + 
  plot_annotation(caption = "Data: Kaggle, IMDB\nFigure by @dm_ferraro for #TidyTuesday") &
  theme(plot.caption = element_text(size = 14))

# Noticing that the area between the wordcloud title and the actual wordcloud matches that of the plot to the left. 
# Not sure how I can avoid this?

# Save
ggsave(here::here("2020-08-25", "chopped.png"), width = 15, height = 9.7, units = "in", dpi = 400)


