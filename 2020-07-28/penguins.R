## ---------------------------
## Script name: penguins.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2020-07-28
## Purpose:
## Tidy Tuesday Week 31
## ---------------------------
## Notes:
## coord_map() in ggplot credit to @alexcookson (https://gist.github.com/tacookson/6ae6bfe9de0d116112d1c829121b8751)
## ---------------------------

library(palmerpenguins)
library(tidyverse)
library(cowplot)

# Inspect
head(penguins_raw)
head(penguins)

penguins %>% 
  map_df(~sum(is.na(.)))

# Wrangle
penguins_wrangled <- penguins %>% 
  drop_na(sex) %>% 
  pivot_longer(cols = bill_length_mm:body_mass_g, names_to = "metric", values_to = "measurement") %>% 
  filter(metric == "body_mass_g")

# Make a violin plot for each island that depicts penguin body size per species and sex
penguin_plots <- penguins_wrangled %>% 
  group_split(island) %>% 
  map(
    ~ggplot(., aes(x = species, y = measurement/1000, color = sex)) +
      geom_violin(position = position_dodge(width = 0.7), fill = NA) +
      geom_boxplot(position = position_dodge(width = 0.7), fill = NA, width = 0.2, show.legend = FALSE) +
      #geom_point(alpha = 0.5, position = position_jitterdodge(dodge.width = 0.7)) + # Didn't use this but fun to learn position_jitterdodge()!
      labs(x = NULL,
           y = "Body mass (kg)",
           color = NULL,
           title = .$island) +
      paletteer::scale_color_paletteer_d(palette = "wesanderson::Cavalcanti1", labels = c("Female", "Male")) +
      theme_minimal_hgrid(font_size = 13) +
      theme(axis.line.x = element_blank(),
            panel.grid = element_line(color = "black"),
            axis.ticks = element_blank())
  )

antarctica <- map_data("world", region = "Antarctica")
wap <- filter(antarctica, between(long, -77, -55), between(lat, -70, -60))

islands <- tibble(island = c("Biscoe", "Torgersen", "Dream"),
                  label = c("A", "C", "B"),
                  x = c(-66.675014, -64.083335, -64.159025),
                  y = c(-66.186419, -64.766668, -64.687260))

(anta_plot <- ggplot() +
    geom_polygon(data = wap, aes(x = long, y = lat, group = group), fill = "antiquewhite", color = "white") +
    geom_point(data = islands, aes(x = x, y = y), size = 0.5) +
    ggrepel::geom_text_repel(data = islands, aes(x = x, y = y, label = island)) +
    # annotate("text", x = -66.675014, y = -66.186419, label = "A") + # Biscoe
    # annotate("text", x = -64.083335, y  = -64.766668, label = "B") + # Torgersen
    # annotate("text", x = -64.159025, y  = -64.687260, label = "C") + # Dream
    coord_map(projection = "ortho", orientation = c(-90, 0, 0)) +
    theme_void()
)

# Stitch plots together
ggdraw() +
  draw_plot(anta_plot) +
  draw_plot(penguin_plots[[1]] + theme(legend.position = "none", axis.title.y = element_blank()), x = .25, y = .2, width = .17, height = .3) +
  draw_plot(penguin_plots[[2]], x = .1, y = .55, width = .3, height = .3) +
  draw_plot(penguin_plots[[3]] + theme(legend.position = "none", axis.title.y = element_blank()), x = .6, y = .45, width = .17, height = .3) +
  draw_text("Size of penguins on the Western Antarctic Peninsula", x = 0.05, y = 0.95, hjust = 0, vjust = 0, size = 20, fontface = "bold") +
  draw_text("Data: Gorman, William, and Fraser (2014)\nFigure by @dm_ferraro for #TidyTuesday", x = 0.05, y = 0.05, hjust = 0, vjust = 0, size = 10)

# Save
ggsave(here::here("2020-07-28", "penguins_plot.png"), width = 10, height = 6, units = 'in')







  



                 