## ---------------------------
## Script name: employment.R
## Author: Danielle Ferraro, UC Santa Barbara
## Date: 2021-02-23
## Purpose:
## Tidy Tuesday 2021, Week 9
## ---------------------------
## Notes:
## Playing with gt package   
## ---------------------------

library(tidyverse)
library(here)
library(gt)

# Load data
employed <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

str(employed)
str(earn)

# Quick wrangle to summarize data
(employed_summary <- employed %>% 
    na.omit() %>% 
    filter(!race_gender %in% c("TOTAL", "Women", "Men"),
           year == max(year)) %>% 
    group_by(industry, race_gender) %>%
    summarize(employ_n = sum(employ_n)) %>% 
    ungroup() %>% 
    group_by(industry) %>% 
    mutate(tot_employ = sum(employ_n),
           employ_n = paste0(scales::number(employ_n/1e6, accuracy = 0.01), " (", scales::percent(employ_n/tot_employ, accuracy = 1), ")")) %>% 
    arrange(desc(tot_employ)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = race_gender, values_from = employ_n) %>% 
    select(-tot_employ)
)

# Get totals to append later (def not the efficient way to do this)
totals <- employed %>% 
  na.omit() %>% 
  filter(!race_gender %in% c("TOTAL", "Women", "Men"),
         year == max(year)) %>% 
  group_by(race_gender) %>% 
  summarize(total = sum(employ_n)) %>% 
  mutate(display_total = paste0(scales::number(total/1e6), " (", scales::percent(total/sum(total), accuracy = 1), ")")) %>% 
  select(-total) %>% 
  pivot_wider(names_from = race_gender, values_from = display_total) %>% 
  mutate(industry = "Total") %>% 
  select(industry, everything())
  
# Let's make a table with {gt} shall we
(employed_gt <- employed_summary %>% 
    bind_rows(totals) %>% 
    gt() %>% 
    # Add title
    tab_header(title = md("**Employed persons by industry and race in 2020**")) %>% 
    # Bold column names
    cols_label(industry = md("**Industry**"),
               White = md("**White**"),
               "Black or African American" = md("**Black or<br>African American**"),
               Asian = md("**Asian**")) %>% 
    # Add banner column
    tab_spanner(label = "Number of people (x 1 million)",
                columns = vars(White, "Black or African American", Asian)) %>% 
    # Add captions
    tab_source_note(source_note = "Data: U.S. Bureau of Labor Statistics") %>% 
    tab_source_note(source_note = "Table by @dm_ferraro for #TidyTuesday") %>% # For some reason a newline wasn't working %>% 
    # Style the totals row
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = vars(industry, White, "Black or African American", Asian),
        rows = industry == "Total")
    ) %>% 
    # Style the title
    tab_style(
      style = list(
        cell_fill(color = "#95AFBA"),
        cell_text(weight = "bold")
      ),
      locations = cells_title(groups = "title")
    ) %>% 
    # Background color
    tab_options(
      table.background.color = "#F3F6F7"
    )
)

# Save
# Had to run webshot::install_phantomjs() first
# gtsave(employed_gt, filename = here("2021-02-23", "employment.png")) # Doesn't save with all of the markdown
gtsave(employed_gt, filename = here("2021-02-23", "employment.html"))



