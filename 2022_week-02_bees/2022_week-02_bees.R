library(tidyverse)
library(patchwork)
library(showtext)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

df <- stressor %>%
  pivot_wider(
    names_from = stressor,
    values_from = stress_pct
  ) %>%
  left_join(colony, by = c("year", "months", "state"))


