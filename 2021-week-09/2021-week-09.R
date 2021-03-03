library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 10)
youtube <- tuesdata$youtube %>% 
  mutate(brand = recode(brand, "Hynudai" = "Hyundai"))