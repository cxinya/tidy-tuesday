library(tidyverse)
library(showtext)

font_add_google("Montserrat", "mont")
font_add_google("Roboto Slab", "robo")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2021, week = 10)
youtube <- tuesdata$youtube %>% 
  mutate(brand = recode(brand, "Hynudai" = "Hyundai"))


# Brands w each criteria ----------------------------------------------------------

criteria <- c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")

criteria_brand <- youtube %>% 
  select(brand, all_of(criteria)) %>% 
  pivot_longer(
    cols = all_of(criteria),
    names_to = "criteria") %>% 
  group_by(criteria, value) %>% 
  count(brand) %>% 
  pivot_wider(
    names_from = value,
    values_from = n,
    names_prefix = "criteria_"
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(criteria_per = criteria_TRUE / (criteria_FALSE + criteria_TRUE) * 100) %>% 
  arrange(criteria, criteria_per) %>% 
  mutate(
    criteria_label = recode(criteria,
      "show_product_quickly" = "Shows product quickly",
      "patriotic" = "Patriotic",
      "funny" = "Contains humor",
      "celebrity" = "Contains celebrity",
      "danger" = "Contains danger",
      "animals" = "Contains animals",
      "use_sex" = "Uses sexuality"),
    order = row_number())


# Plot --------------------------------------------------------------------


# Source: https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

criteria_brand %>% 
  ggplot(aes(x = order, y = criteria_per, fill = brand)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ criteria_label, scales = "free_y") +
  geom_text(
    aes(label = brand, y = 0),
    hjust = 0, color = "white", family = "mont")
  