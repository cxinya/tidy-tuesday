library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(emo)

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
  arrange(criteria, desc(criteria_per)) %>% 
  mutate(
    criteria_label = recode(criteria,
      "show_product_quickly" = "Product glimpse",
      "patriotic" = "Ammurica",
      "funny" = "Lololol",
      "celebrity" = "Celebrity sighting",
      "danger" = "Yikes! Danger!",
      "animals" = "It's a zoo",
      "use_sex" = "Ooo, sexy"),
    brand_order = row_number(),
    brand_label = ifelse(brand_order <= 3, brand, "")) %>% 
  group_by(criteria_label) %>% 
  mutate(criteria_freq = sum(criteria_TRUE)) %>% 
  arrange(desc(criteria_freq)) %>% 
  mutate(criteria_label = factor(criteria_label, levels = unique(.$criteria_label)))

# Plot --------------------------------------------------------------------


# Source: https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

pal <- c("#0C215D","#D47925","#4CB2C1","#5A75C7","#084B55","#8A620C","#197582","#D49D25","#8A490C","#26418F")


criteria_brand %>% 
  ggplot(aes(x = brand_order, color = brand)) +
  geom_point(aes(y = criteria_per), alpha = 0) +
  geom_segment(
    aes(xend = brand_order, y = 0, yend = criteria_per),
    size = 5.5, lineend = "round", show.legend = F) +
  facet_wrap(vars(criteria_label), scales = "free_x", ncol = 3, dir = "v", strip.position = "bottom") +
  geom_text(
    aes(label = round(criteria_per, 0), y =ifelse(criteria_per > 0, criteria_per - 1, 0)),
    family = "mont", size = 7, color = "white", fontface = "bold", show.legend = F) +
  geom_text(
    aes(label = brand_label, y = criteria_per + 8),
    hjust = 0, family = "mont", size = 8, fontface = "bold", angle = 30, show.legend = F) +
  labs(y = "Brands' ads featuring each theme (%)") +
  scale_color_manual(values = pal, name = "Brands") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 120)) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_void() +
  theme(
    text = element_text(family = "mont"),
    strip.text = element_text(family = "robo", size = 30, face = "bold", margin = margin(t = 7, b = 2)),
    strip.placement = "outside",
    panel.spacing.x = unit(.5, "cm"),
    panel.spacing.y = unit(.5, "cm"),
    legend.title = element_text(family = "robo", size = 35, hjust = .5, face = "bold", margin = margin(b = -10)),
    legend.text = element_text(size = 30, margin = margin(l = -15)),
    legend.position = c(.85,.3),
    axis.title.y = element_text(family = "robo", size = 35, face = "bold", angle = 90, margin = margin(r = 20)),
    axis.line.x = element_line(color = "gray71", size = 1.5),
    plot.margin = margin(rep(15, 4))) +
  plot_annotation(
    title = toupper("Super Bowl ad themes, 2000-2020"),
    caption = "Source: FiveThirtyEight & superbowl-ads.com  |  Viz: Xin Yuen @so_xinteresting  |  #TidyTuesday",
    theme = theme(
      plot.title = element_text(family = "robo", face = "bold", size = 75, margin = margin(t = 5)),
      plot.caption = element_text(family = "robo", size = 20, hjust = .5, color = "gray41", margin = margin(t = -5))))



ggsave("2021-week-09-superbowl.png", width = 7, height = 8, units = "in")

