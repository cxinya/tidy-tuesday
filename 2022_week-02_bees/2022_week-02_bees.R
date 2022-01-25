library(tidyverse)
library(patchwork)
library(showtext)

# Data

stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


# Add font

font_add(family = "rubik", regular = "fonts/RubikBeastly-Regular.ttf")
font_add(family = "dongle",
         regular = "fonts/Dongle-Regular.ttf",
         bold = "fonts/Dongle-Bold.ttf")
showtext_auto()

# For each year, calc rank of % colonies affected by stressors

stress_rank <- stressor %>%
  group_by(year, stressor) %>%
  summarize(avg_pct = mean(stress_pct, na.rm = T)) %>%
  group_by(year) %>%
  mutate(rank = rank(avg_pct)) %>%
  mutate(stressor = recode(stressor,
                           "Disesases" = "Diseases",
                           "Other pests/parasites" = "Other pests"))

stress_label <- stress_rank %>%
  filter(year == 2015) %>%
  arrange(rank)

# Colors

pal <- c(
  "#0c3329",
  "#d7c704",
  "#f6df04",
  "#ffb101",
  "#fbc803",
  "#155644")

# Bee icons

bees <- stress_label <- stress_rank %>%
  filter(year == 2021) %>%
  arrange(rank) %>%
  mutate(icon = str_glue("bees/{str_to_lower(str_replace(stressor, ' ', '_'))}.png"))


# Plot

asp_ratio <- 1.618

stress_rank %>%
  ggplot(aes(x = year, y = rank, group = stressor)) +
  ggbump::geom_bump(
    size = .5, linetype = "dashed",
    color = "darkgray") +
  geom_point(aes(color = stressor), size = 1.3, shape = 18) +
  ggimage::geom_image(
    data = bees,
    aes(image = icon, x = 2021, y = rank),
    size = 0.07, by = "width", nudge_x = .3) +
  labs(y = "Affected most colonies \U2192") +
  scale_color_manual(values = pal) +
  scale_y_continuous(
    limits = c(.5, 6.5),
    breaks = seq(1, 6, 1),
    labels = str_to_upper(stress_label$stressor)) +
  scale_x_continuous(limits = c(2015, 2021.5), breaks = seq(2015, 2021,1)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(color = pal, size = 65, family = "rubik", hjust = 1, margin = margin(r = -5, l = 20)),
    axis.text.x = element_text(color = "darkgray", size = 45, family = "dongle", margin = margin(t = -10)),
    axis.title.y = element_text(color = "darkgray", size = 60, family = "dongle", angle = 90, hjust = .8)
  ) +
  plot_annotation(
    title = toupper("Bees under stress"),
    subtitle = "Stressors ranked by % colonies affected",
    caption = "Source: USDA  |  Bee: Flaticon.com  |  Viz: Xin Yuen, @so_xinteresting  |  #TidyTuesday",
    theme = theme(
      plot.title = element_text(family = "dongle", face = "bold", size = 110, margin = margin(t = 15, l = 3), hjust = 0),
      plot.caption = element_text(family = "dongle", size = 40, color = "gray87", hjust = .5, margin = margin(t = 10, b = 5)),
      plot.subtitle = element_text(family = "dongle", size = 70, hjust = 0, lineheight = .3, margin = margin(t = 5, b = -5, l = 3)),
      plot.background = element_rect(fill = "white", color = NA)))


ggsave("2022-week02-bees.png", width = 7, height = 5, unit = "in")
