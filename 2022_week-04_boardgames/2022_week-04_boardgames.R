library(tidyverse)
library(patchwork)
library(showtext)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')


# Game designers

designers <- details %>%
  select(id, primary, boardgamedesigner) %>%
  mutate(boardgamedesigner = gsub(", Jr.", " Jr.", boardgamedesigner)) %>% # Remove comma before Jr. so isn't split
  mutate(boardgamedesigner = gsub(", III", " III", boardgamedesigner)) %>% # Remove comma before Jr. so isn't split
  mutate(boardgamedesigner = gsub("\\[|\\]|\\'|\"", "", boardgamedesigner)) %>% # Remove brackets and quotes in names
  splitstackshape::cSplit("boardgamedesigner", ",") %>%
  pivot_longer(
    cols = -c(id, primary),
    values_to = "designer",
    values_drop_na = T
  ) %>%
  mutate(designer = gsub("<.*>", "", designer)) %>%
  filter(designer != "(Uncredited)") %>%
  left_join(
    select(ratings, id, bayes_average, average, year),
    by = "id"
  )

# Game counts by designer

designer_count <- designers %>%
  group_by(designer) %>%
  summarize(
    games = n(),
    hi = max(average),
    lo = min(average)
  ) %>%
  arrange(desc(games)) %>%
  mutate(
    productivity = row_number(),
    designer_games = str_glue("{str_replace(str_to_upper(designer), ' ', '\n')}\n\n{games} games"))

# Plot

df <- designers %>%
  left_join(designer_count, by = "designer") %>%
  filter(productivity <= 5) %>%
  arrange(productivity) %>%
  mutate(factor_label = factor(designer_games, levels = unique(.$designer_games)),
    game_info = str_glue("{year} | {primary} ({format(round(average, 1), nsmall = 1)})"),
    rating = case_when(
      average == hi ~ "hi",
      average == lo ~ "lo",
      TRUE ~ "mid"
    ))


font_add_google("Righteous", "righteous")
font_add_google("Zilla Slab", "zilla")
showtext_auto()


df %>% ggplot(aes(x = year, y = average)) +
  lemon::facet_rep_wrap(
    vars(factor_label),
    ncol = 1, strip.position = "right") +
  geom_point(
    data = df[which(df$rating == "mid"),],
    size = 1.5, color = "#004643", alpha = .6) +
  geom_point(
    data = df[which(df$rating != "mid"),],
    aes(color = rating),
    size = 1.8) +
  geom_text(
    data = df[which(df$rating != "mid" & df$primary != "Chicken of the Sea"),],
    aes(color = rating, label = game_info),
    hjust = 1, nudge_x = -1,
    family = "zilla", size = 10
  ) +
  geom_text(
    data = df[which(df$primary == "Chicken of the Sea"),],
    aes(color = rating, label = game_info),
    hjust = 0, nudge_x = 1,
    family = "zilla", size = 10
  ) +
  ylab("Average rating") +
  scale_color_manual(values = c("#d1ac00", "#FAF4D3")) +
  scale_y_continuous(limits = c(4, 9)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#0c1618"),
    legend.position = "none",
    strip.text = element_text(
      color = "#D7E7EA",
      face = "bold",
      family = "righteous",
      size = 30,
      lineheight = .3,
      margin = margin(0, 1, 0, 1)),
    axis.line.x = element_line(color = "#2A4A50"),
    axis.ticks.x = element_line(color = "#2A4A50"),
    axis.ticks.length=unit(.1, "cm"),
    axis.text.x = element_text(
      color = "#D7E7EA",
      size = 30,
      family = "righteous",
      margin = margin(t = 5)),
    axis.title.y = element_text (
      color = "#D7E7EA",
      size = 30,
      angle = 90,
      family = "righteous",
      margin = margin(l = 10, r = 10))
  ) +
  plot_annotation(
    title = toupper("Game masters"),
    subtitle = glue::glue("
      Career
       <b style='color:#d1ac00;'>HITS</b>
       and
       <b style='color:#FAF4D3;'>MISSES</b>
      of the five most prolific game designers
      "),
    caption = "Source: BoardGameGeek, Kaggle  |  Viz: @so_xinteresting  |  #TidyTuesday",
    theme = theme(
      plot.title = element_text(
        family = "righteous",
        face = "bold",
        size = 60,
        color = "#D7E7EA",
        hjust = .5,
        margin = margin(t = 15)),
      plot.caption = element_text(
        family = "zilla",
        size = 30,
        color = "#2A4A50",
        hjust = .5,
        margin = margin(t = 15, b = 5)),
      plot.subtitle = ggtext::element_markdown(
        family = "zilla",
        size = 35,
        color = "#D7E7EA",
        hjust = .5,
        margin = margin(t = 10, b = 5)),
      plot.background = element_rect(fill = "#0c1618", color = NA)))


ggsave("2022-week04-boardgames.png", width = 5, height = 7, unit = "in")


