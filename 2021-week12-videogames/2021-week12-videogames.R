library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(ragg)

font_add_google("Roboto", "robo")
font_add_google("Roboto Slab", "robo-slab")
showtext_auto()

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')



# Clean punctuation from non-English names

games <- games %>% mutate(gamename = str_trim(str_remove(gamename, "^<.*>")))


# Check any remaining non-English names with characters in title and fix
games %>% 
  distinct(gamename) %>% 
  filter(str_detect(gamename, "^[[:punct:]]"))

games <- games %>% mutate(
  gamename = recode(gamename,
    "(PathOfWuxia)"       = "Path of Wuxia",
    "(MahjongSoul)"       = "Mahjong Soul",
    "(Gujian3)"           = "Gujian 3",
    "-1v5(Notes of Soul)" = "Notes of Soul"
  ))


# Farm wars ---------------------------------------------------------------

farm <- games %>% filter(str_detect(gamename, "Farm")) %>% 
  unite("month_year", month:year, sep = " ", remove = F) %>%
  mutate(month_year = my(month_year)) %>% 
  group_by(gamename) %>% 
  arrange(month_year) %>% 
  mutate(
    months_out = row_number() - 1,
    # game_lab = ifelse(months_out == 0, str_glue("{gamename}\nReleased {month} {year}"), NA),
    release = min(month_year),
    release_label = format(month_year, "%B %Y")) %>% 
  filter(months_out <= 12) %>% 
  # fill(game_lab) %>% 
  arrange(release) %>% 
  # mutate(game_lab = factor(game_lab, levels = unique(.$game_lab)))
  mutate(
    gamename = factor(gamename, levels = unique(.$gamename)),
    end = max(month_year)) %>% 
  ungroup()


# Tractor emoji --------------------------------------------------------------

tractors <- farm %>% distinct(gamename, month_year = end) %>% 
  left_join(select(farm, gamename, month_year, avg, peak)) %>%
  mutate(
    img_y = avg + (peak - avg)/2,
    img = str_glue("pics/tractor-{row_number()}.png"))
  


# Plot --------------------------------------------------------------------

farm %>% filter(months_out <= 12) %>% 
  ggplot(aes(x = months_out)) +
  lemon::facet_rep_wrap(vars(gamename), strip.position = "bottom") +
  geom_area(aes(y = peak), alpha = .5) +
  geom_line(aes(y = avg)) +
  ggimage::geom_image(data = tractors,
    aes(image = img, x = 11, y = ifelse(peak > 10000, peak + 65, 6500)),
    size = .3, by = "width") +
  scale_size_identity() +
  labs(
    x = "Months since release",
    y = "Simultaneous players"
  ) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 13), breaks = seq(0, 13, 1)) +
  theme_void() +
  theme(
    text = element_text(family = "robo"),
    legend.position = "none",
    axis.line.x = element_line(color = "brown", size = 1.5),
    axis.ticks.x = element_line(color = "green", size = 1.5),
    plot.margin = margin(t = 20, b = 10, r = 10, l = 10),
    strip.text = element_text(face = "bold", size = 40, margin = margin(t = 10, b = 10)),
    strip.placement = "outside",
    panel.spacing = unit(.1, "cm")
  ) +
  plot_annotation(
    title = toupper("Fantasy farm feuds"),
    subtitle = glue::glue(
       "Here's some text about farms"
    ),
    caption = "Source: SteamCharts  |  Viz: Xin Yuen, @so_xinteresting  |  #TidyTuesday",
    theme = theme(
      plot.title = element_text(face = "bold", size = 80, margin = margin(t = 25), lineheight = .3),
      plot.caption = element_text(family = "robo-slab", size = 30, color = "gray50", hjust = .5),
      # plot.subtitle = ggtext::element_markdown(family = "robo-slab", size = 30, lineheight = .5, margin = margin(t = 10, b = -85)),
      plot.background = element_rect(fill = "#dee3e3", color = NA)))


ggsave("2021-week12-videogames.png", width = 9, height = 6, unit = "in")
