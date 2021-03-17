library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(ggimage)

font_add_google("Roboto", "robo")
font_add_google("Press Start 2P", "2p")
showtext_auto()

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


# Farm feuds ---------------------------------------------------------------

farm <- games %>% filter(str_detect(gamename, "Farm")) %>% 
  unite("month_year", month:year, sep = " ", remove = F) %>%
  mutate(month_year = my(month_year)) %>% 
  group_by(gamename) %>% 
  arrange(month_year) %>% 
  mutate(
    months_out = row_number() - 1,
    release = min(month_year),
    release_label = str_glue("Released {format(release, '%b %Y')}")) %>% 
  filter(months_out <= 12) %>% 
  arrange(release) %>% 
  mutate(
    gamename = factor(gamename, levels = unique(.$gamename)),
    end = max(month_year)) %>% 
  ungroup()


# Images --------------------------------------------------------------

tractors <- farm %>% distinct(gamename, month_year = end) %>% 
  left_join(select(farm, gamename, month_year, avg, peak)) %>%
  mutate(
    img_y = avg + (peak - avg)/2,
    img = str_glue("pics/tractor-{row_number()}.png"),
    px_img = str_glue("pics/pixel-tractor-{row_number()}.png"))


clouds <- farm %>% distinct(gamename) %>% 
  group_by(gamename) %>% 
  mutate(
    cloud1 = str_glue("pics/cloud-{sample(1:4, 1)}.png"),
    cloud2 = str_glue("pics/cloud-{sample(1:4, 1)}.png"), 
    cloud1_x = runif(1, 0, 12),
    cloud1_y = runif(1, 3.5, 7) * 10000, 
    cloud2_x = runif(1, 0, 12),
    cloud2_y = runif(1, 3.5, 7) * 10000)
  


# Plot --------------

# Helpful ggimage aspect ratio walkthrough: https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/

asp_ratio <- 1.618*4.5

farm %>% filter(months_out <= 12) %>% 
  ggplot(aes(x = months_out)) +
  lemon::facet_rep_wrap(vars(gamename), strip.position = "bottom", ncol = 1) +
  geom_area(
    aes(y = peak),
    color = "slategray3", fill = "slategray", size = 1, alpha = .6) +
  geom_image(data = clouds,
    aes(image = cloud1, x = cloud1_x, y = cloud1_y),
    size = 0.2, by = "width", asp = asp_ratio) +
  geom_image(data = clouds,
    aes(image = cloud2, x = cloud2_x, y = cloud2_y),
    size = 0.2, by = "width", asp = asp_ratio) +
  geom_image(data = tractors,
    aes(image = px_img, x = 12, y = 27000),
    size = 0.2, by = "width", asp = asp_ratio) +
  geom_text(
    aes(label = release_label, x = 0, y = 6500),
    hjust = 0, size = 10, family = "2p", color = "forestgreen") +
  scale_size_identity() +
  labs(
    x = "Months since release",
    y = "Peak simultaneous players"
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 13.5), breaks = seq(0, 12, 1)) +
  scale_y_continuous(limits = c(0, 70000)) +
  theme_void() +
  theme(
    aspect.ratio = 1/asp_ratio,
    text = element_text(family = "robo"),
    legend.position = "top",
    axis.title = element_text(size = 45, face = "bold"),
    axis.title.y = element_text(angle = 90, margin = margin(r = 15, l = -3)),
    axis.title.x = element_text(margin = margin(t = -5)),
    axis.line.x = element_line(color = "darkgoldenrod4", size = 2, lineend = "square"),
    axis.ticks.x = element_line(color = "chartreuse3", size = 1.5),
    plot.margin = margin(l = 10, r = 10),
    strip.text = element_text(family = "2p", face = "bold", size = 35, margin = margin(t = 10, b = 15)),
    strip.placement = "outside",
    plot.background = element_rect(fill = "lightblue", color = NA)
  ) +
  plot_annotation(
    title = toupper("Farm feuds"),
    subtitle = "Peak number of farm enthusiasts\nsimultaneously playing over the\nfirst year of each game's release",
    caption = "Source: SteamCharts  |  Viz: Xin Yuen, @so_xinteresting  |  #TidyTuesday",
    theme = theme(
      plot.title = element_text(family = "2p", face = "bold", size = 80, margin = margin(t = 15), hjust = .5),
      plot.caption = element_text(family = "robo", size = 30, color = "gray50", hjust = .5, margin = margin(t = 10, b = 5)),
      plot.subtitle = element_text(family = "robo", size = 40, hjust = .5, lineheight = .3, margin = margin(t = 15, b = 25)),
      plot.background = element_rect(fill = "lightblue", color = NA)))


ggsave("2021-week12-videogames.png", width = 5, height = 9, unit = "in")
