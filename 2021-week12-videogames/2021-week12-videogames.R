library(tidyverse)
library(lubridate)

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
  lemon::facet_rep_wrap(vars(gamename)) +
  geom_ribbon(
    aes(ymin = avg, ymax = peak, fill = gamename, color = gamename),
    alpha = .2) +
  ggimage::geom_image(data = tractors,
    aes(image = img, x = 13, y = img_y),
    size = .2, asp = 1) +
  labs(
    x = "Months since release",
    y = "Simultaneous players"
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 15), breaks = seq(0, 12)) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.line.x = element_line(color = "brown", size = 2)
  )

ggsave("2021-week12-videogames.png", width = 10, height = 6, unit = "in")
