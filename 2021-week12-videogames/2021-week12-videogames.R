library(tidyverse)

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
  mutate(month_year = lubridate::my(month_year)) %>% 
  group_by(gamename) %>% 
  arrange(month_year) %>% 
  mutate(months_out = row_number())
