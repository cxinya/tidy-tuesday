library(tidyverse)

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


genre_bechdel <- movies %>% 
  select(imdb_id, genre) %>%
  drop_na(genre) %>% 
  mutate(imdb_id = str_remove(imdb_id, "tt")) %>% 
  separate(
    genre, 
    into = c("genre1", "genre2", "genre3"),
    sep = ", ") %>% 
  pivot_longer(
    cols = -imdb_id,
    names_to = "genre_num",
    names_prefix = "genre",
    values_to = "genre"
  ) %>% 
  left_join(select(raw_bechdel, imdb_id, rating)) %>% 
  drop_na(genre, rating) %>% 
  group_by(genre) %>% 
  count(rating) %>% 
  mutate(per = n / sum(n))

top10_genres <- genre_bechdel %>% 
  summarize(tot_films = sum(n)) %>% 
  slice_max(tot_films, n = 10)

genre_bechdel <- genre_bechdel %>% 
  filter(genre %in% top10_genres$genre)