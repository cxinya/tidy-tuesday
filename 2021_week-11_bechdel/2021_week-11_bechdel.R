library(tidyverse)
library(waffle)
library(showtext)
library(patchwork)

font_add_google("Roboto", "robo")
font_add_google("Roboto Slab", "robo-slab")
showtext_auto()

# Data --------------------------------------------------------------------

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
  mutate(genre = toupper(ifelse(genre == "Adventure", "Advent-\nure", genre)))

top10_genres <- genre_bechdel %>% 
  summarize(tot_films = sum(n)) %>% 
  slice_max(tot_films, n = 10) %>% 
  arrange(tot_films)

genre_bechdel <- genre_bechdel %>% 
  filter(genre %in% top10_genres$genre) %>% 
  mutate(
    rating = as.character(rating),
    genre = factor(genre, levels = top10_genres$genre)
    )

# Viz ---------------------------------------------------------------------

pal <- c("#e76f51","#f4a261","#2a9d8f","#264653")


genre_bechdel %>% ggplot() +
  facet_wrap(~genre, nrow = 1, strip.position = "bottom") +
  geom_waffle(
    aes(values = n/10, fill = rating),
    n_rows = 4, size = .45, colour = "#dee3e3", flip = T, show.legend = F) +
  coord_equal() +
  scale_fill_manual(values = pal) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_void() +
  theme(
    text = element_text(family = "robo"),
    plot.margin = margin(b = 5, r = 25, l = 25),
    plot.background = element_rect(fill = "#dee3e3", color = NA),
    strip.text = element_text(face = "bold", size = 30, margin = margin(t = 7), lineheight = .3, vjust = 1),
    panel.spacing = unit(.6, "cm"),
  ) +
  plot_annotation(
    title = toupper("Bechdel test scores among\ntop 10 film genres"),
    subtitle = glue::glue(
       "Films pass the Bechdel Test if they clear the nearly subterraneanly low bar of:
       <br><b style='color:#f4a261;'>Including at least two named women...</b>
       <br><b style='color:#2a9d8f;'>...who have a conversation...</b>
       <br><b style='color:#264653;'>...that's not about a male character.</b>
       <br>Or, of course, films may just <b style = 'color:#e76f51'>completely fail</b>."
    ),
    caption = "Note: Each square represents 10 films  |  Source: FiveThirtyEight, Bechdeltest.com, & imdb.com  |  Viz: Xin Yuen @so_xinteresting",
    theme = theme(
      plot.title = element_text(face = "bold", size = 80, margin = margin(t = 25), lineheight = .3),
      plot.caption = element_text(family = "robo-slab", size = 20, color = "gray50", hjust = .5),
      plot.subtitle = ggtext::element_markdown(family = "robo-slab", size = 27, lineheight = .5, margin = margin(t = 10, b = -85)),
      plot.background = element_rect(fill = "#dee3e3", color = NA)))


ggsave("2021_week-11_bechdel.png", width = 10, height = 5, units = "in")
