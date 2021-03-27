library(tidyverse)
library(patchwork)
library(showtext)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


font_add_google("Poppins", "pop")
showtext_auto()

pal <- c("#5b92e5", "#B5BFC8", "#E6DC5C")


# Votes per resolution

vote_res <- unvotes %>%
  group_by(rcid) %>%
  count(vote) %>%
  group_by(rcid) %>%
  mutate(per = n / sum(n) * 100) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from = vote,
    values_from = per
  ) %>%
  replace(is.na(.), 0) %>%
  left_join(select(roll_calls, rcid, importantvote)) %>%
  filter(!is.na(importantvote)) %>%
  # Vote results - simple majority for regular resolutions, 2/3 for important issues to pass
  mutate(
    result = case_when(
      importantvote == 0 & (no == 50 | yes == 50)       ~ "No decision",
      importantvote == 1 & (no <= 200/3 & yes <= 200/3) ~ "No decision",
      abstain < 50 & no < 50 & yes < 50 ~ "No decision",
      abstain >= 50                     ~ "No decision",
      importantvote == 0 & no > 50      ~ "Fail",
      importantvote == 1 & no > 200/3   ~ "Fail",
      importantvote == 0 & yes > 50     ~ "Pass",
      importantvote == 1 & yes > 200/3  ~ "Pass"))


# Add US votes and see whether they match

us_match <- vote_res %>%
  left_join(filter(unvotes, country_code == "US")) %>%
  mutate(
    us_vs_world = factor(case_when(
      vote == "abstain" ~ "Abstained",
      vote == "yes" & result == "Pass" ~ "Match",
      vote == "no"  & result == "Fail" ~ "Match",
      vote == "no"  & result == "Pass" ~ "Mismatch",
      vote == "yes" & result == "Fail" ~ "Mismatch"
  ), levels = c("Match", "Abstained", "Mismatch"))) %>%
  left_join(select(issues, -short_name)) %>%
  left_join(select(roll_calls, rcid, date)) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(!is.na(country_code) & result != "No decision") %>%
  arrange(date) %>%
  mutate(order = row_number()) # Orig RCIDs not in chronological order


# Match/mismatch by year

us_match_year <- us_match %>%
  group_by(year) %>%
  count(us_vs_world) %>%
  group_by(year) %>%
  mutate(per = n / sum(n) * 100)

us_match_year_plot <- us_match_year %>%
  ggplot(aes(x = year, y = per, fill = us_vs_world, color = us_vs_world)) +
  geom_bar(position = "stack", stat = "identity", width = 1) +
  geom_text(
    aes(x = 1964, y = 0, label = "N o   v o t e s   i n   1 9 6 4"),
    family = "pop", size = 8, fontface = "bold", color = "#0C274F", angle = 90, hjust = 0) +
  geom_text(
    aes(
      x = ifelse(year %% 10 == 0, year, NA),
      y = 0,
      label = ifelse(year %% 10 == 0, year, "")),
    family = "pop", size = 8, fontface = "bold", color = "#0C274F", angle = 90, hjust = 0) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_void() +
  theme(
    text = element_text(family = "pop"),
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0, "lines"),
    plot.background = element_rect(fill = "white", color = "NA"),
    plot.margin = margin(b = 7))


# Mass/mismatch by topic

us_match_topic_plot <- us_match %>%
  filter(!is.na(issue)) %>%
  ggplot() +
  facet_wrap(vars(issue), ncol = 1) +
  geom_segment(
    aes(x = order, xend = order, y = 0, yend = 1, color = us_vs_world)
  ) +
  geom_text(
    aes(x = 0, y = .02, label = toupper(issue)),
    vjust = 0, hjust = 0, fontface = "bold", size = 18, color = "#0C274F", family = "pop") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(family = "pop"),
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0, "lines"),
    plot.background = element_rect(fill = "white", color = "NA")
  )


# Combine plots

us_match_year_plot / us_match_topic_plot +
  plot_layout(heights = unit(c(1.25, 3), "in")) +
  plot_annotation(
    title = toupper("U n i t e d  S t a t e s\nv s .\nt h e  W i l l   o f  t h e   W o r l d"),
    subtitle = str_wrap(glue::glue("
      Each United Nations vote that resulted in a decision was contrasted against the United State's vote.<br>
      The United States could either
       <b style='color:#5b92e5;'>AGREE with</b>,
       <b style='color:#B5BFC8;'>DISAGREE with</b>, or
       <b style='color:#E6DC5C;'>ABSTAIN from commenting on </b><br>
      the will of the United Nations. Results are presented by vote year and, when available, resolution topic.<br>
      <b>The United States has stood in opposition to the United Nations more and more frequently</b>, particularly regarding the Palestinian conflict.
      "), 100),
    caption = "Source: Harvard Dataverse  |  Viz: Xin Yuen, @so_xinteresting  |  #Tidy Tuesday",
    theme = theme(
      text = element_text(family = "pop"),
      plot.title = element_text(face = "bold", size = 80, margin = margin(t = 15), lineheight = .3, hjust = .5),
      plot.subtitle = ggtext::element_markdown(size = 30, lineheight = .5, margin = margin(t = 10, b = 15)),
      plot.caption = element_text(size = 20, color = "gray50", hjust = .5, margin = margin(t = 7, b = 7)),
      plot.background = element_rect(fill = "white", color = NA)))


ggsave("test.png", width = 8, height = 7)
