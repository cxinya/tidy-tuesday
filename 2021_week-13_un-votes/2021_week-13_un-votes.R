library(tidyverse)
library(ggstream)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# Brainstorm
# % role calls about issues over time -- stream on top and # votes in panel below?
# Close votes
# US vs the world


pal <- c("#5b92e5", "#B5BFC8", "#E6DC5C")




# US vs the world ---------------------------------------------------------

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

# Add US votes and see how they match
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
  filter(!is.na(country_code) & result != "No decision")


# Match/mismatch by year --------------------------------------------------

us_match_year <- us_match %>%
  group_by(year) %>%
  count(us_vs_world) %>%
  group_by(year) %>%
  mutate(per = n / sum(n) * 100)

us_match_year %>%
  ggplot(aes(x = year, y = per, fill = us_vs_world, color = us_vs_world)) +
  geom_bar(position = "stack", stat = "identity") +
  # geom_stream(bw = .8, alpha = .9, size = 1) +
  # geom_stream_label(
  #   aes(label = us_vs_world),
  #   lineheight = .5, fontface = "bold") +
  scale_fill_manual(values = pal3) +
  scale_color_manual(values = pal3) +
  scale_x_continuous(breaks = seq(1945, 2020, 5), labels = seq(1945, 2020, 5)) +
  theme_minimal()


# Mass/mismatch by topic --------------------------------------------------

us_match_topic <- us_match %>%
  filter(!is.na(issue) & rcid < 6000) %>%
  ggplot() +
  facet_wrap(vars(issue), ncol = 1) +
  geom_segment(
    aes(x = rcid, xend = rcid, y = 0, yend = 1, color = us_vs_world),
    size = 1
  ) +
  geom_text(
    aes(x = 0, y = .05, label = toupper(issue)),
    vjust = 0, hjust = 0, fontface = "bold", size = 10, color = "#0C274F") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = rev(pal3)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0, "lines")
  )
