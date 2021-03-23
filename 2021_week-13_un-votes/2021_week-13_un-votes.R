library(tidyverse)
library(ggstream)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# Brainstorm
# % role calls about issues over time -- stream on top and # votes in panel below?
# Close votes
# US vs the world


pal <- c("#042A2B", "#5EB1BF", "#83A8A6", "#CDEDF6", "#EF7B45", "#772E25")
pal2 <- c("#022B3A", "1F7A8C", "#6FABC2", "#BFDBF7", "#E1E5F2", "#B5BFC8")


issues_yr <- issues %>%
  left_join(select(roll_calls, rcid, date)) %>%
  separate(date, into = c("year", "month", "day")) %>%
  group_by(year) %>%
  count(issue) %>%
  mutate(
    year = as.numeric(year),
    per = n / sum(n) * 100)

test <- issues %>%
  left_join(select(roll_calls, rcid, date)) %>%
  separate(date, into = c("year", "month", "day"))


issues_yr %>%
  # filter(year < 1950) %>%
  ggplot(aes(x = year, y = n, fill = issue)) +
  geom_stream(bw = 1, extra_span = .2, true_range = "none") +
  scale_fill_manual(values = pal) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

issues_yr %>% ggplot(aes(x = year, y = n, color = issue, group = issue)) +
  geom_line()



ggsave("2021_week-13_un-votes_outtake2.png", width = 8, height = 5)
