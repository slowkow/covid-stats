#!/usr/bin/env Rscript

pacman::p_load(
  ggplot2,
  readr,
  janitor,
  dplyr,
  stringr,
  tibble,
  zoo,
  scales,
  patchwork,
  tidyr,
  ggtext,
  glue,
  pals
)
#
source("theme-kamil.R")
theme_set(theme_kamil)

# Cases {{{

# Each column represents one day
# Each row represents one county in each of the 50 states in USA
d <- read_csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
d <- clean_names(d)
m <- d %>% select(starts_with("x"))

# Each row represents one day
d <- tibble(
  date = as.Date(colnames(m), format = "x%Y_%m_%d"),
  cases = unname(colSums(m))
)
d$newcases <- d$cases - c(0, head(d$cases, length(d$cases) - 1))
d$mean_newcases <- rollmean(x = d$newcases, k = 7, fill = rep("extend", 3))
d$year <- str_split_fixed(d$date, "-", 2)[,1]
d$day  <- str_split_fixed(d$date, "-", 2)[,2]
d$date2 <- as.Date(sprintf("2020-%s", d$day), format = "%Y-%m-%d")

# }}}

# Deaths {{{

deaths <- read_csv("https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv")
deaths <- clean_names(deaths)
deaths <- deaths %>% select(starts_with("x"))

# Each row represents one day
d2 <- tibble(
  date = as.Date(colnames(deaths), format = "x%Y_%m_%d"),
  cases = unname(colSums(deaths))
)
d2$newcases <- d2$cases - c(0, head(d2$cases, length(d2$cases) - 1))
d2$mean_newcases <- rollmean(x = d2$newcases, k = 7, fill = rep("extend", 3))
d2$year <- str_split_fixed(d2$date, "-", 2)[,1]
d2$day  <- str_split_fixed(d2$date, "-", 2)[,2]
d2$date2 <- as.Date(sprintf("2020-%s", d2$day), format = "%Y-%m-%d")

# }}}

p1 <- ggplot(d) +
  aes(x = date2, y = mean_newcases, group = year, color = year) +
  geom_line(linewidth = 2) +
  scale_x_date(date_labels="%b",date_breaks = "1 month") +
  scale_y_continuous(trans = "log10", labels = label_number_si()) +
  scale_color_manual(values = pals::okabe()) +
  annotation_logticks(sides = "l") +
  guides(color = guide_legend(title = NULL)) +
  theme(
    panel.grid.major = element_line(),
    legend.position = "top",
    legend.box.spacing = unit(0, "lines"),
    plot.title = element_markdown(),
    axis.text.x = element_blank()
  ) +
  labs(
    title = "<b>New COVID-19 cases per day in USA</b>",
    y = "Cases",
    x = NULL
  )
p2 <- ggplot(d2) +
  aes(x = date2, y = mean_newcases, group = year, color = year) +
  geom_line(linewidth = 2) +
  scale_x_date(date_labels="%b",date_breaks = "1 month") +
  scale_y_continuous(trans = "log10", labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = pals::okabe()) +
  annotation_logticks(sides = "l") +
  guides(color = guide_legend(title = NULL)) +
  theme(
    panel.grid.major = element_line(),
    legend.position = "none",
    legend.box.spacing = unit(0, "lines"),
    plot.title = element_markdown()
  ) +
  labs(
    title = "<b>New COVID-19 deaths per day in USA</b>",
    caption = "Rolling 7 day mean. Data from usafacts.org",
    y = "Deaths",
    x = NULL
  )
p <- p1 / p2
ggsave("usafacts-covid.pdf", p, width = 10, height = 8)
ggsave("usafacts-covid.png", p, width = 10, height = 8)

