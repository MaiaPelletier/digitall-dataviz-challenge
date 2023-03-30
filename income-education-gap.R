# load libraries
library(tidyverse)
library(janitor)
library(ggtext)
library(glue)
library(extrafont)
library(scales)


# load data ---------------------------------------------------------------


# renaming function
rename_tables <- function(data) {
  data %>% 
    clean_names() %>% 
    rename_all(~ str_remove(.x, "_[0-9]+\\w*"))
}


# load income data
# downloaded from Table 98-10-0334-01 (source: 2021 Census)
income <- 
  read_csv("income.csv") %>% 
  rename_tables() %>%
  select(
    geo,
    vm = visible_minority,
    gender,
    age,
    income = value
  )


# load education data
# downloaded from Table 98-10-0429-01 (source: 2021 Census)
education <- 
  read_csv("education.csv")  %>%
  rename_tables() %>% 
  select(
    geo,
    vm = visible_minority,
    gender,
    age,
    education = value
  )


# load population counts
# downloaded from Table 98-10-0326-01 (source: 2021 Census)
population <- 
  read_csv("population.csv") %>% 
  rename_tables() %>% 
  select(
    geo,
    vm = visible_minority,
    gender, age,
    population = value
  )


# read csv with list of province abbreviations
abbreviations <- read_csv("geo_abbreviations.csv")


# data manipulation -------------------------------------------------------


# join data and create intersection categories
data <- 
  left_join(income, education) %>% 
  left_join(population) %>% 
  left_join(abbreviations) %>% 
  filter_at(2:3, ~ !str_detect(.x, "Total - ")) %>%   # filter out totals
  mutate(
    quad = case_when(
      vm == "Total visible minority population" & gender == "Women+" ~ "VM Women",
      vm == "Total visible minority population" & gender == "Men+" ~ "VM Men",
      vm == "Not a visible minority" & gender == "Women+" ~ "non-VM Women",
      vm == "Not a visible minority" & gender == "Men+" ~ "non-VM Men"
    )
  ) %>% 
  filter( 
    geo != "Canada",
    !code %in% c("YT", "NT", "NU")
  )


# calculate each demograhic % of total population
pct_pop <-
  data %>%
  left_join(
    population %>%
      group_by(geo) %>%
      summarise(total = sum(population), .groups = "drop")
  ) %>%
  mutate(pct_pop = (population/total)*100000)


# join population percents
data <-
  data %>%
  left_join(pct_pop) %>% 
  mutate(quad = factor(quad, levels = c("VM Women", 
                                        "non-VM Women", 
                                        "VM Men", 
                                        "non-VM Men")))


# plot set-up -------------------------------------------------------------


# line segments for grid squares
line_segments <- data.frame(
  xstart = c(rep(0, 3), seq(0, 100000, length.out = 3)),
  xend = c(rep(100000, 3), seq(0, 100000, length.out = 3)),
  ystart = c(seq(0, 100, 50), rep(0, 3)),
  yend = c(seq(0, 100, 50), rep(100, 3))
)


# subtitle with HTML formatting
subtitle <- "In 2021 <b>working-aged women</b> who were <b>visible minorities (VM)</b> had the <b>highest rates of post-secondary<br>education</b> in almost all Canadian provinces<sup>1</sup> and earned the <b>lowest annual salary</b> on average overall."


# create footnotes with HTML formatting
ftn_1 <- "<b> Working-aged</b> - Population aged 25 to 64"
ftn_2 <- "<b> Women</b> - Includes women (and/or girls), as well as some non-binary persons."
ftn_3 <- "<b> Visible minority (VM)</b> - A person who is a visible minority as defined by the <i>Employment Equity Act</i>. Consists mainly of the following groups: South Asian, Chinese, Black,  Filipino,<br>Arab, Latin American, Southeast Asian, West Asian, Korean and Japanese. Excludes Indigenous persons."
ftn_4 <- "<b> Postsecondary education</b> - Postsecondary certificate, diploma or degree"
ftn_5 <- " Territories are excluded          from analysis."
source <- "<b>Source: </b> Statistics Canada, Census of Population, 2021. | <b>Author: </b> Maia Pelletier"


# combine to create caption
caption <- glue("<b>Definitions:</b><br>{ftn_1}<br>{ftn_2}<br>{ftn_3}<br>{ftn_4}<br><br><sup>1</sup>{ftn_5}<br><br><br>{source}")


# create palette for plot
pal <- c(
  "#6daabd", # blue
  "#bae79d", # green
  "#f3cb04", # yellow
  "#e28027", # red
  "#FBF7EB"  # off-white
)


# load installed fonts
extrafont::loadfonts()


# ggplotting --------------------------------------------------------------


data %>%
  ggplot() +
  geom_segment(
    data = line_segments,
    aes(x = xstart,
        y = ystart,
        xend = xend,
        yend = yend),
    color = "#E7D6C5"
  ) +
  geom_rect(
    aes(
      xmin = 0,
      ymin = 0,
      xmax = income,
      ymax = education,
      color = quad,
      fill = quad
    ),
    alpha = 0.5
  )+
  geom_col(
    data = 
      data %>% 
      mutate(quad = reorder(quad, pct_pop)),
    aes(x = pct_pop, y = 98, fill = quad),
    orientation = "y",
    position = "stack",
    width = 4,
    alpha = 0.5
  ) +
  labs(
    x = "Average annual salary",
    y = "% with postsecondary achievement",
    title = str_to_upper("Over-educated & under-paid"),
    subtitle = subtitle,
    caption = caption
  ) +
  facet_wrap(~code, nrow = 2) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = c(0, 50, 100),
    labels = percent_format(scale = 1),
    position = "left"
  ) +
  scale_x_continuous(
    limits = c(0, 110000),
    breaks = c(0, 50000, 100000),
    labels = dollar_format(suffix = "k", scale = 0.001)
  ) +
  scale_fill_manual(
    values = pal[c(4, 2, 3, 1)],
    name = NULL
  ) +
  scale_color_manual(
    values = pal[c(4, 2, 3, 1)],
    name = NULL
  ) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    text = element_text(color = "#4D4519", 
                        family = "Lato"),
    plot.margin = margin(t = 20, b = 20, l = 20, r = 28),
    plot.background = element_rect(fill = pal[5], 
                                   color = NA),
    panel.spacing = unit(10, "mm"),
    plot.title = element_text(face = "bold", 
                              size = 32, 
                              hjust = 0.5, 
                              margin = margin(b = 5), 
                              family = "Comfortaa"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12, 
                                     hjust = 0.5),
    plot.caption = element_markdown(margin = margin(t = 20, b = -5, l = -50), 
                                    size = 8, 
                                    hjust = 0, 
                                    lineheight = 1.4),
    strip.text = element_text(face = "bold", 
                              size = 10, 
                              family = "Comfortaa"),
    axis.text = element_text(size = 7, 
                             margin = margin(l = 2, t = 2)),
    axis.title.x = element_text(margin = margin(t = 10), 
                                size = 12),
    axis.title.y = element_text(angle = 90, 
                                margin = margin(r = 15), 
                                size = 12),
    legend.position = "top",
    legend.margin = margin(t = 20, b = 20),
    legend.text = element_text(margin = margin(r = 50), size = 10)
  )


ggsave("income-education-gap.png", dpi = 300)

