# TODO: Figure out way to show population counts for each population
# TODO: Add Indigenous dimension


# load libraries
library(tidyverse)
library(janitor)
library(geofacet)
library(showtext)
library(cowplot)
library(ggtext)


# data manipulation -------------------------------------------------------

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

abbreviations <- read_csv("geo_abbreviations.csv")

# join data and create coordinates for plot
data <- 
  left_join(income, education) %>% 
  filter_at(2:3, ~ !str_detect(.x, "Total - ")) %>% 
  mutate(
    quad = case_when(
      vm == "Total visible minority population" & gender == "Women+" ~ "VM Women+",
      vm == "Total visible minority population" & gender == "Men+" ~ "VM Men+",
      vm == "Not a visible minority" & gender == "Women+" ~ "non-VM Women+",
      vm == "Not a visible minority" & gender == "Men+" ~ "non-VM Men+"
    )
  ) %>% 
  # mutate(
  #   x = ifelse(gender == "Women+", income, -income),
  #   y = ifelse(visible_minority == "Not a visible minority", -education, education),
  #   quad = fct_inorder(
  #     case_when(
  #       x > 0 & y > 0 ~ "VM Women+",
  #       x < 0 & y > 0 ~ "VM Men+",
  #       x < 0 & y < 0 ~ "non-VM Men+",
  #       x > 0 & y < 0 ~ "non-VM Women+"
  #     )
  #   )
  # ) %>% 
  left_join(abbreviations)

national_data <- data  %>% 
  filter(geo == "Canada") %>% 
  select(quad, income, education)

# plot set up -------------------------------------------------------------

# grid for facets
mygrid <- data.frame(
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  col = c(1, 2, 3, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
  code = c("YT", "NT", "NU", "NL", "PE", "BC", "AB", "SK", "MB", 
           "ON", "QC", "NB", "NS"),
  name = c("Yukon", "Northwest Territories", "Nunavut", 
           "Newfoundland and Labrador", "Prince Edward Island", 
           "British Columbia", "Alberta", "Saskatchewan", "Manitoba", 
           "Ontario", "Quebec", "New Brunswick", "Nova Scotia"),
  stringsAsFactors = FALSE
)

line_segments <- data.frame(
  xstart = c(rep(0, 3), seq(0, 100000, length.out = 3)),
  xend = c(rep(100000, 3), seq(0, 100000, length.out = 3)),
  ystart = c(seq(0, 100, 50), rep(0, 3)),
  yend = c(seq(0, 100, 50), rep(100, 3))
)

# colour palette for viz
# pal <- paste0("#", c("F7BEB6","f4724e","DAC1A9","4c3346","3d8a4f","F7C164","95d3d3"))

pal <- paste0("#", c(
  "e07a5f", # red
  "8589ad", # navy
  "f5f2e0", # beige
  "81b29a", # teal
  "f2cc8f"  # yellow
  ))

# add font to be used
font_add_google("Comfortaa", "oxygen", bold.wt = 700)

definitions <- tribble(
  ~footnote_no,   ~footnote_text,
  "1",   "<b>Working-aged</b>: Population aged 25 to 64",
  "2",   "<b>Women</b>: Women+ includes women (and/or girls), as well as some non-binary persons.",
  "3",   "<b>Visible minority</b>: A person who is a visible minority as defined by the <i>Employment Equity Act</i>. The visible minority population consists mainly of the following groups: South Asian, Chinese, Black, Filipino, Arab, Latin American, Southeast Asian, West Asian, Korean and Japanese."
)

# plotting ----------------------------------------------------------------

showtext_auto()
# showtext_opts(dpi = 300)

p <- 
  data  %>%
  filter(geo != "Canada") %>%
  filter(!code %in% c("YT", "NT", "NU")) %>%
    # filter(quad == "VM Men+") %>% 
  mutate(
    quad = factor(
      quad,
      levels = c(
        "VM Women+", 
        "non-VM Women+", 
        "VM Men+", 
        "non-VM Men+"
      )
    )
  ) %>%
  ggplot() +
  geom_segment(
    data = line_segments,
    aes(x = xstart, y = ystart, xend = xend, yend = yend),
    color = "#E7D6C5"
  ) +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = income, ymax = education, fill = quad),
    color = NA, alpha = 0.45
  ) +
  labs(
    x = "Average annual salary",
    y = "% with postsecondary certificate, diploma or degree",
    title = str_to_upper("Over-educated & under-paid"),
    subtitle = "In 2021, working-aged women who were visible minorities were the most educated in almost all provinces and\nearned the lowest annual salary on average overall."
    # caption = "Author: Maia Pelletier | Source: Statistics Canada. Census of Population, 2021"
  ) +
  # facet_geo(~code, grid = mygrid) +
  facet_wrap(~code, nrow = 2) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = c(0, 50, 100),
    labels = scales::percent_format(scale = 1),
    position = "left"
  ) +
  scale_x_continuous(
    limits = c(0, 110000),
    breaks = c(0, 50000, 100000),
    labels = scales::dollar_format(suffix = "k", scale = 0.001)
  ) +
  scale_fill_manual(
    values = pal[c(1:2, 4:5)],
    name = NULL
  ) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    text = element_text(color = "#4D4519", family = "oxygen"),
    plot.margin = margin(t = 20, b = 150, l = 20, r = 28),
    plot.background = element_rect(fill = pal[3], color = NA),
    panel.spacing = unit(10, "mm"),
    plot.title = element_text(face = "bold", size = 28, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(margin = margin(t = 20, b = -5), size = 8),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(size = 8, margin = margin(l = 2, t = 2)),
    axis.title.x = element_text(margin = margin(t = 10), size = 10),
    axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10),
    legend.position = "top",
    legend.margin = margin(t = 20, b = 20),
    legend.text = element_text(margin = margin(r = 50))
  )

ggdraw(p) +
  draw_label(
    "this is a test label\ni want to add some definitions to\n the bottom of the plot",
    x = 0.5,
    y = 0.1,
    size = 8,
    fontfamily = "oxygen"
  )

 ggsave("income-education-gap.png", dpi = 300)
dev.off()
