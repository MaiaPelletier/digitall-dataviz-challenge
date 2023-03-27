# TODO: Figure out way to show population counts for each population


# load libraries
library(tidyverse)
library(janitor)
library(geofacet)


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
    visible_minority,
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
    visible_minority,
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
    x = ifelse(gender == "Women+", income, -income),
    y = ifelse(visible_minority == "Not a visible minority", -education, education),
    quad = fct_inorder(
      case_when(
        x > 0 & y > 0 ~ "VM Women+",
        x < 0 & y > 0 ~ "VM Men+",
        x < 0 & y < 0 ~ "non-VM Men+",
        x > 0 & y < 0 ~ "non-VM Women+"
      )
    )
  ) %>% 
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

annotation_text <- data.frame(
  x = c(0, 0, 100000, -100000),
  y = c(100, -100, 0, 0),
  angles = c(0, 0, -90, 90),
  labels = c("Visible minorities", "Not visible minorities", "Women", "Men")
)

# plotting ----------------------------------------------------------------

data  %>%
  filter(geo != "Canada") %>%
  filter(!code %in% c("YT", "NT", "NU")) %>% 
  mutate(
    quad = factor(
      quad, 
      levels = c("VM Women+", "non-VM Women+", "VM Men+", "non-VM Men+")
    )
  ) %>% 
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = income, ymax = education, color = quad),
    fill = NA, size = 1.5
  ) +
  labs(
    x = "Average salary",
    y = "% with postsecondary certificate, diploma or degree",
    title = "Over-educated & under-paid",
    subtitle = "VM Women+ are the most educated in most provinces and the lowest paid on average in all provinces (Nunavut exception)"
  ) +
  # facet_geo(~geo, grid = mygrid) +
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
    labels = scales::dollar_format()
  ) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(t = 20, b = 20, l = 20, r = 20),
    panel.spacing = unit(10, "mm"),
    panel.grid.major = element_line(color = "grey90"),
    # panel.border = element_rect(fill = NA),
    axis.text = element_text(size = 8, margin = margin(l = 2, t = 2)),
    strip.text = element_text(margin = margin(b = 5)),
    axis.title = element_text(),
    axis.title.y = element_text(angle = 90),
    legend.position = "top"
  )

# canada-wide & legend
data  %>%
  filter(geo == "Canada") %>%
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = x, ymax = y, fill = quad),
    color = "white"
  ) +
  # geom_rect(
  #   aes(
  #     xmin = -100000, xmax = 100000, ymin = -100, ymax = 100
  #   ),
  #   fill = NA, color = "black"
  # ) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(
    data = annotation_text,
    aes(x = x, y = y),
    color = "white", 
    shape = 15,
    size = 5
  ) +
  geom_text(
    data = annotation_text,
    aes(x = x, y = y, label = labels, angle = angles),
    size = 5
  ) +
  labs(
    x = "Average salary",
    y = "% with postsecondary certificate, diploma or degree"
  ) +
  guides(
    fill = guide_none()
  ) +
  scale_y_continuous(
    limits = c(-110, 110),
    breaks = c(-100, -50, 0, 50, 100),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    limits = c(-110000, 110000),
    breaks = c(-100000, -50000, 0, 50000, 100000),
    labels = scales::dollar_format()
  ) +
  # facet_geo(~ code, grid = "ca_prov_grid1") +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(25, 25, 25, 25)
  )

data  %>% 
  filter(geo != "Canada") %>% 
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = x, ymax = y, fill = quad)
  ) +
  geom_rect(
    data = national_data,
    aes(xmin = 0, ymin = 0, xmax = x, ymax = y),
    fill = NA, color = "black"
  ) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  guides(
    fill = guide_none()
  ) +
  scale_y_continuous(
    limits = c(-110, 110),
    breaks = c(-100, -50, 0, 50, 100)
    # labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    limits = c(-110000, 110000),
    breaks = c(-110000, -50000, 0, 50000, 100000)
    # labels = scales::dollar_format()
  ) +
  # facet_wrap(~ geo, nrow = 2) +
  facet_geo(~ code, grid = mygrid) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(margin = margin(b = 7), face = "bold")
  )
