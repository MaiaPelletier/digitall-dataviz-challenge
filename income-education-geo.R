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
        x > 0 & y < 0 ~ "non-VM Women +"
      )
    )
  ) %>% 
  left_join(abbreviations)

national_data <- data  %>% 
  filter(geo == "Canada") %>% 
  select(quad, x, y)

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
  x = c(0, 0, 90000, -90000),
  y = c(90, -90, 0, 0),
  angles = c(0, 0, -90, 90),
  labels = c("Visible minorities", "Not visible minorities", "Women", "Men")
)

# plotting ----------------------------------------------------------------



# canada-wide & legend
data  %>%
  filter(geo == "Canada") %>%
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = x, ymax = y, fill = quad),
    color = "white"
  ) +
  geom_rect(
    aes(
      xmin = -100000, xmax = 100000, ymin = -100, ymax = 100
    ),
    fill = NA, color = "black", size = 3
  ) +
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
    aes(x = x, y = y, label = labels, angle = angles)
  ) +
  labs(
    x = "Average salary",
    y = "% with postsecondary certificate, diploma or degree"
  ) +
  guides(
    fill = guide_none()
  ) +
  scale_y_continuous(
    breaks = 100,
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = 100000,
    labels = scales::dollar_format()
  ) +
  # facet_geo(~ code, grid = "ca_prov_grid1") +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(hjust = 0.05, size = 8),
    axis.title.y = element_text(hjust = 0.1, size = 8)
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
  # facet_wrap(~ geo, nrow = 2) +
  facet_geo(~ code, grid = mygrid) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(20, 20, 20, 20),
    strip.text = element_text(margin = margin(b = 7), face = "bold")
  )
