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
    quad = case_when(
      x > 0 & y > 0 ~ "VM Women+",
      x < 0 & y > 0 ~ "VM Men+",
      x < 0 & y < 0 ~ "non-VM Men+",
      x > 0 & y < 0 ~ "non-VM Women +"
    )
  ) %>% 
  left_join(abbreviations)


# plot set up -------------------------------------------------------------

# grid for facets
mygrid <- data.frame(
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  col = c(1, 2, 4, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
  code = c("YT", "NT", "NU", "NL", "PE", "BC", "AB", "SK", "MB", 
           "ON", "QC", "NB", "NS"),
  name = c("Yukon", "Northwest Territories", "Nunavut", 
           "Newfoundland and Labrador", "Prince Edward Island", 
           "British Columbia", "Alberta", "Saskatchewan", "Manitoba", 
           "Ontario", "Quebec", "New Brunswick", "Nova Scotia"),
  stringsAsFactors = FALSE
)




# plotting ----------------------------------------------------------------

data  %>% 
  filter(geo != "Canada") %>% 
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = x, ymax = y, fill = quad),
    color = "black"
  ) +
  # facet_wrap(~ geo) +
  facet_geo(~ code, grid = "ca_prov_grid1") +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    panel.background = element_rect(color = "black")
  )
  # coord_equal()
