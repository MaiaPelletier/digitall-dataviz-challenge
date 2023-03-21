# dimensions being used for income
# income source = market income
# income statistic = average income

# dimensions being used for education
# highest degree = post secondary
# statistic = % distribution

# IDEAS
# Map colour to Men+/Women+
# Map colour intensity to average income of province / % of degrees per province

# VM isn't a good distinguisher because Indigenous is a separate category from 
# "VM" and therefore "non-VM" is not necessarily a stand-in for "white" bc it
# includes Indigenous populations

# load libraries
library(tidyverse)
library(janitor)
library(geofacet)

# renaming function
rename_tables <- function(data) {
  data %>% 
  clean_names() %>% 
  rename_all(~ str_remove(.x, "_[0-9]+\\w*"))
}

# load data (downloaded from CODR tables)
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

# gender income gap -----
gender_income_gap <-
  income %>% 
  filter(
  visible_minority == "Total - Visible minority",
  age == "25 to 64 years",
  gender != "Total - Gender"
  ) %>% 
  pivot_wider(
    names_from = gender,
    values_from = income
  ) %>% 
  mutate(
    diff = `Men+` - `Women+`,
    total = `Men+` + `Women+`,
    gender_income_gap = (diff / (total/2))*100
  ) %>% 
  select(geo, gender_income_gap)

# minority income gap -----
minority_income_gap <-
  income %>% 
  filter(
    visible_minority != "Total - Visible minority",
    age == "25 to 64 years",
    gender == "Total - Gender"
  ) %>% 
  pivot_wider(
    names_from = visible_minority,
    values_from = income
  ) %>% 
  mutate(
    diff = `Not a visible minority` - `Total visible minority population`,
    total = `Not a visible minority` + `Total visible minority population`,
    vm_income_gap = (diff / (total/2))*100
  ) %>% 
  select(geo, vm_income_gap)

income_gap <- left_join(gender_income_gap, minority_income_gap)

# gender education gap -----
gender_education_gap <- 
  education %>% 
  filter(
    visible_minority == "Total - Visible minority",
    gender != "Total - Gender"
  ) %>% 
  pivot_wider(
    names_from = gender,
    values_from = education
  ) %>% 
  mutate(
    gender_edu_gap = `Men+` - `Women+`
  )  %>% 
  select(geo, gender_edu_gap)

# minority education gap -----
minority_education_gap <- 
  education %>% 
  filter(
    visible_minority != "Total - Visible minority",
    age == "25 to 64 years",
    gender == "Total - Gender"
  ) %>% 
  pivot_wider(
    names_from = visible_minority,
    values_from = education
  ) %>% 
  mutate(
    vm_edu_gap = `Not a visible minority` - `Total visible minority population`
  ) %>% 
  select(geo, vm_edu_gap)

education_gap <- left_join(gender_education_gap, minority_education_gap)

ggplot() +
  geom_rect(
    data = income_gap,
    aes(xmin = 0, xmax = gender_income_gap, ymin = 0, ymax = vm_income_gap),
    fill = "blue"
  ) +
  geom_rect(
    data = education_gap,
    aes(xmin = 0, xmax = gender_edu_gap, ymin = 0, ymax = vm_edu_gap)
  ) +
  facet_wrap(~ geo) +
  xlim(c(-55, 55)) +
  ylim(c(-55, 55))

# # create geofacet grid
# mygrid <- data.frame(
#   row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
#   col = c(1, 2, 4, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
#   code = c("YT", "NT", "NU", "NL", "PE", "BC", "AB", "SK", "MB", 
#            "ON", "QC", "NB", "NS"),
#   name = c("Yukon", "Northwest Territories", "Nunavut", 
#            "Newfoundland and Labrador", "Prince Edward Island", 
#            "British Columbia", "Alberta", "Saskatchewan", "Manitoba", 
#            "Ontario", "Quebec", "New Brunswick", "Nova Scotia"),
#   stringsAsFactors = FALSE
# )

