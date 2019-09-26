########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
# Author: Kanjana Laosuntisuk
# Last modified: Spe 26, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
head(cotton)
tail(cotton)
dim(cotton)
summary(cotton)

# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) -> nc_cotton

# 3.2. Divide the data_item column ----
nc_cotton %>%
  separate(data_item, 
           into = c("cotton_type", "measurement"), 
           sep = " - ") -> nc_cotton_tidy

# 3.3. Convert the value column to numeric type ----
nc_cotton_tidy %>%
  filter(value != "(D)") -> nc_cotton_tidy
as.numeric(nc_cotton_tidy$value)
nc_cotton_tidy$value <- as.numeric(nc_cotton_tidy$value)
str(nc_cotton_tidy)
head(nc_cotton_tidy)

# 4. Visualizing trends ----
nc_cotton_tidy %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(rows = vars(measurement), cols = vars(ag_district),
             scales = "free_y")

# 5. Summarize data from 2018 ----
nc_cotton_tidy %>%
  group_by(ag_district, county, cotton_type) %>%
  filter(year == 2018) %>%
  spread(key = measurement, value = value) %>%
  mutate(total_lbs = `ACRES HARVESTED`*`YIELD, MEASURED IN LB / ACRE`) %>%
  ungroup() %>%
  top_n(3, total_lbs)
