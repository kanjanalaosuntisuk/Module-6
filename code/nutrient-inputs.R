########################################################################################
# Summary: Land application of nitrogen and phosphorus
# Date: September 25, 2019
# Data analyzed in this session are from the US Geological Survey and include total 
# nitrogen (N) and phosphorus (P) mass (kg) applied to the land surface via fertilizer
# manure, and atmospheric deposition.    
# Measurements are at the county-scale across the US. The total land areas (sq. km) over which 
# nutrients were applied are also reported per county. The data can be accessed from USGS 
# Scientific Investigations Report  2006-5012, "County-Level Estimates of Nutrient Inputs 
# to the Land Surface of the Conterminous United States, 1982-2001" 
# (https://pubs.usgs.gov/sir/2006/5012/) by Barbara C. Ruddy, David L. Lorenz, and David K. 
# Mueller.
########################################################################################

# Motivating question ----
### Which NC counties, on average over the period of record, have the highest rates of N and P input relative to different application types?

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# Read & inspect the dataset ----
nut <- read_csv("data/nutrient-inputs-reformatted.csv")
str(nut)
summary(nut)
dim(nut)

## Tidy data ----
# 1. Gather and separate columns
nut %>%
  gather(-State, -County, -FIPS, - Area, 
         key = "year_nutrient_application",
         value = "kg") %>%
  separate(year_nutrient_application,
           into = c("year", "nutrient", "application"),
           sep = "_") -> nut
head(nut)

# 2. Convert year to numeric
as.numeric(nut$year)
nut$year <- as.numeric(nut$year)
str(nut)
head(nut)

# 3. Handle NAs
nut
## drop_na()
nut %>%
  drop_na()
nut %>%
  drop_na(kg)
nut %>%
  drop_na(Area)

## fill()
nut %>%
  fill(kg, .direction = "down")
nut %>%
  fill(kg, .direction = "up")

## replace_na()
nut %>%
  replace_na(list(kg = 0))

# 4. Create a NC subset
nut %>%
  filter(State == "NC") -> nc
head(nc)
  
# Visualize ----
nc %>%
  filter(County == "WAKE") %>%
  ggplot(mapping = aes(x = year, y = kg, color = application)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ nutrient, nrow = 2,
             scale = "free_y")

# Summarize ----
### Which NC counties, on average over the period of record, have the highest rates of N and P input relative to different application types?

nc %>%
  group_by(County, nutrient, application) %>%
  summarize(mean_kg = mean(kg)) %>%
  ungroup() %>%
  group_by(nutrient, application) %>%
  mutate(rank = min_rank(desc(mean_kg))) %>%
  arrange(rank)
