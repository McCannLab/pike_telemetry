library(sf)
library(raster)
library(tidyverse)
library(anytime)
# library(lubridate)
source("R/helper_funs.R")


# Shapefile of the lake 
lk <- st_read("data/Alexie_Outline_Islands.shx") %>% 
  st_zm() %>% 
  st_transform(lk, crs = 26911)
lk_pt <- st_cast(lk, "POINT")

# reference tag telemetry data
ref <- read_csv("data/alexie_reference_tag_data_raw.csv") %>% 
  st_as_sf(crs = 4326, coords = c('lon', 'lat')) %>%
  st_transform(lk, crs = 26911) %>%
  rename(tag = fish)

# quick plot to check
plot(lk$geometry)
plot(ref$geometry[1:10000], pch = 19, col = "red", add = T)
  
# change names of tags to match known position file
# SR now is a reference tag, and R is a co-located sync tag
ref$tag <- gsub("SR", "RR", ref$tag)
ref$tag <- gsub("S", "R", ref$tag)
ref$tag <- gsub("RR", "SR", ref$tag)

# check median measured error at each ref tag over the entire study
x = ref %>% 
  mutate(date = strftime(datetime, format = "%Y-%m-%d")) %>%
  group_by(tag) %>%
  summarise(hpem_mean = median(hpem, na.rm = T)) %>%
  select(tag, hpem_mean)


