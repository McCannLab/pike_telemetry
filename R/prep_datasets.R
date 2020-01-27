library(tidyverse)
library(data.table)

setwd("C:/Users/mattg/OneDrive - University of Guelph/Current Papers/AlexiePikeTelemetry/data")

#identify files in folder
filenames <- list.files(path = "C:/Users/mattg/OneDrive - University of Guelph/Current Papers/AlexiePikeTelemetry/data", 
                        pattern = "ALL-CALC-+.*csv")

#Read the above files into R together as a list
ld <- plyr::llply(filenames, fread)

#Convert the above list into a dataframe
dat <- plyr::ldply(ld, data.frame)

setwd("C:/Users/mattg/OneDrive - University of Guelph/Current Papers/AlexiePikeTelemetry")

dat <- dat %>% filter(str_detect(TRANSMITTER, "NP")) %>%
  select(TRANSMITTER, DATETIME, LAT, LON, HPE, DEPTH)
colnames(dat) <- c("fish_id", "datetime", "lat", "lon", "hpe", "depth")

#write.csv(dat, "data/alexie_pike_telemetry_data_raw.csv", row.names = F)

info <- read_csv("data/telemetry_surgery_info.csv")
info <- info %>% filter(species == "NP") %>%
  select(fish_id, fork_length, mass, date_tagged)

#write.csv(info, "data/alexie_pike_meta_data.csv", row.names = F)


dat <- right_join(dat, info)

rm(ld, filenames, info)


