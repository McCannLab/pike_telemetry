library(sf)
library(raster)
# library(mapview)
library(tidyverse)
library(anytime)
# library(lubridate)
source("R/fish_speed.R")


# Shapefile of the lake 
lk <- st_read("data/Alexie_Outline_Islands.shx") %>% 
    st_zm() %>% 
    st_transform(lk, crs = 26911)

# Raster files 
## Bathymetry
bat <- raster("data/bathy.asc")
## Hardness 
har <- raster("data/E2_c.asc")
## Substrate
sub <- raster("data/substrate.asc")
# visual checks 
# plot(bat)                                                               
# plot(st_geometry(lk), add=T, lwd = 5)  
# plot(st_geometry(lk), lwd = 4) 

## light data (measured) 
light <- read_csv("data/interpolated_light_data.csv")
## temperature data (measured) 
temp <- read_csv("data/temp_string_32_interp.csv")
## sunrise/sumset at Yellowknife
sun <- read_csv("data/YK_sun.csv")


# fish individual data 
meta <- read_csv("data/alexie_pike_meta_data.csv")
# telemetry data (2012-2014)
dat <- read_csv("data/alexie_pike_telemetry_data_raw.csv") %>% 
    st_as_sf(crs = 4326, coords = c('lon', 'lat')) %>%
    st_transform(lk, crs = 26911) 


# Spatial Filtering (remove data point outside of the lakes)
id <- st_contains(lk, dat, sparse = FALSE)  
dat2 <- dat[id,]

# Address potential depth errors max depth = 32m ; error_max = 1.7
dat2 <- dat2 %>% filter(depth > -1.7 & depth < 33.7 )  
# actually there are none below -1.7 and >50000 over 33.7
dat2$depth[dat2$depth < 0] <- 0 # 1010 cases
dat2$depth[dat2$depth < 33.7 & dat2$depth > 32] <- 32

# Extract bathymetry / hardness and substract data 
dat2$bathy <- -raster::extract(bat, dat2)
dat2$hard <- -raster::extract(har, dat2)
dat2$subs <- -raster::extract(sub, dat2)
# Format date and time data
dat2$date <- anydate(dat2$datetime)
dat2$year <- strftime(dat2$date, format = "%y")
dat2$month <- strftime(dat2$date, format = "%m")
dat2$day <- strftime(dat2$date, format = "%d")
dat2$time <- strftime(dat2$datetime, format = "%H:%M:%S")
dat2$hour <- strftime(dat2$datetime, format = "%H")

## ice on / ice off 
dat2$ice <- TRUE 
dat2$ice[dat2$datetime < as.Date("2012-10-31")] <- FALSE
dat2$ice[dat2$datetime > as.Date("2014-05-29")] <- FALSE
dat2$ice[dat2$datetime > as.Date("2013-05-27") & 
    dat2$datetime < as.Date("2013-11-09")] <- FALSE

# visual checks
# plot(st_geometry(dat2))
# plot(st_geometry(lk), add=T, lwd = 4, border = 2)   

# Format sunset surise and depth 
get_sun <- function(x) {
    names(x)[1 + 1:12] <- 1:12
    out <- gather(x, key = month, value = sunrise, -Day)
    names(out)[1] <- "day"
    out$month <- sprintf("%02d", as.numeric(out$month))
    out$day <- sprintf("%02d", out$day)
    out
}

tmp_sunrise <- sun[c(1, 2*1:12)]
sunval <- get_sun(tmp_sunrise)
tmp_sunset <- sun[c(1, 2*1:12 + 1)]
sunset <- get_sun(tmp_sunset)
sunval$sunset<- sunset$sunrise
names(sunval)[4] <- "sunset"

write.csv(sunval, "data/sunrise_sunset.csv", row.names = F)

dat2$depth_raw <- dat2$depth
dat2$depth <- round(dat2$depth, 1)


# Merging light, depth, sunset and surise
dat3 <- dat2 %>%
    left_join(light, by = c("date","depth")) %>% 
    left_join(temp, by = c("date","depth"))  %>% 
    left_join(sunval, by = c("day","month"))


# Compute time and distance between two successive poinst 
# as well as fish speed  and remove data for the first 14 days after the tags was implanted

bef <- anydate(meta$date_tagged) + 14
dat4 <- split(dat3, f = dat3$fish_id)
res <- list()
for (i in seq_along(dat4)) {
    res[[i]] <- fish_speed(dat4[[i]])[-1,]
    k = which(meta$fish_id == res[[i]]$fish_id[2]) 
    res[[i]] <- res[[i]][res[[i]]$date > bef[k],] 
}

res3 <- lapply(res2, function(x) {rownames(x)<-NULL; x })

## 
rbindnr <- function(x) rbind(x, make.row.names = FALSE) 
datf <- do.call(rbindnr, res2)
saveRDS(datf, file = "datf.rds")
