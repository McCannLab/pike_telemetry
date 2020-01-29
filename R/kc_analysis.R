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


## temperature data (measured) 
temp <- read_csv("data/temp_string_32_interp.csv")

## sunrise/sumset at Yellowknife
sun <- read_csv("data/YK_sun.csv")
# Format sunset and surise 
tmp_sunrise <- sun[c(1, 2*1:12)]
sunval <- get_sun(tmp_sunrise)
tmp_sunset <- sun[c(1, 2*1:12 + 1)]
sunset <- get_sun(tmp_sunset)
sunval$sunset <- sunset$sunrise
names(sunval)[4] <- "sunset"


# fish individual data 
meta <- read_csv("data/alexie_pike_meta_data.csv")
# telemetry data (2012-2014)
dat0 <- read_csv("data/alexie_pike_telemetry_data_raw.csv") %>% 
    st_as_sf(crs = 4326, coords = c('lon', 'lat')) %>%
    st_transform(lk, crs = 26911)

# Spatial Filtering (remove data point outside of the lakes)
<<<<<<< HEAD
dat1 <- dat0[st_contains(lk, dat0, sparse = FALSE),]

# Add distance to shore
ind_nr <- st_nearest_feature(st_geometry(dat1), lk_pt)
nr_co <- st_coordinates(lk_pt)[ind_nr,]
dat1$dis_2_shore <- Mod((st_coordinates(dat1)[,1] - nr_co[,1]) + 1i*(st_coordinates(dat1)[,2] -  nr_co[,2]))

# add sunset sunrise and lighton 
dat1$month <- strftime(dat1$datetime, format = "%m")
dat1$day <- strftime(dat1$datetime, format = "%d")
dat1$time <- strftime(dat1$datetime, format = "%H:%M:%S")
dat1 <- dat1 %>% left_join(sunval, by = c("day","month"))
dat1$lightson <- 0
dat1$lightson[dat1$time < dat1$sunset & dat1$time > dat1$sunrise] <- 1 

# Address potential depth errors max depth = 32m ; error_max = 1.7
dat1$depth[dat1$depth < -1.7 | dat1$depth > 33.7] <- NA 
=======
id <- st_contains(lk, dat, sparse = FALSE)  
dat2 <- dat[id,]

# Address potential depth errors max depth = 32m ; error_max = 1.7
dat2 <- dat2 %>% filter(depth > -1.7 & depth < 33.7 )  
>>>>>>> master
# actually there are none below -1.7 and >50000 over 33.7
dat1$depth[dat1$depth < 0] <- 0 # 1010 cases
dat1$depth[dat1$depth < 33.7 & dat1$depth > 32] <- 32

# Extract bathymetry / hardness and substract data 
dat1$bathy <- -raster::extract(bat, dat1)
dat1$hard <- -raster::extract(har, dat1)
dat1$subs <- -raster::extract(sub, dat1)
# Format date and time data
<<<<<<< HEAD
dat1$date <- anydate(dat1$datetime)
dat1$hour <- strftime(dat1$datetime, format = "%H")
=======
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

dat2$depth_raw <- dat2$depth
dat2$depth <- round(dat2$depth, 1)


# Merging light, depth, sunset and surise
dat3 <- dat2 %>%
    left_join(light, by = c("date","depth")) %>% 
    left_join(temp, by = c("date","depth"))  %>% 
    left_join(sunval, by = c("day","month"))
>>>>>>> master

# Format depth 
dat1$depth_raw <- dat1$depth
dat1$depth <- round(dat1$depth, 1)
# Merging depth
dat1 <- dat1 %>%
    left_join(temp, by = c("date", "depth"))
# names(dat1)

# Compute time and distance between two successive poinst 
# as well as fish speed  and remove data for the first 14 days after the tags was implanted

bef <- anydate(meta$date_tagged) + 14
dat2 <- split(dat1, f = dat1$fish_id)
res <- list()
for (i in seq_along(dat2)) {
    res[[i]] <- fish_speed(dat2[[i]])[-1,]
    k = which(meta$fish_id == res[[i]]$fish_id[2]) 
    res[[i]] <- res[[i]][res[[i]]$date > bef[k],] 
}
dat3 <- do.call(rbind, res)

## 
dat3$date <- anydate(dat3$datetime)
dat3$dab <- dat3$bathy - dat3$depth_raw
dat3$dab[dat3$dab < -5] <- NA 
dat3$dab[dat3$dab >= -5 & dat3$dab < 0] <- 0

datf <- dat3 %>% 
    select(dab, dis_2_shore, speed, bathy, temp, depth_raw, lightson, 
        fish_id, date, hour) %>% 
    group_by(fish_id, date, hour) %>% 
    summarise_if(is.numeric, c(mean=mean, min=min, max=max, median=median), 
        na.rm = TRUE)

# add year / monts / day
datf$year <- strftime(datf$date, format = "%y")
datf$month <- strftime(datf$date, format = "%m")
datf$day <- strftime(datf$date, format = "%d")


# Add Predictor values 

## ice on / ice off 
datf$ice <- TRUE 
datf$ice[datf$date < as.Date("2012-10-31")] <- FALSE
datf$ice[datf$date >= as.Date("2014-05-29")] <- FALSE
datf$ice[datf$date >= as.Date("2013-05-27") & 
    datf$date < as.Date("2013-11-09")] <- FALSE

## season 
datf <- datf %>%
  mutate(season = case_when(
    date > as.Date("2012-06-20") & date < as.Date("2012-09-10") ~ "summer",
    date >= as.Date("2012-09-10") & date < as.Date("2012-10-31") ~ "fall",
    date >= as.Date("2012-10-31") & date < as.Date("2012-12-03") ~ "winter_l",
    date >= as.Date("2012-12-03") & date <= as.Date("2013-02-04")  ~ "winter_nl",
    date > as.Date("2013-02-04") & date < as.Date("2013-05-25") ~ "winter_l",
    date >= as.Date("2013-05-25") & date < as.Date("2013-06-22") ~ "spring",
    date >= as.Date("2013-06-22") & date < as.Date("2013-08-29") ~ "summer",
    date >= as.Date("2013-08-29") & date < as.Date("2013-11-09") ~ "fall",
    date >= as.Date("2013-11-09") & date < as.Date("2013-12-07") ~ "winter_l",
    date >= as.Date("2013-12-07") & date <= as.Date("2014-02-23") ~ "winter_nl",
    date > as.Date("2014-02-23") & date < as.Date("2013-05-29") ~ "winter_l",
    date >= as.Date("2014-05-29") & date < as.Date("2014-06-20") ~ "spring",
    date >= as.Date("2014-06-20") & date < as.Date("2014-08-26")  ~ "summer",
    date >= as.Date("2014-08-26") ~ "fall"
    ))


# Surface Water Temeprature
lit_temp <- temp %>% 
    filter(depth < 6) %>% 
    group_by(date) %>% 
    summarise(lit_temp = mean(temp))

datf <- datf %>% left_join(lit_temp)

# assign twilight hours to the lightoff or lighton according to the 
# data recorder (if>.5 mean most of observation are when light is on)
datf$lightson_mean

<<<<<<< HEAD
=======
res3 <- lapply(res2, function(x) {rownames(x)<-NULL; x })

## 
rbindnr <- function(x) rbind(x, make.row.names = FALSE) 
datf <- do.call(rbindnr, res2)
>>>>>>> master
saveRDS(datf, file = "datf.rds")













