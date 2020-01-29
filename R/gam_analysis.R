library(mgcv)
library(tidyverse)

dat <- readRDS("datf.rds")
dat <- dat %>% filter(!is.nan(speed_mean))

dat$hour <- as.integer(dat$hour)
dat$fish_id <- as.factor(dat$fish_id)
dat$lightson_mean <- as.factor(dat$lightson_mean)

hist(sqrt(dat$depth_raw_mean))

depth_mod <- bam(sqrt(depth_raw_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                   + s(fish_id, k = 10, bs = "re"),
                 knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat)
summary(depth_mod)

plot(residuals(depth_mod))


hist(dat$temp_mean)

temp_mod <- bam(sqrt(temp_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                   + s(fish_id, k = 10, bs = "re"),
                 knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat)
summary(temp_mod)

hist(sqrt(dat$speed_mean))

activity_mod <- bam(sqrt(speed_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                  + s(fish_id, k = 10, bs = "re"),
                knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat)
summary(activity_mod)

hist(sqrt(dat$dis_2_shore_mean))

distshore_mod <- bam(sqrt(dis_2_shore_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                      + s(fish_id, k = 10, bs = "re"),
                    knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat)
summary(distshore_mod)










bird_modG <- gam(count ~ te(week, latitude, bs=c("cc", "tp"), k=c(10, 10)), 
                 data=bird_move, method="REML", family="poisson", knots=list(week=c(0, 52)))