library(mgcv)
library(tidyverse)

dat <- readRDS("datf.rds")

dat$hour <- as.integer(dat$hour)
dat$fish_id <- as.factor(dat$fish_id)
dat$lightson_mean <- as.factor(dat$lightson_mean)

dat2 <- dat %>% filter(!is.nan(speed_mean))

hist(sqrt(dat$depth_raw_mean))

depth_mod <- bam(sqrt(depth_raw_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                   + s(fish_id, k = 10, bs = "re"),
                 knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat2)
summary(depth_mod)

plot(residuals(depth_mod))


hist(dat2$temp_mean)

temp_mod <- bam(sqrt(temp_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                   + s(fish_id, k = 10, bs = "re"),
                 knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat2)
summary(temp_mod)

hist(sqrt(dat2$speed_mean))

activity_mod <- bam(sqrt(speed_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                  + s(fish_id, k = 10, bs = "re"),
                knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat2)
summary(activity_mod)

hist(sqrt(dat2$dis_2_shore_mean))

distshore_mod <- bam(sqrt(dis_2_shore_mean) ~ s(hour, k = 24, bs = "cc") + season + lit_temp + lightson_mean +
                      + s(fish_id, k = 10, bs = "re"),
                    knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat2)
summary(distshore_mod)



### Tim's figures

# there are gaps in the data, apparently corresponding to when gear was offline
dat2 %>%
  ggplot(aes(x = date, y = dab_mean)) + 
  geom_point() +
  theme_bw()

# some individuals data is patchy or short, can we use all individuals
dat2 %>%
  ggplot(aes(x = date, y = dab_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

dat2 %>%
  ggplot(aes(x = date, y = dis_2_shore_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

dat2 %>%
  ggplot(aes(x = date, y = speed_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

dat2 %>%
  ggplot(aes(x = date, y = bathy_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

dat2 %>%
  ggplot(aes(x = date, y = temp_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

dat2 %>%
  ggplot(aes(x = date, y = depth_raw_mean)) + 
  geom_point() +
  facet_wrap(~fish_id) +
  theme_bw()

# pike temperature corresponds pretty well to littoral water temp, but they are clearly deeper at some points
dat2 %>%
  ggplot() + 
  geom_point(aes(x = date, y = temp_mean), color = "grey") +
  facet_wrap(~fish_id) +
  geom_point(aes(x = date, y = lit_temp), color = "black") +
  theme_bw()








bird_modG <- gam(count ~ te(week, latitude, bs=c("cc", "tp"), k=c(10, 10)), 
                 data=bird_move, method="REML", family="poisson", knots=list(week=c(0, 52)))

