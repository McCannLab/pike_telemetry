library(mgcv)

dat <- readRDS("datf.rds")
dat$hour <- as.integer(dat$hour)

hist(sqrt(dat$depth_raw_mean))

depth_mod <- gam(sqrt(depth_raw_mean) ??? s(hour, k = 24, bs = "cc") + s(fish_id, k = 10, bs = "re"),
                 knots = list(hour = c(0, 23)), method = "REML", family = "gaussian", data = dat)


bird_modG <- gam(count ??? te(week, latitude, bs=c("cc", "tp"), k=c(10, 10)), 
                 data=bird_move, method="REML", family="poisson", knots=list(week=c(0,





bird_modG <- gam(count ??? te(week, latitude, bs=c("cc", "tp"), k=c(10, 10)), data=bird_move, method="REML", family="poisson", knots=list(week=c(0, 52)))