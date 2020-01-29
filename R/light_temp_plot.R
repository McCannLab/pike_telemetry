library(tidyverse)
library(lubridate)

air_temp <- read_csv("")
day_len <- read_csv("data/sunrise_sunset.csv") %>%
  mutate(day_length = difftime(sunset, sunrise))
water_temp <- read_csv("data/temp_string_32_interp.csv")
water_light <- read_csv("data/interpolated_light_data.csv")


day_len$date <- paste(day_len$year, day_len$month, day_len$day, sep="-") %>% ymd() %>% as.Date()



# lake temperature profile
ggplot(water_temp, aes(x=date, y=depth, fill=temp))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("Depth (m)")+
  xlab("Date")+
  theme_classic(base_size=20)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

#light profile plot
ggplot(water_light, aes(x=date, y=depth, fill=log10(light + 1)))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("white", "dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("Depth (m)")+
  xlab("Date")+
  theme_classic(base_size=20)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))+
  theme(legend.justification=c(0,0), legend.position=c(0.15, 0.05))+
  labs(fill = "log10(lux)") +
  guides(colour = F, shape = F)

ggplot(ec, aes(x=date, y=mean_temp, colour=mean_temp))+
  geom_point()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  theme_bw(base_size=20)+
  ylab("Daily air temperature (°C)")+
  xlab("Date")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))+
  scale_colour_gradientn(colours = rev(rainbow(5))) +
  theme(legend.justification=c(0,0), legend.position=c(0, 0.05))+
  theme(legend.title=element_blank())+
  theme(legend.background = element_rect(colour = NA, fill = NA))