library(tidyverse)

air_temp <- read_csv("")
day_len <- read_csv("sunrise_sunset.csv")
water_temp <- read_csv("temp_string_32_interp.csv")
water_light <- read_csv("interpolated_light_data.csv")


# lake temperature profile
ggplot(temp, aes(x=date, y=depth, fill=temp))+
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
ggplot(light, aes(x=date, y=depth, fill=log10(light + 1)))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("white", "dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("Depth (m)")+
  xlab("Date")+
  theme_classic(base_size=20)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))+
  theme(legend.justification=c(0,0), legend.position=c(0.35, 0.05))+
  theme(legend.title=element_blank())+
  guides(colour = F, shape = F)
