### load packages -------

library(tidyverse)
library(lubridate)
library(ggpubr)



### import data -------

air_temp <- read_csv("data/yk_air_temp_entire_study.csv")

day_len <- read_csv("data/sunrise_sunset.csv") %>%
  mutate(day_length = difftime(sunset, sunrise))

water_temp <- read_csv("data/temp_string_32_interp.csv")

water_light <- read_csv("data/interpolated_light_data.csv")



### plot lake temperature profile -------

water_temp_plot <- ggplot(water_temp, aes(x=date, y=depth, fill=temp))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("depth (m)")+
  xlab("date")+
  theme_classic(base_size=14)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))



### plot light profile -------

light_plot <- ggplot(water_light, aes(x=date, y=depth, fill=log10(light + 1)))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("white", "dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("depth (m)")+
  xlab("date")+
  labs(fill = "log10(lux)") +
  theme_classic(base_size=14)+
  theme(axis.text.x = element_text(colour = 'black'),
    axis.text.y = element_text(colour = 'black'))+
  theme(legend.justification=c(0,0), legend.position=c(0.15, 0.05))+
  guides(colour = F, shape = F)



### plot air temperature -------

air_temp_plot <- ggplot(air_temp, aes(x=date, y=mean_temp, colour=mean_temp))+
  geom_point()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  ylab("daily air temperature (C)")+
  xlab("date")+
  labs(colour = "temperature") +
  theme_classic(base_size=14)+
  theme(axis.text.x = element_text(colour = 'black'),
    axis.text.y = element_text(colour = 'black'))+
  scale_colour_gradientn(colours = rev(rainbow(5))) +
  theme(legend.justification=c(0,0), legend.position=c(0, 0.05))+
  theme(legend.background = element_rect(colour = NA, fill = NA))



### multiplot -------

multiplot <- ggarrange(air_temp_plot, 
  water_temp_plot, 
  light_plot, 
  air_temp_plot,
  labels = c("A", "B", "C", "D"),
  font.label = list(size = 18),
  ncol = 2,
  nrow = 2, 
  align = "h")

ggsave(filename = "figures/supplementary_figure_4.png", 
  plot = supplementary_figure_4,
  width = 12,
  height = 12,
  dpi = "print")

