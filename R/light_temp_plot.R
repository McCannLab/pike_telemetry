### load packages -------

library(tidyverse)
library(lubridate)
library(ggpubr)



### import and manipulate data -------

air_temp <- read_csv("data/yk_air_temp_entire_study.csv") %>%
  mutate(julian_day = yday(date))

day_length <- read_csv("data/sunrise_sunset.csv") %>%
  filter(!is.na(sunrise)) %>%
  mutate(day = as.numeric(day),
    day_length_hrs = difftime(sunset, sunrise),
    julian_day = ifelse(month == "01", day,
      ifelse(month == "02", day + 31, 
        ifelse(month == "03", day + 59, 
          ifelse(month == "04", day + 90,
            ifelse(month == "05", day + 120,
              ifelse(month == "06", day + 151,
                ifelse(month == "07", day + 181,
                  ifelse(month == "08", day + 212,
                    ifelse(month == "09", day + 243,
                      ifelse(month == "10", day + 273,
                        ifelse(month == "11", day + 304,
                          ifelse(month == "12", day + 334, NA)))))))))))))

water_temp <- read_csv("data/temp_string_32_interp.csv") %>%
  mutate(temp_cleaned = ifelse(temp < 0, 0, temp))

water_light <- read_csv("data/interpolated_light_data.csv")

day_length_extended <- air_temp %>% left_join(day_length)



### plot lake temperature profile -------

water_temp_plot <- ggplot(water_temp, aes(x=date, y=depth, fill=temp_cleaned))+
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("depth (m)")+
  xlab("date")+
  theme_classic(base_size=16)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  theme(legend.justification=c(0,0), legend.position=c(0.25, 0.05))



### plot light profile -------

light_plot <- ggplot(water_light, aes(x=date, y=depth, fill=log10(light + 1))) +
  scale_y_reverse()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  geom_tile()+
  scale_fill_gradientn(colours = c("white", "dark blue", "blue", "cyan", "green",  "orange", "red"))+
  ylab("depth (m)")+
  xlab("date")+
  labs(fill = "log10(lux)") +
  theme_classic(base_size=16)+
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
  theme_classic(base_size=16)+
  theme(axis.text.x = element_text(colour = 'black'),
    axis.text.y = element_text(colour = 'black'))+
  scale_colour_gradientn(colours = rev(rainbow(5))) +
  theme(legend.justification=c(0,0), legend.position=c(0, 0.05))+
  theme(legend.background = element_rect(colour = NA, fill = NA))



### day length -------

day_length_plot <- ggplot(day_length_extended, aes(x=date, y=day_length_hrs))+
  geom_point()+
  scale_x_date(date_break = "4 month", date_labels = "%b %Y") +
  ylab("day length in hours")+
  xlab("date")+
  ylim(0, 24) +
  theme_classic(base_size=16)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))+
  scale_colour_gradientn(colours = rev(rainbow(5))) +
  theme(legend.justification=c(0,0), legend.position=c(0, 0.05))+
  theme(legend.background = element_rect(colour = NA, fill = NA))



### multiplot -------

multiplot <- ggarrange(day_length_plot,
  air_temp_plot,
  light_plot, 
  water_temp_plot,
  labels = c("A", "B", "C", "D"),
  font.label = list(size = 24),
  ncol = 2,
  nrow = 2, 
  align = "h")

ggsave(filename = "figures/multiplot.tiff", 
  plot = multiplot,
  width = 16,
  height = 12,
  dpi = "print")
