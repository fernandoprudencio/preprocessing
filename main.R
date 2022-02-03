library(R.matlab)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(sqldf)
library(ggthemes)
source("utils.R")

# 1. Reading files --------------------------------------------------------

#Temperature data

data_temp(path = "data/temp2m/")

temperature_data <- data_temp(path = "data/temp2m/")

#Parsivel data

parsivelData(path = "data/parsivel/pbmatrix.mat")

parsivel_data <- parsivelData(path = "data/parsivel/pbmatrix.mat")

# 2. Joining files ------------------------------------------------------
dataset <- merge(parsivel_data, temperature_data, by = "datetime")

# 3. Exploring the new dataset -------------------------------------------

group_by(dataset, SYNOP4680) %>% 
  summarise(n= n())

#Observing precipitation data occurring at temperatures below 30°

dataset_events <- filter(dataset, temp <= 30) %>% 
  as_tibble()

summary(dataset_events)

#Observar los tipos de eventos que hay 
group_by(dataset_events, SYNOP4680) %>% 
  summarise(n= n())


#4. Ploteando los eventos de lluvia -----------------------------

data_rain <- filter(dataset_events, RI != 0)
group_by(data_rain, SYNOP4680) %>% 
  summarise(n= n())
summary(data_rain)

ggplot(data_rain, aes(x = temp))+
  geom_histogram(fill = "seagreen3", colour = "seagreen4", alpha = 0.7)+
  scale_x_continuous(breaks = seq(5,30,5))+
  scale_y_continuous(breaks = seq(0,7000, 1000))+
  labs(x = "Temperature °C", y = "Quantity of data",
       title = "Data distribution",
       tag = "Fig. 1")+
theme(axis.text.x = element_text(face = "bold", hjust = 1, size = 12),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 11),
      axis.title.x = element_text(face = "bold", size = 11, 
                                  margin = margin(t = 15, r = 0, b = 5, l = 0)),
      axis.title.y = element_text(face = "bold", size = 11, 
                                  margin = margin(t = 0, r = 15, b = 5, l = 0)))

summary(data_rain)

group_by(data_rain, SYNOP4680) %>% 
  summarise(n= n())

liquidpp <- c(51,52,53,57,58,61,62,63,67,68)
solidpp <- c(88, 89)

#Lluvia
data_liquid <- data_rain %>% filter(SYNOP4680 %in% liquidpp)

ggplot(data_liquid, aes(x = temp))+
  geom_histogram(fill = "palegreen1", colour = "palegreen3", alpha = 0.7, bins = 30)+
  scale_x_continuous(breaks = seq(0,30))+
  scale_y_continuous(breaks = seq(0,6000,1000))+
  labs(x = "Temperature °C", y = "Quantity of data",
       title = "Data distribution",
       subtitle = "Liquid Precipitation",
       tag = "Fig. 2")+
  theme(axis.text.x = element_text(face = "bold", hjust = 1, size = 12),
        axis.text.y = element_text(face = "bold", hjust = 1, size = 12),
        axis.title.x = element_text(face = "bold", size = 15, 
                                    margin = margin(t = 15, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(face = "bold", size = 15, 
                                    margin = margin(t = 0, r = 15, b = 5, l = 0)))

#Solido

data_hail <- data_rain %>% filter(SYNOP4680 %in% solidpp)

ggplot(data_hail, aes(x = temp))+
geom_histogram(fill = "palegreen1", colour = "palegreen3", alpha = 0.7, bins = 30)+
  scale_x_continuous(breaks = seq(0,30))+
  scale_y_continuous(breaks = seq(0,20))+
  labs(x = "Temperature °C", y = "Quantity of data",
       title = "Data distribution",
       subtitle = "Solid Precipitation",
       tag = "Fig. 3")+
  theme(axis.text.x = element_text(face = "bold", hjust = 1, size = 12),
        axis.text.y = element_text(face = "bold", hjust = 1, size = 11),
        axis.title.x = element_text(face = "bold", size = 11, 
                                    margin = margin(t = 15, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(face = "bold", size = 15, 
                                    margin = margin(t = 0, r = 10, b = 5, l = 0)))

#5. CONSTRUCCIÓN DE DATOS DE FRACCIÓN -------------------------------

data_rain <- pptypes(data_rain)
 
# RI <- data_rain %>% group_by(date_day) %>% 
#   summarise(total_RI = sum(RI))


data_frac<- pp_fraction(data_rain)

ggplot2::ggplot(data_rain_frac, aes(x = temp, y = liquid_fracc))+
  geom_point(alpha = 0.5, color = "springgreen2", size = 4)+
  scale_x_continuous(breaks = seq(0, 25))+
  scale_y_continuous(breaks = seq(0, 1, 0.2))+
  labs(x = "Mean temperature °C°", 
       y = "Liquid rain proportion",
       title = "Precipitation that falls as rain",
       tag = "Fig. 4")+
  theme(axis.text.x = element_text(face = "bold", hjust = 1, size = 12),
        axis.text.y = element_text(face = "bold", hjust = 1, size = 11),
        axis.title.x = element_text(face = "bold", size = 11, 
                                    margin = margin(t = 15, r = 0, b = 5, l = 0)),
        axis.title.y = element_text(face = "bold", size = 11, 
                                    margin = margin(t = 0, r = 10, b = 5, l = 0)))

                   