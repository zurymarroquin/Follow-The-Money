library(ggplot2)
library(dplyr)
library(plotly)
library(sf)
library(mapview)
library(leafpop)
library(readxl)
library(tidyverse)
library(scales)
library(tidyr)

##Run this if you want to know available colors for plot: grDevices::colors()

##LOAD WORKSPACE 
#load("operationWonk.RDATA")

##Read in data 
file_path_one_year <- "C:/Users/zmarroquinmarroquin/OneDrive - Furman University/Desktop/operation_wonk/one-year-estimate-summary.xlsx"

file_path_five_year <- "C:/Users/zmarroquinmarroquin/OneDrive - Furman University/Desktop/operation_wonk/race-greenville-5-year-est.xlsx"

data1 <- read_excel(file_path_one_year, sheet=3)

data5 <- read_excel(file_path_five_year, sheet=3)

##Slice data 
new_data1 <- slice(data1,1:6) 

new_data5 <- slice(data5, 1:6)

##Converting raw data into tidy data for ACS one year  
longer_data1 <- new_data1 %>% 
  pivot_longer(c('2016', '2017', '2018', '2019', '2021', '2022'),
               names_to = "year", values_to = "percentage")

tidy_data1 <- longer_data1 %>% pivot_wider(names_from = 1, values_from = percentage)

tidy_data1 <- tidy_data1 %>% 
  rename(
    Year = 'year',
    White_Percentage = 'White Alone',
    White_Upper = 'White Upper',
    White_Lower = 'White Lower',
    Black_Percentage = 'Black Alone',
    Black_Upper = 'Black Upper',
    Black_Lower = 'Black Lower'
  )

tidy_data1$White_Percentage <- round(tidy_data1$White_Percentage, 2)
tidy_data1$White_Upper <- round(tidy_data1$White_Upper, 2)
tidy_data1$White_Lower <- round(tidy_data1$White_Lower, 2)

tidy_data1$Black_Percentage <- round(tidy_data1$Black_Percentage, 2)
tidy_data1$Black_Upper <- round(tidy_data1$Black_Upper, 2)
tidy_data1$Black_Lower <- round(tidy_data1$Black_Lower, 2)

##Converting raw data into tidy data for ACS five year 
longer_data5 <- new_data5 %>% 
  pivot_longer(c('2016', '2017', '2018', '2019', '2020', '2021', '2022'),
               names_to = "year", values_to = "percentage")

tidy_data5 <- longer_data5 %>% pivot_wider(names_from = 1, values_from = percentage)

##Error graphs for ACS 1 year 
ggplot(tidy_data1, aes(x = as.numeric(Year))) +
  geom_line(aes(y = White_Percentage), color = "orange", size = 1)+
  geom_ribbon(aes(ymin = White_Lower, ymax = White_Upper), fill = "lightsalmon", alpha = 0.3) +
  geom_point(aes(y = White_Percentage), color = "orange", size = 2)+
  geom_line(aes(y = Black_Percentage), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Black_Lower, ymax = Black_Upper), fill = "lightblue", alpha = 0.3) +
  geom_point(aes(y = Black_Percentage), color = "blue", size = 2) +
  geom_text(aes(x= as.numeric(Year), y = White_Percentage, label = paste0(White_Percentage, "%")), vjust = -0.5, color = "black")+
  geom_text(aes(x = as.numeric(Year), y = Black_Percentage, label = paste0(Black_Percentage, "%")), vjust = 1, color = "black") +
  labs(title = "Black and White Population Percentages Over Time",
       x = "Year",
       y = "Population Percentage") +
  scale_x_continuous(breaks = as.numeric(unique(tidy_data1$Year)), labels = unique(tidy_data1$Year)) +
  # scale_color_manual(name = "Population",
  #                    values = c("white" = "orange", "black" = "blue"),
  #                    labels = c("White Population", "Black Population "))+
  theme_minimal()







