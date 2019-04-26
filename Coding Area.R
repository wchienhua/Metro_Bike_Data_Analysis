library(lattice)
library(rlang)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(plotly)
library(caret)
library(lubridate)
library(httpuv)

rawdata = read.csv("F:/Mtero Bike Competition/metro-bike-share-trip-data.csv")
#Summary Raw Data
glimpse(rawdata)
#Missing Values 
sapply(rawdata, function(x) sum(is.na(x)))

#New dataset without missing values
nonadataset = rawdata[complete.cases(rawdata),]

# Calculate distance in kilometers between two points
library(geodist)
library(geosphere)
p1 = nonadataset[,c('Starting.Station.Longitude','Starting.Station.Latitude')]
p2 = nonadataset[,c('Ending.Station.Longitude','Ending.Station.Latitude')]
# Default unit = meters
distance.between.stations=distVincentyEllipsoid(p1,p2)
nonadataset = cbind(nonadataset,distance.between.stations)
temdata = nonadataset

#Data Analysis
#Time.spend: Travel Time == Duration
temdata = temdata %>%
  separate(Start.Time,c("Start.date","Start.time"),"T") %>%
  separate(End.Time,c("End.date","End.time"),"T") %>%
  mutate(Start.Time = as.POSIXct(paste(Start.date,Start.time, sep=" ")),
         End.Time = as.POSIXct(paste(End.date,End.time, sep=" ")),
         Time.spend = difftime(End.Time,Start.Time,units="secs"))

#Labeling Plan.Duration
temdata = temdata %>%
  mutate(Plan.Duration = ordered(Plan.Duration,
                                 levels = c(0,30,365),
                                 labels = c("Single Ride", "Monthly", "Yearly")))


#Analysis for Customer's Behaviors on Passholder.Type
bike_data_group <- temdata %>%
  group_by(Trip.Route.Category,Passholder.Type,Plan.Duration) %>%
  summarize(average_duration = round(mean(Duration)))
bike_data_group %>%
  subset(Trip.Route.Category == 'One Way') %>%
  plot_ly(x=~Passholder.Type, y=~average_duration,type = 'bar', name='One Way') %>%
  add_trace(data=subset(bike_data_group,Trip.Route.Category == 'Round Trip'),x=~Passholder.Type,y=~average_duration,name='Round Trip')
bike_data_group

library(leaflet)
#Record Count by Map
#Find out Data is not clean
#Longitude has 0 value in the column.
levels(as.factor(temdata$Starting.Station.Latitude))
levels(as.factor(temdata$Starting.Station.Longitude))
#Record Count Cluster Mapping 
temdata %>%
  filter(Starting.Station.Longitude != 0) %>%
  select(lat=Starting.Station.Latitude,lng=Starting.Station.Longitude) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

#Extra Revenue Analysis
temdata = temdata %>%
  mutate(overtimerevenue = ifelse(Duration > (30*60), (Duration-(30*60))/(30*60)*1.75, 0))
temdata = as_tibble(as.data.frame(temdata))
YoM_df = temdata %>%
  mutate(Start.date = as.Date(Start.date)) %>%
  arrange(Start.date) %>%
  group_by(Year_Month=floor_date(Start.date, "month"),Passholder.Type) %>%
  summarise(total.extra.revenue = sum(overtimerevenue))
YoM_df = as_tibble(as.data.frame(YoM_df))
YoM_df %>%
  mutate(total.extra.revenue = as.numeric(total.extra.revenue)) %>%
  subset(Passholder.Type == 'Flex Pass') %>% plot_ly() %>%
  add_trace(x=~Year_Month, y=~total.extra.revenue,name='Flex Pass', type='scatter',mode='lines') %>%
  add_trace(data=subset(YoM_df,Passholder.Type == 'Monthly Pass'),x=~Year_Month, y=~total.extra.revenue,name='Monthly Pass', type='scatter',mode='lines') %>%
  add_trace(data=subset(YoM_df,Passholder.Type == 'Walk-up'),x=~Year_Month, y=~total.extra.revenue,name='Walk-up', type='scatter',mode='lines')


