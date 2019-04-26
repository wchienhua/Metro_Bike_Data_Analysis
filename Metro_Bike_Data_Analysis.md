---
title: "Metro_Bike_Data_Analysis"
author: "Chien-Hua Wang"
date: "2019/04/25"
output: 
  html_document:
    keep_md:  yes
---


```r
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
```

# Data Preprocessing

```r
rawdata = read.csv("F:/Mtero Bike Competition/metro-bike-share-trip-data.csv")
#Summary Raw Data
glimpse(rawdata)
```

```
## Observations: 132,427
## Variables: 16
## $ Trip.ID                    <int> 1912818, 1919661, 1933383, 1944197,...
## $ Duration                   <int> 180, 1980, 300, 10860, 420, 780, 60...
## $ Start.Time                 <fct> 2016-07-07T04:17:00, 2016-07-07T06:...
## $ End.Time                   <fct> 2016-07-07T04:20:00, 2016-07-07T06:...
## $ Starting.Station.ID        <int> 3014, 3014, 3016, 3016, 3032, 3021,...
## $ Starting.Station.Latitude  <dbl> 34.05661, 34.05661, 34.05290, 34.05...
## $ Starting.Station.Longitude <dbl> -118.2372, -118.2372, -118.2416, -1...
## $ Ending.Station.ID          <int> 3014, 3014, 3016, 3016, 3032, 3054,...
## $ Ending.Station.Latitude    <dbl> 34.05661, 34.05661, 34.05290, 34.05...
## $ Ending.Station.Longitude   <dbl> -118.2372, -118.2372, -118.2416, -1...
## $ Bike.ID                    <int> 6281, 6281, 5861, 5861, 6674, 6717,...
## $ Plan.Duration              <int> 30, 30, 365, 365, 0, 30, 30, 365, 3...
## $ Trip.Route.Category        <fct> Round Trip, Round Trip, Round Trip,...
## $ Passholder.Type            <fct> Monthly Pass, Monthly Pass, Flex Pa...
## $ Starting.Lat.Long          <fct> "{'longitude': '-118.23721', 'latit...
## $ Ending.Lat.Long            <fct> "{'longitude': '-118.23721', 'latit...
```

```r
#Missing Values 
sapply(rawdata, function(x) sum(is.na(x)))
```

```
##                    Trip.ID                   Duration 
##                          0                          0 
##                 Start.Time                   End.Time 
##                          0                          0 
##        Starting.Station.ID  Starting.Station.Latitude 
##                         19                         48 
## Starting.Station.Longitude          Ending.Station.ID 
##                         48                         96 
##    Ending.Station.Latitude   Ending.Station.Longitude 
##                       1051                       1051 
##                    Bike.ID              Plan.Duration 
##                         10                        766 
##        Trip.Route.Category            Passholder.Type 
##                          0                          0 
##          Starting.Lat.Long            Ending.Lat.Long 
##                          0                          0
```

```r
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
```

# Data Analysis for Travel Distance by different Ticket's Type

```r
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
```

<!--html_preserve--><div id="htmlwidget-8ed357d8453b099de220" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-8ed357d8453b099de220">{"x":{"visdat":{"242c2fee67b9":["function () ","plotlyVisDat"],"242c607139b2":["function () ","data"]},"cur_data":"242c607139b2","attrs":{"242c2fee67b9":{"x":{},"y":{},"name":"One Way","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"242c607139b2":{"x":{},"y":{},"name":"Round Trip","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Passholder.Type","type":"category","categoryorder":"array","categoryarray":["Flex Pass","Monthly Pass","Staff Annual","Walk-up"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"average_duration"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["Flex Pass","Monthly Pass","Walk-up"],"y":[987,740,2162],"name":"One Way","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Flex Pass","Monthly Pass","Walk-up"],"y":[2550,1547,4112],"name":"Round Trip","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
bike_data_group
```

```
## # A tibble: 6 x 4
## # Groups:   Trip.Route.Category, Passholder.Type [6]
##   Trip.Route.Category Passholder.Type Plan.Duration average_duration
##   <fct>               <fct>           <ord>                    <dbl>
## 1 One Way             Flex Pass       Yearly                     987
## 2 One Way             Monthly Pass    Monthly                    740
## 3 One Way             Walk-up         Single Ride               2162
## 4 Round Trip          Flex Pass       Yearly                    2550
## 5 Round Trip          Monthly Pass    Monthly                   1547
## 6 Round Trip          Walk-up         Single Ride               4112
```

####  Based on the map, we could quickly understand each station's situation. Moreover, we could through this map to find out pain points of areas which have less usage. For Monthly and Walk-Up, They show the normal proportion. The One-Way average duration is almost half of Round-Trip duration. However, For Flex Pass, it's One-Way average duration is much less than half of the Round-Trip duration. Therefore, we could conclude that customers who buy the Flex Pass are more prefer the round trip as their transportation behavior. As a result, we could make a promotion on Round-Trip for customers who buy Flex Pass.

# Customer's Usage Area and Record Count

```r
library(leaflet)
#Record Count by Map
#Find out Data is not clean
#Longitude has 0 value in the column.
head(levels(as.factor(temdata$Starting.Station.Latitude)))
```

```
## [1] "0"          "33.987381"  "34.024479"  "34.02589"   "34.028511" 
## [6] "34.0310516"
```

```r
head(levels(as.factor(temdata$Starting.Station.Longitude)))
```

```
## [1] "-118.472832" "-118.393867" "-118.270813" "-118.27081"  "-118.268082"
## [6] "-118.26808"
```

```r
#Record Count Cluster Mapping 
temdata %>%
  filter(Starting.Station.Longitude != 0) %>%
  select(lat=Starting.Station.Latitude,lng=Starting.Station.Longitude) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())
```

<!--html_preserve--><div id="htmlwidget-24ca76815db34fe29054" style="width:672px;height:480px;" class="leaflet html-widget"></div>

####  Based on the map, we could quickly understand each station's situation. Moreover, we could through this map to pain point those areas which have less usage. Those stations close to the airport or parking lots have less usage, so we could assume that customers would prefer to use other tools as instead.

# Extra Revenue Analysis

```r
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
```

<!--html_preserve--><div id="htmlwidget-ae82ad8c3f13c08a280c" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-ae82ad8c3f13c08a280c">{"x":{"visdat":{"242c2c107fa6":["function () ","plotlyVisDat"],"242c50d362cd":["function () ","data"],"242c4b063d7b":["function () ","data"]},"cur_data":"242c4b063d7b","attrs":{"242c2c107fa6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"name":"Flex Pass","type":"scatter","mode":"lines","inherit":true},"242c50d362cd":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"name":"Monthly Pass","type":"scatter","mode":"lines","inherit":true},"242c4b063d7b":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"name":"Walk-up","type":"scatter","mode":"lines","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Year_Month"},"yaxis":{"domain":[0,1],"automargin":true,"title":"total.extra.revenue"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01"],"y":[985.658333333333,610.166666666667,308.175,491.225,240.566666666667,122.791666666667,78.1666666666667,212.391666666667,278.133333333333],"name":"Flex Pass","type":"scatter","mode":"lines","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01"],"y":[2567.36666666667,2346.575,1940.05,1111.54166666667,1479.50833333333,720.358333333333,1141.81666666667,778.808333333333,1652.875],"name":"Monthly Pass","type":"scatter","mode":"lines","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01"],"y":[628.191666666667,14703.675,9262.16666666667,9090.66666666667,4746.75833333333,3115.81666666667,4287.26666666667,3969.29166666667,5783.51666666667],"name":"Walk-up","type":"scatter","mode":"lines","marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

####  The walk-up ticket has much higher extra revenue for the company. Therefore, this will be the company's potential profit model. We should make a strategy and push other types of customers to this type.

