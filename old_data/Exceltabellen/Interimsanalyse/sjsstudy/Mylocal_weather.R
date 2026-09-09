library(tidyverse)
library(glue)
library(lubridate)
t = 0:0.01:2*pi
y = sin(t)
polar(t,y)
inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"
inventory <- read.table(inventory_url,
                        col.names = c("station", "lat","lon","variable","start","end"))


#my lan and lon converted to radian by using *2*pi/360
my_lat <- 52.39921193877698*2*pi/360
my_lon <- 9.804871838646267*2*pi/360

my_station <- inventory %>% 
  mutate( lat_r = lat *2*pi/360,
          lon_r = lon *2*pi/360,
          d_Km =1.693* 3963 * acos((sin(lat_r)* (sin(my_lat))+
                             cos(lat_r)* cos(my_lat)* cos(my_lon -lon_r)))) %>% 
  filter(start < 1950 & end > 2020) %>% 
  top_n(n = -1, d_Km) %>% 
  distinct(station) %>% 
  pull(station)

daily_station <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

local_weahter <- read_csv(daily_station,
         col_names = c("station","date","variable","value","a","b","c","d")) %>% 
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value",values_fill = 0) %>% 
  select(date, PRCP,SNWD,TMAX) %>% 
  mutate(date= ymd(date),
         TMAX = TMAX / 10,
         PRCP = PRCP / 10) %>% 
  rename_all(tolower) %>% 
  filter(prcp < 100)


### CC232### checking anomalies in data ##
local_weahter %>% 
  ggplot(aes(x = date, y= tmax))+
  geom_line()

local_weahter %>% 
  slice_max(n=5,tmax) 

local_weahter %>% 
  ggplot(aes(x = date, y= prcp))+
  geom_line()
