######### Global Map with infected species locations ================================


######### PRESENTATION-READY MAP ==============================
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools) ##scalebar
library(ggsn) ##scale bar: http://oswaldosantos.github.io/ggsn/ ; 

## ggmap intro: https://appsilon.com/r-ggmap/

#### load data ==============
Spp <- read_csv("data/OsHV-1_Spp_23Mar23.csv")

### latitudes
Spp_lat <- Spp$GPS_lat
Spp_lat

### longitudes
Spp_long <- Spp$GPS_long
Spp_long

## unique species
unique(Spp$Paper) #38 as of March 23, 2023
unique(Spp$Country) #15 as of March 23, 2023
unique(Spp$Species) #29 as of March 23, 2023
unique(Spp$Taxa) #12 as of March 23, 2023



## get Maps API Key
register_google(key = "AIzaSyAromYd5yoy--uNE9ANyPyWCS1PdGZwYGg", write = TRUE) #that is my "Maps API Key": https://console.cloud.google.com/apis/credentials?project=garbage-cat 

#create a data.frame with Hog Island, Bodega Bay & Tomales Bay oyster company leases
sites.df <- data.frame(
  lon = Spp_long,
  lat = Spp_lat)
glimpse(sites.df)

sites.labels <- data.frame(
  #lon = c(-122.947833, -122.927504, -122.865700),
  #lat = c(38.218050, 38.205616, 38.120200),
  #site.name = c("HI", "BB", "TB"))
glimpse(sites.labels)


#load a googlemap 
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = sites.df, scale = 2,  maptype = "hybrid") %>% ggmap()

## generate high quality maps using geom_point() to generate markers

# satellite style map of California with Zoom
Spp_Map <- get_map("TAtlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Spp_Map) +
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'purple', alpha = 0.7,  size = 5) #+
  #geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.05, nudge_y = 0.006, hjust = 1)

# Tone-Lite Map
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5,  size = 4) #+
  #geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

# Watercolor Map
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5,  size = 4) #+
  #geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

TB_watercolor


###########


#make inset maps for sites AND for landmarks
sites_inset_map = ggdraw() +
  draw_plot(TB_watercolor) +
  draw_plot(CA_watercolor, x = 0.638, y = 0.628, width = 0.3, height = 0.4)

sites_inset_map

#run sites_inset_map before running ggsave
ggsave("fig_output/WatercolorMap_sites.png", dpi = 320, bg='transparent') 

landmarks_inset_map = ggdraw() +
  draw_plot(SFBay_watercolor) +
  draw_plot(CA_watercolor_BayArea, x = 0.655, y = 0.700, width = 0.3, height = 0.3)

landmarks_inset_map

#run landmarks_inset_map before running ggsave
ggsave("fig_output/WatercolorMap_landmarks.png", dpi = 320, bg='transparent')