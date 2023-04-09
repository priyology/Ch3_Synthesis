######### Global Map of OsHV-1 Detections ================================

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
Map <- read_csv("data/OsHV1map_08April2023.csv")
glimpse(Map)


## unique species
unique(Map$Paper) #35
unique(Map$Country) #19
unique(Map$Species) #10

Map %>% 
  group_by(Country) %>%
  filter(OsHV1_var == "OsHV-1") %>% 
  reframe(count = n()) #14 countries

Map %>% 
  group_by(Country) %>%
  filter(OsHV1_var != "OsHV-1") %>% 
  reframe(count = n()) #13 countries

#### Filter out M. gigas ====
Map.gigas <- Map %>% 
  filter(Species == "Magallana gigas")

glimpse(Map.gigas)

unique(Map.gigas$Paper) #32
unique(Map.gigas$Country) #19
unique(Map.gigas$Species) #1

## OsHV-1 type
library(ggdark)

Country.Plot <-  Map.gigas %>%
  group_by(Country, OsHV1_var) %>% 
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV1_var, group = OsHV1_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  dark_theme_classic()

Country.Plot

#### ** PPT: OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Country.Plot_fig <- read_pptx()
Country.Plot_fig <- add_slide(Country.Plot_fig , layout = "Title and Content", master = "Office Theme")
Country.Plot_fig <-  ph_with(x = Country.Plot_fig, value = Country.Plot, location = ph_location_fullsize() )
Country.Plot_fig  <- ph_with(x = Country.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Country.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Country.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################


######## Map of OsHV-1 Detections ==========================

#### OsHV-1 coords ==============

glimpse(Map.gigas)

OsHV1_coord <- Map.gigas %>% 
  filter(!is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

### latitudes
Map_nonVarlat <- OsHV1_coord$GPS_lat
Map_nonVarlat

### longitudes
Map_nonVarlong <- OsHV1_coord$GPS_long
Map_nonVarlong

nonVarSites.df <- data.frame(
  lon = Map_nonVarlong,
  lat = Map_nonVarlat)



glimpse(nonVarSites.df)

#### OsHV-1 uvar data ==============
OsHV1Var_coord <- Map.gigas %>% 
  filter(OsHV1_var != "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1Var_coord$GPS_lat <- as.double(OsHV1Var_coord$GPS_lat)
OsHV1Var_coord$GPS_lat

### latitudes
Map_Varlat <- OsHV1Var_coord$GPS_lat
Map_Varlat

### longitudes
Map_Varlong <- OsHV1Var_coord$GPS_long
Map_Varlong

VarSites.df <- data.frame(
  lon = Map_Varlong,
  lat = Map_Varlat)

glimpse(VarSites.df)

## get Maps API Key
register_google(key = "AIzaSyAPHAhoKrfamwGo3d06FAirHsuqU6dOvZM", write = TRUE) #that is my "Maps API Key": https://console.cloud.google.com/apis/credentials?project=garbage-cat 

#create a data.frame
#sites.df <- data.frame(
#  lon = Spp_long,
#  lat = Spp_lat)
# glimpse(sites.df)

#sites.labels <- data.frame(
#lon = c(-122.947833, -122.927504, -122.865700),
#lat = c(38.218050, 38.205616, 38.120200),
#site.name = c("HI", "BB", "TB"))
#glimpse(sites.labels)

nonVarSites.df

#load a googlemap 
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = nonVarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()

## generate high quality maps using geom_point() to generate markers

# satellite style map of California with Zoom
Detection_Map <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Detection_Map) +
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5)
#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.05, nudge_y = 0.006, hjust = 1)

# Tone-Lite Map
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5)
#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

# Watercolor Map
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = 'purple', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = 'red', alpha = 0.7,  size = 5)

#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

TB_watercolor


