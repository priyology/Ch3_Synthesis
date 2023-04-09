######### Global Map with reservoirs ================================


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
Spp <- read_csv("data/OsHV1reservoirs_08April2023.csv")
glimpse(Spp)

## unique species
unique(Spp$Paper) #39
unique(Spp$Country) #16
unique(Spp$Species) #29
unique(Spp$Taxa) #12



#### Filter out M. gigas ====

Spp.NoGigas <- Spp %>% 
  filter(Species != "Magallana gigas",
         !is.na(Country))

colSums(is.na(Spp.NoGigas))

## unique species w/o M. gigas
unique(Spp.NoGigas$Paper) #39
unique(Spp.NoGigas$Country) #14
unique(Spp.NoGigas$Species) #28
unique(Spp.NoGigas$Taxa) #9


#### Plot Count of Other taxa ====
library(ggdark)

Reservoirs.Plot <-  Spp.NoGigas %>% 
  group_by(Taxa) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = Taxa, y = count, fill = Taxa)) +
  geom_bar(stat = "identity") +
  dark_theme_classic()

Reservoirs.Plot

#### ** PPT: Reservoirs Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Reservoirs.Plot_fig <- read_pptx()
Reservoirs.Plot_fig <- add_slide(Reservoirs.Plot_fig , layout = "Title and Content", master = "Office Theme")
Reservoirs.Plot_fig <-  ph_with(x = Reservoirs.Plot_fig, value = Reservoirs.Plot, location = ph_location_fullsize() )
Reservoirs.Plot_fig  <- ph_with(x = Reservoirs.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Reservoirs.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Reservoirs.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

#### OsHV-1 Var Type Plot==============

## "Other Oyster" species
Spp.NoGigas %>% 
  filter(Taxa == "Other Oyster") %>% 
  reframe(Ostreids = unique(Species))

## OsHV-1 type
Var.Plot <-  Spp.NoGigas %>%
  filter(Species != "Magallana gigas") %>% 
  group_by(Taxa, OsHV1_var) %>% 
  reframe(count = n()) %>% 
  ggplot(aes(x = Taxa, y = count, fill = OsHV1_var, group = OsHV1_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  dark_theme_classic()

Var.Plot

#### ** PPT: OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Var.Plot_fig <- read_pptx()
Var.Plot_fig <- add_slide(Var.Plot_fig , layout = "Title and Content", master = "Office Theme")
Var.Plot_fig <-  ph_with(x = Var.Plot_fig, value = Var.Plot, location = ph_location_fullsize() )
Var.Plot_fig  <- ph_with(x = Var.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Var.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Var.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################


######## Map of OsHV-1 Reservoirs ==========================

### latitudes
Spp_lat <- Spp.NoGigas$GPS_lat
Spp_lat

### longitudes
Spp_long <- Spp.NoGigas$GPS_long
Spp_long

#### OsHV-1 coords ==============

OsHV1_coord <- Spp.NoGigas %>% 
  filter(OsHV1_var == "OsHV-1")

OsHV1_coord

### latitudes
Spp_nonVarlat <- OsHV1_coord$GPS_lat
Spp_nonVarlat

### longitudes
Spp_nonVarlong <- OsHV1_coord$GPS_long
Spp_nonVarlong

nonVarSites.df <- data.frame(
  lon = Spp_nonVarlong,
  lat = Spp_nonVarlat)

glimpse(nonVarSites.df)

#### OsHV-1 uvar data ==============
OsHV1Var_coord <- Spp.NoGigas %>% 
  filter(OsHV1_var != "OsHV-1")

### latitudes
Spp_Varlat <- OsHV1Var_coord$GPS_lat
Spp_Varlat

### longitudes
Spp_Varlong <- OsHV1Var_coord$GPS_long
Spp_Varlong

VarSites.df <- data.frame(
  lon = Spp_Varlong,
  lat = Spp_Varlat)

glimpse(VarSites.df)


## get Maps API Key
register_google(key = "AIzaSyAromYd5yoy--uNE9ANyPyWCS1PdGZwYGg", write = TRUE) #that is my "Maps API Key": https://console.cloud.google.com/apis/credentials?project=garbage-cat 

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


#load a googlemap 
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = nonVarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()

## generate high quality maps using geom_point() to generate markers

# satellite style map of California with Zoom
Spp_Map <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Spp_Map) +
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