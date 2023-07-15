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
OsHV1 <- read_csv("data/OsHV1infections_14Jul2023.csv")
glimpse(OsHV1)

#### Other spp. ====
Spp <- OsHV1 %>% 
  filter(Taxa != "Pacific Oyster",
         Country != "NA")

## unique species
unique(Spp$Paper) #50
unique(Spp$Country) #15
unique(Spp$Species) #37
unique(Spp$Taxa) #13
unique(Spp$Paper)


#### Plot Count of Other taxa ====
library(ggdark)

Reservoirs.Plot <-  Spp %>% 
  group_by(Taxa) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = Taxa, y = count, fill = Taxa)) +
  geom_bar(stat = "identity") +
  theme_classic()

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
Spp.OsHVonly <- Spp %>% 
  filter(OsHV_var == "OsHV-1" | OsHV_var == "OsHV-1 μVar" | OsHV_var == "OsHV-1 non-μvar variant")

unique(Spp.OsHVonly$Country)

SppCountry.Plot <- Spp.OsHVonly %>%
  group_by(Country, OsHV_var) %>%
  #filter(OsHV_var != "Herpesvirus" | OsHV_var != "Herpes-like virus" | OsHV_var != "AVNV") %>%   
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV_var, group = OsHV_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#D53E4F")) +
  theme_classic()

SppCountry.Plot


#### ** PPT: OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SppCountry.Plot_fig <- read_pptx()
SppCountry.Plot_fig <- add_slide(SppCountry.Plot_fig , layout = "Title and Content", master = "Office Theme")
SppCountry.Plot_fig <-  ph_with(x = SppCountry.Plot_fig, value = SppCountry.Plot, location = ph_location_fullsize() )
SppCountry.Plot_fig  <- ph_with(x = SppCountry.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SppCountry.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SppCountry.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################


######## Map of OsHV-1 Reservoirs ==========================

### latitudes
Spp_lat <- Spp$GPS_lat
Spp_lat

### longitudes
Spp_long <- Spp$GPS_long
Spp_long

#### OsHV-1 coords ==============

OsHV1_coord <- Spp %>% 
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
OsHV1Var_coord <- Spp %>% 
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