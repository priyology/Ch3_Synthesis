######### Global Mgigas of OsHV-1 Detections ================================

######### PRESENTATION-READY Mgigas ==============================
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools) ##scalebar
library(ggsn) ##scale bar: http://oswaldosantos.github.io/ggsn/ ; 
library(cowplot)

## ggMgigas intro: https://appsilon.com/r-ggMgigas/

#### load data ==============
OsHV1 <- read_csv("data/OsHV1infections_30Jul2023.csv")
glimpse(OsHV1)

unique(OsHV1$OsHV_var)
# [1] "OsHV-1"                  "OsHV-1 μVar"            
#[3] "Herpes-like virus"       "Herpesvirus"            
#[5] "AVNV"                    "OsHV"                   
#[7] "OsHV-1 non-μvar variant"

Mgigas <- OsHV1 %>% 
  filter(Taxa == "Pacific Oyster",
         Country != "NA")

## unique species
unique(Mgigas$Paper) #79
unique(Mgigas$Country) #18
unique(Mgigas$Species) #1


## OsHV-1
Mgigas %>% 
  group_by(Country) %>%
  filter(OsHV_var == "OsHV-1") %>% 
  reframe(count = n()) #13 countries
  
## Not OsHV-1
Mgigas %>% 
  group_by(Country) %>%
  reframe(count = n()) #17 countries

## OsHV-1 type

Mgigas.OsHVonly <- Mgigas %>% 
  filter(OsHV_var == "OsHV-1" | OsHV_var == "OsHV-1 μVar" | OsHV_var == "OsHV-1 non-μvar variant")

Country.Plot <- Mgigas.OsHVonly %>%
  group_by(Country, OsHV_var) %>%
  #filter(OsHV_var != "Herpesvirus" | OsHV_var != "Herpes-like virus" | OsHV_var != "AVNV") %>%   
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV_var, group = OsHV_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) +
  theme_classic()

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


Mgigas.nonOsHV <- Mgigas %>% 
  filter(OsHV_var == "AVNV" | OsHV_var == "Herpesvirus" | OsHV_var == "Herpes-like virus")

unique(Mgigas.nonOsHV$OsHV_var)

NonOsHV.Plot <- Mgigas.nonOsHV %>%
  group_by(Country, OsHV_var) %>%
  #filter(OsHV_var != "Herpesvirus" | OsHV_var != "Herpes-like virus" | OsHV_var != "AVNV") %>%   
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV_var, group = OsHV_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values=c("#FF7F00", "#4DAF4A", "#984EA3")) +
  theme_classic()

NonOsHV.Plot

#### ** PPT: non-OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
NonOsHV.Plot_fig <- read_pptx()
NonOsHV.Plot_fig <- add_slide(NonOsHV.Plot_fig , layout = "Title and Content", master = "Office Theme")
NonOsHV.Plot_fig <-  ph_with(x = NonOsHV.Plot_fig, value = NonOsHV.Plot, location = ph_location_fullsize() )
NonOsHV.Plot_fig  <- ph_with(x = NonOsHV.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(NonOsHV.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = NonOsHV.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

######## Map: Mgigas of OsHV-1 Detections ==========================

unique(Mgigas$OsHV_var)
# [1] "OsHV-1"                  "OsHV-1 μVar"             "Herpes-like virus"      
# [4] "Herpesvirus"             "AVNV"                    "OsHV-1 non-μvar variant"

#### OsHV-1 coords ==============

glimpse(Mgigas)

OsHV1_coord <- Mgigas %>% 
  filter(OsHV_var == "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

OsHV1_coord$GPS_lat <- as.double(OsHV1_coord$GPS_lat)
OsHV1_coord$GPS_lat

OsHV1_coord$GPS_long <- as.double(OsHV1_coord$GPS_long)
OsHV1_coord$GPS_long

### latitudes
Mgigas_OsHV1_lat <- OsHV1_coord$GPS_lat
Mgigas_OsHV1_lat

### longitudes
Mgigas_OsHV1long <- OsHV1_coord$GPS_long
Mgigas_OsHV1long

OsHV1Sites.df <- data.frame(
  lon = Mgigas_OsHV1long,
  lat = Mgigas_OsHV1_lat)

glimpse(OsHV1Sites.df)

#### OsHV-1 μVar data ==============
OsHV1Var_coord <- Mgigas %>% 
  filter(OsHV_var == "OsHV-1 μVar",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1Var_coord$GPS_lat <- as.double(OsHV1Var_coord$GPS_lat)
OsHV1Var_coord$GPS_lat

OsHV1Var_coord$GPS_long <- as.double(OsHV1Var_coord$GPS_long)
OsHV1Var_coord$GPS_long


### latitudes
Mgigas_Varlat <- OsHV1Var_coord$GPS_lat
Mgigas_Varlat

### longitudes
Mgigas_Varlong <- OsHV1Var_coord$GPS_long
Mgigas_Varlong

VarSites.df <- data.frame(
  lon = Mgigas_Varlong,
  lat = Mgigas_Varlat)

glimpse(VarSites.df)


#### OsHV-1 non-μVar data ==============
OsHV1nonVar_coord <- Mgigas %>% 
  filter(OsHV_var == "OsHV-1 non-μvar variant",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1nonVar_coord$GPS_lat <- as.double(OsHV1nonVar_coord$GPS_lat)
OsHV1nonVar_coord$GPS_lat

OsHV1nonVar_coord$GPS_long <- as.double(OsHV1nonVar_coord$GPS_long)
OsHV1nonVar_coord$GPS_long


### latitudes
Mgigas_nonVarlat <- OsHV1nonVar_coord$GPS_lat
Mgigas_nonVarlat

### longitudes
Mgigas_nonVarlong <- OsHV1nonVar_coord$GPS_long
Mgigas_nonVarlong

nonVarSites.df <- data.frame(
  lon = Mgigas_nonVarlong,
  lat = Mgigas_nonVarlat)

glimpse(nonVarSites.df)

#### AVNV data ==============
AVNV_coord <- Mgigas %>% 
  filter(OsHV_var == "AVNV",
         !is.na(GPS_lat),
         !is.na(GPS_long))

AVNV_coord$GPS_lat <- as.double(AVNV_coord$GPS_lat)
AVNV_coord$GPS_lat

AVNV_coord$GPS_long <- as.double(AVNV_coord$GPS_long)
AVNV_coord$GPS_long


### latitudes
Mgigas_ANVNlat <- AVNV_coord$GPS_lat
Mgigas_ANVNlat

### longitudes
Mgigas_ANVNlong <- AVNV_coord$GPS_long
Mgigas_ANVNlong

AVNVSites.df <- data.frame(
  lon = Mgigas_ANVNlong,
  lat = Mgigas_ANVNlat)

glimpse(AVNVSites.df)

#### Herpesvirus data ==============
Herp_coord <- Mgigas %>% 
  filter(OsHV_var == "Herpesvirus",
         !is.na(GPS_lat),
         !is.na(GPS_long))

Herp_coord$GPS_lat <- as.double(Herp_coord$GPS_lat)
Herp_coord$GPS_lat

Herp_coord$GPS_long <- as.double(Herp_coord$GPS_long)
Herp_coord$GPS_long


### latitudes
Mgigas_Herplat <- Herp_coord$GPS_lat
Mgigas_Herplat

### longitudes
Mgigas_Herplong <- Herp_coord$GPS_long
Mgigas_Herplong

HerpSites.df <- data.frame(
  lon = Mgigas_Herplong,
  lat = Mgigas_Herplat)

glimpse(HerpSites.df)

#### Herpes-like data ==============
herpL_coord <- Mgigas %>% 
  filter(OsHV_var == "Herpes-like virus",
         !is.na(GPS_lat),
         !is.na(GPS_long))

herpL_coord$GPS_lat <- as.double(herpL_coord$GPS_lat)
herpL_coord$GPS_lat

herpL_coord$GPS_long <- as.double(herpL_coord$GPS_long)
herpL_coord$GPS_long


### latitudes
Mgigas_herpLlat <- herpL_coord$GPS_lat
Mgigas_herpLlat

### longitudes
Mgigas_herpLlong <- herpL_coord$GPS_long
Mgigas_herpLlong

herpLSites.df <- data.frame(
  lon = Mgigas_herpLlong,
  lat = Mgigas_herpLlat)

glimpse(herpLSites.df)

## get Mgigass API Key
register_google(key = "AIzaSyAPHAhoKrfamwGo3d06FAirHsuqU6dOvZM", write = TRUE) #that is my "Mgigass API Key": https://console.cloud.google.com/apis/credentials?project=garbage-cat 

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

#load a googlemaps 
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = nonVarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()


## generate high quality ggmap() using geom_point() to generate markers

# satellite style Mgigas of California with Zoom
Detection_Mgigas <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Detection_Mgigas) +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 6) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 6) +
  geom_point(data = OsHVSites.df, aes(x = lon, y = lat), color = '#ABD9E9', alpha = 0.7,  size = 6) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#FF7F00', alpha = 0.7,  size = 6) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 6)


# Tone-Lite Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 6) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 6) +
  geom_point(data = OsHVSites.df, aes(x = lon, y = lat), color = '#ABD9E9', alpha = 0.7,  size = 6) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#FF7F00', alpha = 0.7,  size = 6) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 6)

# Watercolor Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 6) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 6) +
  geom_point(data = OsHVSites.df, aes(x = lon, y = lat), color = '#ABD9E9', alpha = 0.7,  size = 6) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#FF7F00', alpha = 0.7,  size = 6) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 6)
