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
OsHV1 <- read_csv("data/OsHV1infections_19Aug2023.csv")
glimpse(OsHV1)

#### Other spp. ====
Spp <- OsHV1 %>% 
  filter(Species != "Magallana gigas",
         Country != "NA")

## unique species
unique(Spp$Paper) #51
unique(Spp$Country) #15
unique(Spp$Species) #42
unique(Spp$Taxa) #13
unique(Spp$Paper)
unique(Spp$OsHV_var)

#### Plot Host Taxa ====
library(ggdark)

Hosts.Prop <-  Spp %>% 
  group_by(Taxa) %>%
  reframe(n=n()) %>%
  mutate(freq = n / sum(n))

Hosts.Prop

Hosts.Prop.Plot <- ggplot(aes(x = Taxa, y = freq, fill = Taxa), data = Hosts.Prop) +
  geom_col(position=position_dodge2(preserve = "single")) +
  #scale_fill_manual(values=c("#4DA8F9", "#4DAF4A","#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Hosts.Prop.Plot 

#### ** PPT: Host Taxa Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Hosts.Prop.Plot_fig <- read_pptx()
Hosts.Prop.Plot_fig <- add_slide(Hosts.Prop.Plot_fig , layout = "Title and Content", master = "Office Theme")
Hosts.Prop.Plot_fig <-  ph_with(x = Hosts.Prop.Plot_fig, value = Hosts.Prop.Plot, location = ph_location_fullsize() )
Hosts.Prop.Plot_fig  <- ph_with(x = Hosts.Prop.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Hosts.Prop.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Hosts.Prop.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################



#### Plot Count of Other taxa ====
library(ggdark)

Reservoirs.Data <-  Spp %>% 
  select(OsHV_var, Country) %>%
  group_by(OsHV_var, Country) %>%
  reframe(n=n()) %>%
  mutate(freq = n / sum(n))

Reservoirs.Data

Reservoirs.Plot <- ggplot(aes(x = Country, y = freq, group = OsHV_var, fill = OsHV_var), data = Reservoirs.Data) +
  geom_col(position=position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("#4DA8F9", "#4DAF4A","#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

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

#### OsHV-1 Var by Species Plot==============

unique(Spp$OsHV_var)

## Other species
Reservoirs.Spp <-  Spp %>% 
  select(OsHV_var, Species) %>%
  group_by(OsHV_var, Species) %>%
  reframe(n=n()) %>%
  mutate(freq = n / sum(n))

Reservoirs.Spp

Reservoirs.Spp.Plot <- ggplot(aes(x = Species, y = freq, group = OsHV_var, fill = OsHV_var), data = Reservoirs.Spp) +
  geom_col(position=position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("#4DA8F9", "#4DAF4A","#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Reservoirs.Spp.Plot

#### ** PPT: OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Reservoirs.Spp.Plot_fig <- read_pptx()
Reservoirs.Spp.Plot_fig <- add_slide(Reservoirs.Spp.Plot_fig , layout = "Title and Content", master = "Office Theme")
Reservoirs.Spp.Plot_fig <-  ph_with(x = Reservoirs.Spp.Plot_fig, value = Reservoirs.Spp.Plot, location = ph_location_fullsize() )
Reservoirs.Spp.Plot_fig  <- ph_with(x = Reservoirs.Spp.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Reservoirs.Spp.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Reservoirs.Spp.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

unique(Spp.nonOsHV$OsHV_var)

Spp.nonOsHV <- Spp %>% 
  filter(OsHV_var == "AVNV" | OsHV_var == "Herpes-like virus")

Spp.nonOsHV.Plot <- Spp.nonOsHV %>%
  group_by(Country, OsHV_var) %>%
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV_var, group = OsHV_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values=c("#FF7F00", "#4DAF4A", "#984EA3")) +
  theme_classic()

Spp.nonOsHV.Plot

#### ** PPT: non-OsHV-1 VarType Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Spp.nonOsHV.Plot_fig <- read_pptx()
Spp.nonOsHV.Plot_fig <- add_slide(Spp.nonOsHV.Plot_fig , layout = "Title and Content", master = "Office Theme")
Spp.nonOsHV.Plot_fig <-  ph_with(x = Spp.nonOsHV.Plot_fig, value = Spp.nonOsHV.Plot, location = ph_location_fullsize() )
Spp.nonOsHV.Plot_fig  <- ph_with(x = Spp.nonOsHV.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Spp.nonOsHV.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Spp.nonOsHV.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

######## Map of OsHV-1 Reservoirs ==========================

unique(Spp$OsHV_var)
# [1] "OsHV-1"            "OsHV-1 μVar"       "Herpes-like virus"     
# [4] "AVNV"              "OsHV"

######## 1991 - 2000 ==========================

Spp_91_00 <- Spp %>% 
  filter(Species != "Magallana gigas",
         Year_Sampled >= "1990" & Year_Sampled <= "2000")

View(Spp_91_00)

## OsHV-1
Spp_91_00 %>% 
  group_by(Country) %>%
  filter(OsHV_var == "OsHV-1") %>% 
  reframe(count = n()) #14 countries

## Not OsHV-1
Spp_91_00 %>% 
  group_by(Country) %>%
  reframe(count = n()) #18 countries

## OsHV-1 type


unique(Spp_91_00$OsHV_var)
# [1] "OsHV-1"            "Herpes-like virus" 


#### OsHV-1 coords ==============

glimpse(Spp_91_00)

OsHV1_coord <- Spp_91_00 %>% 
  filter(OsHV_var == "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

OsHV1_coord$GPS_lat <- as.double(OsHV1_coord$GPS_lat)
OsHV1_coord$GPS_lat

OsHV1_coord$GPS_long <- as.double(OsHV1_coord$GPS_long)
OsHV1_coord$GPS_long

### latitudes
Spp_OsHV1_lat <- OsHV1_coord$GPS_lat
Spp_OsHV1_lat

### longitudes
Spp_OsHV1long <- OsHV1_coord$GPS_long
Spp_OsHV1long

OsHV1Sites.df <- data.frame(
  lon = Spp_OsHV1long,
  lat = Spp_OsHV1_lat)

glimpse(OsHV1Sites.df)

#### Herpes-like data ==============
herpL_coord <- Spp_91_00 %>% 
  filter(OsHV_var == "Herpes-like virus",
         !is.na(GPS_lat),
         !is.na(GPS_long))

herpL_coord$GPS_lat <- as.double(herpL_coord$GPS_lat)
herpL_coord$GPS_lat

herpL_coord$GPS_long <- as.double(herpL_coord$GPS_long)
herpL_coord$GPS_long


### latitudes
Spp_herpLlat <- herpL_coord$GPS_lat
Spp_herpLlat

### longitudes
Spp_herpLlong <- herpL_coord$GPS_long
Spp_herpLlong

herpLSites.df <- data.frame(
  lon = Spp_herpLlong,
  lat = Spp_herpLlat)

glimpse(herpLSites.df)

## get Mgigas API Key
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
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = VarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()


## generate high quality ggmap() using geom_point() to generate markers

# satellite style Mgigas of California with Zoom
Detection_Spp_91_00 <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Detection_Spp_91_00) +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 4)


# Tone-Lite Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 4)

# Watercolor Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = herpLSites.df, aes(x = lon, y = lat), color = '#4DAF4A', alpha = 0.7,  size = 5)

######## 2001 - 2010 ==========================

Spp_01_10 <- Spp %>% 
  filter(Species != "Magallana gigas",
         Year_Sampled >= "2001" & Year_Sampled <= "2010")

View(Spp_01_10)

## OsHV-1
Spp_01_10 %>% 
  group_by(Country) %>%
  filter(OsHV_var == "OsHV-1") %>% 
  reframe(count = n()) #14 countries

## Not OsHV-1
Spp_01_10 %>% 
  group_by(Country) %>%
  reframe(count = n()) #18 countries

## OsHV-1 type


unique(Spp_01_10$OsHV_var)
# [1] "OsHV-1"      "OsHV-1 μVar" "OsHV"        "AVNV"   


#### OsHV-1 coords ==============

glimpse(Spp_01_10)

OsHV1_coord <- Spp_01_10 %>% 
  filter(OsHV_var == "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

OsHV1_coord$GPS_lat <- as.double(OsHV1_coord$GPS_lat)
OsHV1_coord$GPS_lat

OsHV1_coord$GPS_long <- as.double(OsHV1_coord$GPS_long)
OsHV1_coord$GPS_long

### latitudes
Spp_OsHV1_lat <- OsHV1_coord$GPS_lat
Spp_OsHV1_lat

### longitudes
Spp_OsHV1long <- OsHV1_coord$GPS_long
Spp_OsHV1long

OsHV1Sites.df <- data.frame(
  lon = Spp_OsHV1long,
  lat = Spp_OsHV1_lat)

glimpse(OsHV1Sites.df)

#### OsHV-1 μVar data ==============
OsHV1Var_coord <- Spp_01_10 %>% 
  filter(OsHV_var == "OsHV-1 μVar",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1Var_coord$GPS_lat <- as.double(OsHV1Var_coord$GPS_lat)
OsHV1Var_coord$GPS_lat

OsHV1Var_coord$GPS_long <- as.double(OsHV1Var_coord$GPS_long)
OsHV1Var_coord$GPS_long


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

#### OsHV data ==============
OsHV_coord <- Spp_01_10 %>% 
  filter(OsHV_var == "OsHV",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV_coord$GPS_lat <- as.double(OsHV_coord$GPS_lat)
OsHV_coord$GPS_lat

OsHV_coord$GPS_long <- as.double(OsHV_coord$GPS_long)
OsHV_coord$GPS_long


### latitudes
Spp_OsHVlat <- OsHV_coord$GPS_lat
Spp_OsHVlat

### longitudes
Spp_OsHVlong <- OsHV_coord$GPS_long
Spp_OsHVlong

OsHVsites.df <- data.frame(
  lon = Spp_OsHVlong,
  lat = Spp_OsHVlat)

glimpse(OsHVsites.df)

#### AVNV data ==============
AVNV_coord <- Spp_01_10 %>% 
  filter(OsHV_var == "AVNV",
         !is.na(GPS_lat),
         !is.na(GPS_long))

AVNV_coord$GPS_lat <- as.double(AVNV_coord$GPS_lat)
AVNV_coord$GPS_lat

AVNV_coord$GPS_long <- as.double(AVNV_coord$GPS_long)
AVNV_coord$GPS_long


### latitudes
Spp_ANVNlat <- AVNV_coord$GPS_lat
Spp_ANVNlat

### longitudes
Spp_ANVNlong <- AVNV_coord$GPS_long
Spp_ANVNlong

AVNVSites.df <- data.frame(
  lon = Spp_ANVNlong,
  lat = Spp_ANVNlat)

glimpse(AVNVSites.df)


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
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = VarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()


## generate high quality ggmap() using geom_point() to generate markers

# satellite style Mgigas of California with Zoom
Detection_Mgigas <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Detection_Mgigas) +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 4) +
  geom_point(data = OsHVsites.df, aes(x = lon, y = lat), color = '#a700a7', alpha = 0.7,  size = 4) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#4DA8F9', alpha = 0.7,  size = 4)

# Tone-Lite Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 4) +
  geom_point(data = OsHVsites.df, aes(x = lon, y = lat), color = '#a700a7', alpha = 0.7,  size = 4) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#4DA8F9', alpha = 0.7,  size = 4) 

# Watercolor Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5) +
  geom_point(data = OsHVsites.df, aes(x = lon, y = lat), color = '#a700a7', alpha = 0.7,  size = 5) +
  geom_point(data = AVNVSites.df, aes(x = lon, y = lat), color = '#4DA8F9', alpha = 0.7,  size = 5)

######## 2011 - 2022 ==========================

Spp_11_22 <- Spp %>% 
  filter(Species != "Magallana gigas",
         Year_Sampled >= "2011" & Year_Sampled <= "2022")

View(Spp_11_22)

## OsHV-1
Spp_11_22 %>% 
  group_by(Country) %>%
  filter(OsHV_var == "OsHV-1") %>% 
  reframe(count = n()) #14 countries

## Not OsHV-1
Spp_11_22 %>% 
  group_by(Country) %>%
  reframe(count = n()) #18 countries

## OsHV-1 type


unique(Spp_11_22$OsHV_var)
# [1] "OsHV-1"      "OsHV-1 μVar"


#### OsHV-1 coords ==============

glimpse(Spp_11_22)

OsHV1_coord <- Spp_11_22 %>% 
  filter(OsHV_var == "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

OsHV1_coord$GPS_lat <- as.double(OsHV1_coord$GPS_lat)
OsHV1_coord$GPS_lat

OsHV1_coord$GPS_long <- as.double(OsHV1_coord$GPS_long)
OsHV1_coord$GPS_long

### latitudes
Spp_OsHV1_lat <- OsHV1_coord$GPS_lat
Spp_OsHV1_lat

### longitudes
Spp_OsHV1long <- OsHV1_coord$GPS_long
Spp_OsHV1long

OsHV1Sites.df <- data.frame(
  lon = Spp_OsHV1long,
  lat = Spp_OsHV1_lat)

glimpse(OsHV1Sites.df)

#### OsHV-1 μVar data ==============
OsHV1Var_coord <- Spp_11_22 %>% 
  filter(OsHV_var == "OsHV-1 μVar",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1Var_coord$GPS_lat <- as.double(OsHV1Var_coord$GPS_lat)
OsHV1Var_coord$GPS_lat

OsHV1Var_coord$GPS_long <- as.double(OsHV1Var_coord$GPS_long)
OsHV1Var_coord$GPS_long


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
get_googlemap(center = "Atlantic Ocean", zoom = 1, markers = VarSites.df, scale = 2,  maptype = "hybrid") %>% ggmap()


## generate high quality ggmap() using geom_point() to generate markers

# satellite style Mgigas of California with Zoom
Detection_Mgigas <- get_map("Atlantic Ocean", zoom =  1, maptype = "satellite")

ggmap(Detection_Mgigas) +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 4)

# Tone-Lite Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 4) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 4)

# Watercolor Mgigas
qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = OsHV1Sites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5)