######### Global Mgigas of OsHV-1 Detections ================================

######### PRESENTATION-READY Mgigas ==============================
library(tidyverse)
library(ggplot2)
library(ggMgigas)
library(Mgigass)
library(Mgigasdata)
library(Mgigastools) ##scalebar
library(ggsn) ##scale bar: http://oswaldosantos.github.io/ggsn/ ; 

## ggMgigas intro: https://appsilon.com/r-ggMgigas/

#### load data ==============
OsHV1 <- read_csv("data/OsHV1infections_14Jul2023.csv")
glimpse(OsHV1)

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
  filter(OsHV_var != "OsHV-1") %>% 
  reframe(count = n()) #17 countries

## OsHV-1 type

Country.Plot <-  Mgigas %>%
  group_by(Country, OsHV_var) %>% 
  reframe(count = n()) %>% 
  ggplot(aes(x = Country, y = count, fill = OsHV_var, group = OsHV_var)) +
  geom_bar(stat = "identity", position = position_dodge()) +
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


######## Mgigas of OsHV-1 Detections ==========================

#### OsHV-1 coords ==============

glimpse(Mgigas)

OsHV1_coord <- Mgigas %>% 
  filter(!is.na(GPS_lat),
         !is.na(GPS_long))

glimpse(OsHV1_coord)

### latitudes
Mgigas_nonVarlat <- OsHV1_coord$GPS_lat
Mgigas_nonVarlat

### longitudes
Mgigas_nonVarlong <- OsHV1_coord$GPS_long
Mgigas_nonVarlong

nonVarSites.df <- data.frame(
  lon = Mgigas_nonVarlong,
  lat = Mgigas_nonVarlat)



glimpse(nonVarSites.df)

#### OsHV-1 uvar data ==============
OsHV1Var_coord <- Mgigas %>% 
  filter(OsHV1_var != "OsHV-1",
         !is.na(GPS_lat),
         !is.na(GPS_long))

OsHV1Var_coord$GPS_lat <- as.double(OsHV1Var_coord$GPS_lat)
OsHV1Var_coord$GPS_lat

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

nonVarSites.df

#load a googleMgigas 
get_googleMgigas(center = "Atlantic Ocean", zoom = 1, markers = nonVarSites.df, scale = 2,  Mgigastype = "hybrid") %>% ggMgigas()

## generate high quality Mgigass using geom_point() to generate markers

# satellite style Mgigas of California with Zoom
Detection_Mgigas <- get_Mgigas("Atlantic Ocean", zoom =  1, Mgigastype = "satellite")

ggMgigas(Detection_Mgigas) +
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5)
#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.05, nudge_y = 0.006, hjust = 1)

# Tone-Lite Mgigas
qMgigas("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", Mgigastype = "toner-lite") +
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = '#FFDB58', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = '#D53E4F', alpha = 0.7,  size = 5)
#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

# Watercolor Mgigas
qMgigas("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", Mgigastype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = nonVarSites.df, aes(x = lon, y = lat), color = 'purple', alpha = 0.7,  size = 5) +
  geom_point(data = VarSites.df, aes(x = lon, y = lat), color = 'red', alpha = 0.7,  size = 5)

#geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.015, nudge_y = 0.006, hjust = 1)

TB_watercolor


