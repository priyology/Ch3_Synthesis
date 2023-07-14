## Meta-Analysis Presentation

library(tidyverse)
library(ggthemes)

#CHANGES

data <- read.csv('data/Ch3_MetaAnalysis_01June2021.csv')

## remove empty rows
data <- data[-c(61:78),]

View(data)

## innoculation methodologies (# rows, not # individual papers)
data.method <- data %>%
  group_by(OsHV.1.innoculation.method) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = OsHV.1.innoculation.method, y = count, fill = OsHV.1.innoculation.method)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(legend.position = "none")
    
data.method

## year study was published
## <! -- Need to look at year(s) study was done -->
data.year <- data %>%
  group_by(Year) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = Year, y = count, fill = Year)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0, 15)) +
  theme_classic() +
  theme(legend.position = "none")
  

data.year

## degree days / thermal accumulation
install.packages("devtools")
devtools::install_github(build_vignettes = TRUE,repo = "trenchproject/TrenchR")


vignette("TeTutorial", package="TrenchR")

library(TrenchR)

degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.sine")
degree_days(T_min=7, T_max=14, LDT=12, UDT=33, method="single.triangulation")

# See 'DegreeDaysCode_TrenchR.R for this to work since package won't install

df <- read.delim(source("degree"))

## location of study

library(ggmap)

#get API key @ https://developers.google.com/places/web-service/get-api-key
register_google(key = "AIzaSyC0Zqe8HRYsP4TIFaJU0_yfa1VSCMLhPpc",  write = TRUE)
has_google_key() #checks key's existence

#coordinates for some sites
countries.OsHV <- data.frame(
  country = c("Spain", "China", "Italy", "France", "Australia", "USA", "Brazil", "Ireland"),
  longitude = c(40.722828, 36.112516, 40.233469, 48.337963, -33.554289, 38.172259, -27.493282, 55.159766),
  latitude = c(0.872172, 120.562580, 18.451638, -4.442585, 151.303541, -122.933771, -48.527304, -7.127637)
)

#define map source / type / color
BerkeleyCA <- c(longitude = -122.29072179363794, latitude = 37.881735838803564) #Berkeley, CA

myMap <- get_map(location = BerkeleyCA, zoom = 3, source = "google", maptype = "hybrid", crop = FALSE)

get_map()


#### SPARE CODE $$$$

#omit NAs from dataset
data.omit <- na.omit(data)
View(data.omit)

#### ================== ####

## MAP DIDN'T WORK RIGHT B/C NEED GOOGLE API KEY ##

#### ================== ####

## location of studies

library(tidyverse)
library(rvest)
library(magrittr)
library(maps)
library(ggmap)
library(stringr)

## GET WORLD MAP ##

View(map.world)

map.world <- map_data("world")
countries.OsHV <- data.frame(
  country = c("Spain", "China", "Italy", "France", "Australia", "USA", "Brazil", "Ireland"),
  longitude = c(40.722828, 36.112516, 40.233469, 48.337963, -33.554289, 38.172259, -27.493282, 55.159766),
  latitude = c(0.872172, 120.562580, 18.451638, -4.442585, 151.303541, -122.933771, -48.527304, -7.127637)
)

##Join both datasets: 

map.world_joined <- left_join(map.world, countries.OsHV, by = c('region' = 'country'))

head(map.world_joined)

geocode.country_points <- geocode(countries.OsHV$country)

#=======================================================
# CREATE POINT LOCATIONS FOR SINGAPORE AND LUXEMBOURG
# - Luxembourg and Singapore are countries with
#   high 'talent competitiveness'
# - But, they are both small on the map, and hard to see
# - We'll create points for each of these countries
#   so they are easier to see on the map
#=======================================================
#df.country_points <- data.frame(country = c("Singapore","Luxembourg"),stringsAsFactors = F)
#glimpse(df.country_points)