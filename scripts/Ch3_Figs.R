###### Figures for Chapter 3

library(tidyverse)
library(gtsummary)


### load data
OsHV1 <- read_csv("data/OsHV1infections_30Jul2023.csv")
glimpse(OsHV1)

unique(OsHV1$Study_numb)
unique(OsHV1$Paper)
unique(OsHV1$Year_Published)
unique(OsHV1$Title)
unique(OsHV1$Country)
unique(OsHV1$GPS_lat)
unique(OsHV1$GPS_long)
unique(OsHV1$Taxa)
unique(OsHV1$Species)
unique(OsHV1$Year_Sampled)
unique(OsHV1$OsHV_var)

       
Detections <- OsHV1 %>% 
  select(Year_Sampled, Country, OsHV_var, Taxa) %>%
  group_by(Year_Sampled, OsHV_var) %>% 
  count(OsHV_var)

Detections


Detections.plot <- ggplot(aes(x = Year_Sampled, y = n, color = OsHV_var), data = Detections) +
  geom_jitter(size = 7) +
  scale_x_continuous(limits = c(1990, 2023), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023)) +
  scale_color_manual(values=c("#FF7F00", "#4DAF4A", "#FDAE61", "#4DA8F9", "#984EA3","#ABD9E9", "#D53E4F")) +
  theme_classic()

Detections.plot

#### ** PPT: OsHV-1 infections through time ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Detections.plot_fig <- read_pptx()
Detections.plot_fig <- add_slide(Detections.plot_fig , layout = "Title and Content", master = "Office Theme")
Detections.plot_fig <-  ph_with(x = Detections.plot_fig, value = Detections.plot, location = ph_location_fullsize() )
Detections.plot_fig  <- ph_with(x = Detections.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Detections.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Detections.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################


#### Number of studies per group ====

#### M. gigas ====
Mgigas <- OsHV1 %>% 
  filter(Taxa == "Pacific Oyster")

## No. papers
unique(Mgigas$Paper) # 79 

## Number Web of Science
Mgigas %>%
  filter(Search_Strategy != "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #34

## Number Snowball
Mgigas %>%
  filter(Search_Strategy == "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #46

#### Other spp. ====
Spp <- OsHV1 %>% 
  filter(Taxa != "Pacific Oyster")

## No. papers
unique(Spp$Paper) # 51

## Number Web of Science
Spp %>%
  filter(Search_Strategy != "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #21

## Number Snowball
Spp %>%
  filter(Search_Strategy == "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #30

#### Tables of studies ====

#### ALL ====
Prev.csv <- OsHV1 %>% 
  select(Study_numb, URL, Paper, Title, Year_Sampled, Species, OsHV_var) %>% 
  group_by(Paper) %>% 
  arrange(Study_numb) %>% 
  distinct() #%>% 
  #gt()

Prev.csv

write_csv(Prev.csv, "data/QuantifyingOsHV.csv")

#### M. gigas ====
Mgigas %>% 
  select(OsHV_var, Paper) %>% 
  arrange(Species) %>% 
  distinct() %>% 
  tbl_summary()
  

#### Other spp. ====

Spp %>% 
  select(Species, OsHV_var, Paper) %>% 
  group_by(Species) %>% 
  arrange(OsHV_var) %>% 
  distinct() %>% 
  gt()

##### XXX =========


