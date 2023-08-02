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

#### Number of studies per group ====

#### M. gigas ====
Mgigas <- OsHV1 %>% 
  filter(Taxa == "Pacific Oyster")

## No. papers
unique(Mgigas$Paper) # 75 

## Number Web of Science
Mgigas %>%
  filter(Search_Strategy != "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #32

## Number Snowball
Mgigas %>%
  filter(Search_Strategy == "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #43

#### Other spp. ====
Spp <- OsHV1 %>% 
  filter(Taxa != "Pacific Oyster")

## No. papers
unique(Spp$Paper) # 51

## Number Web of Science
Spp %>%
  filter(Search_Strategy != "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #20

## Number Snowball
Spp %>%
  filter(Search_Strategy == "Snowball") %>% 
  count(Search_Strategy, Paper) %>% 
  nrow() #30

#### Tables of studies ====

#### ALL ====
Int.csv <- OsHV1 %>% 
  select(Study_numb, URL, Paper, Title, Year_Sampled, Species, OsHV_var) %>% 
  group_by(Paper) %>% 
  arrange(Study_numb) %>% 
  distinct() 

Int.csv

write_csv(Int.csv, "data/QuantifyingOsHV.csv")

Table_Papers <- OsHV1 %>% 
  select(Year_Sampled, Species, OsHV_var, Paper, Title) %>% 
  arrange(Year_Sampled) %>% 
  distinct() 

Table_Papers

write_csv(Table_Papers, "data/AppendixA.csv")

#### Detections by Year & Variant ====       
DetectionsA <- OsHV1 %>% 
  select(Year_Sampled, Country, OsHV_var, Taxa) %>%
  group_by(Year_Sampled, OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsA)

DetectionsA.plot <- ggplot(aes(x = Year_Sampled, y = freq, color = OsHV_var), data = DetectionsA) +
  geom_point(position = "jitter", size = 4) +
  scale_x_continuous(limits = c(1990, 2023), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023)) +
  scale_color_manual(values=c("#4DA8F9", "#4DAF4A", "#00fa9a", "#a700a7", "#FFDB58","#FF7F00", "#D53E4F")) +
  theme_classic()

DetectionsA.plot

#### ** PPT: OsHV-1 infections through time ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
DetectionsA.plot_fig <- read_pptx()
DetectionsA.plot_fig <- add_slide(DetectionsA.plot_fig , layout = "Title and Content", master = "Office Theme")
DetectionsA.plot_fig <-  ph_with(x = DetectionsA.plot_fig, value = DetectionsA.plot, location = ph_location_fullsize() )
DetectionsA.plot_fig  <- ph_with(x = DetectionsA.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(DetectionsA.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = DetectionsA.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

#### Detections by Country and Variant =======
DetectionsB <- OsHV1 %>% 
  select(Year_Sampled, Country, OsHV_var, Taxa) %>%
  group_by(Country, OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsB)

#### Detections by Country =======

DetectionsC <- OsHV1 %>% 
  select(Year_Sampled, Country, OsHV_var, Taxa) %>%
  group_by(Country) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsC)

#### Detections by Species =======

DetectionsD <- OsHV1 %>% 
  select(Species, OsHV_var) %>%
  group_by(Species) %>%
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsD)

#### Detections by Variant =======

DetectionsE <- OsHV1 %>% 
  select(OsHV_var) %>%
  group_by(OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsE)


#### Detections in M.gigas ONLY =======

#### M. gigas + Variant ====

DetectionsF <- OsHV1 %>%
  filter(Species == "Magallana gigas") %>% 
  select(OsHV_var) %>%
  group_by(OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsF)

#### M. gigas + Country ====

DetectionsG <- OsHV1 %>%
  filter(Species == "Magallana gigas") %>% 
  select(Country, OsHV_var) %>%
  group_by(Country) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsG)

#### M. gigas + Country + Variant ====

DetectionsH <- OsHV1 %>%
  filter(Species == "Magallana gigas") %>% 
  select(Country, OsHV_var) %>%
  group_by(Country, OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsH)


#### Detections NOT in M.gigas =======

#### Other Spp. + Variant ====

DetectionsI <- OsHV1 %>%
  filter(Species != "Magallana gigas") %>% 
  select(OsHV_var) %>%
  group_by(OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsI)

#### Other spp. + Species ====

DetectionsJ <- OsHV1 %>%
  filter(Species != "Magallana gigas") %>% 
  select(Species, OsHV_var) %>%
  group_by(Species) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsJ)

#### Other spp. + Country ====

DetectionsK <- OsHV1 %>%
  filter(Species != "Magallana gigas") %>% 
  select(Country, OsHV_var) %>%
  group_by(Country) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsK)

#### Other spp. + Country + Variant ====

DetectionsL <- OsHV1 %>%
  filter(Species != "Magallana gigas") %>% 
  select(Species, Country, OsHV_var) %>%
  group_by(Country, OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(DetectionsL)


#### PREVALENCE ====

glimpse(OsHV1)


## Freq Data

Freq_Study <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(Study_numb, Prevalence) %>%
  group_by(Study_numb) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(Freq_Study)

### Proportion of studies without Prev Data
(116-64)/116 #0.44827

Freq_Spp <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(Species, Prevalence) %>%
  group_by(Species) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(Freq_Spp)

Freq_Country <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(Country, Prevalence) %>%
  group_by(Country) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(Freq_Country)

Freq_Var <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(OsHV_var, Prevalence) %>%
  group_by(OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(Freq_Var)


## Prev Data
Prev_Var <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(OsHV_var, Prevalence) %>%
  group_by(OsHV_var) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_Var)

Prev_SppVar <- OsHV1 %>%
  filter(Prevalence != "NA",
         Species != "NA") %>% 
  select(Species, OsHV_var, Prevalence) %>%
  group_by(Species, OsHV_var) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_SppVar)

Prev_Spp <- OsHV1 %>%
  filter(Prevalence != "NA") %>% 
  select(Species, Prevalence) %>%
  group_by(Species) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_Spp)

Prev_SppCountry <- OsHV1 %>%
  filter(Prevalence != "NA",
         Country != "NA") %>% 
  select(Species, Country, OsHV_var, Prevalence) %>%
  group_by(Species, Country, OsHV_var) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_SppCountry)

Prev_SppCountry.plot <- ggplot(aes(x = Country, y = mean_prev, group = OsHV_var, fill = OsHV_var), data = Prev_SppCountry) +
  facet_wrap(Species~.) +
  geom_col(position=position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean_prev-SE_prev, ymax = mean_prev + SE_prev), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#4DAF4A", "#00fa9a", "#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip()

Prev_SppCountry.plot



Prev_Country <- OsHV1 %>%
  filter(Prevalence != "NA",
         Country != "NA") %>% 
  select(Country, OsHV_var, Prevalence) %>%
  group_by(Country, OsHV_var) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_Country)

Prev_Country.plot <- ggplot(aes(x = Country, y = mean_prev, group = OsHV_var, fill = OsHV_var), data = Prev_Country) +
  facet_wrap(OsHV_var~.) +
  geom_col(position=position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean_prev-SE_prev, ymax = mean_prev + SE_prev), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#4DAF4A", "#00fa9a", "#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip()

Prev_Country.plot

#### ** PPT: Prevalence - Countries ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Prev_Country.plot_fig <- read_pptx()
Prev_Country.plot_fig <- add_slide(Prev_Country.plot_fig , layout = "Title and Content", master = "Office Theme")
Prev_Country.plot_fig <-  ph_with(x = Prev_Country.plot_fig, value = Prev_Country.plot, location = ph_location_fullsize() )
Prev_Country.plot_fig  <- ph_with(x = Prev_Country.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Prev_Country.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Prev_Country.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

Prev_SppVar <- OsHV1 %>%
  filter(Prevalence != "NA",
         Species != "NA") %>% 
  select(Species, OsHV_var, Prevalence) %>%
  group_by(Species, OsHV_var) %>% 
  reframe(mean_prev = mean(Prevalence),
          SD_prev = sd(Prevalence),
          SE_prev = SD_prev/sqrt(n())) %>% 
  arrange(mean_prev)

View(Prev_SppVar)

Prev_SppVar.plot <- ggplot(aes(x = Species, y = mean_prev, group = OsHV_var, fill = OsHV_var), data = Prev_SppVar) +
  facet_wrap(OsHV_var~.) +
  geom_col(position=position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean_prev-SE_prev, ymax = mean_prev + SE_prev), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#4DAF4A", "#00fa9a", "#a700a7", "#FFDB58", "#D53E4F")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip()

Prev_SppVar.plot


#### INTENSITY ====

glimpse(OsHV1)

## Freq Data

FreqI_Study <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(Study_numb, Intensity) %>%
  group_by(Study_numb) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(FreqI_Study)

### Proportion of studies without Intensity Data
(116-42)/116 #0.637931

FreqI_Spp <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(Species, Prevalence) %>%
  group_by(Species) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(FreqI_Spp)

FreqI_Country <- OsHV1 %>%
  filter(Intensity != "NA") %>%  
  select(Country, Prevalence) %>%
  group_by(Country) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(FreqI_Country)

FreqI_Var <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(OsHV_var, Prevalence) %>%
  group_by(OsHV_var) %>% 
  reframe(n=n()) %>% 
  mutate(freq = n / sum(n))

View(FreqI_Var)

## Intensity Data
Int_Var <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(OsHV_var, Intensity) %>%
  group_by(OsHV_var) %>% 
  reframe(mean_Int = mean(Intensity),
          SD_Int = sd(Intensity),
          SE_Int = SD_Int/sqrt(n())) %>% 
  arrange(mean_Int)

View(Int_Var)

Int_SppVar <- OsHV1 %>%
  filter(Intensity != "NA",
         OsHV_var != "OsHV") %>% 
  select(Species, OsHV_var, Intensity) %>%
  group_by(Species, OsHV_var) %>% 
  reframe(mean_Int = mean(Intensity),
          SD_Int = sd(Intensity),
          SE_Int = SD_Int/sqrt(n())) %>% 
  arrange(mean_Int)

View(Int_SppVar)

Int_SppVar.plot <- ggplot(aes(x = Species, y = mean_Int, group = OsHV_var, fill = OsHV_var), data = Int_SppVar) +
  facet_wrap(OsHV_var~.) +
  geom_col(position=position_dodge2(preserve = "single"))+
  geom_errorbar(aes(ymin = mean_Int-SE_Int, ymax = mean_Int + SE_Int), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FFDB58", "#D53E4F")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip()

Int_SppVar.plot


Int_Spp <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(Species, Intensity) %>%
  group_by(Species) %>% 
  reframe(mean_Int = mean(Intensity),
          SD_Int = sd(Intensity),
          SE_Int = SD_Int/sqrt(n())) %>% 
  arrange(mean_Int)

View(Int_Spp)

Int_Country <- OsHV1 %>%
  filter(Intensity != "NA",
         OsHV_var != "OsHV") %>% 
  select(Country, OsHV_var, Intensity) %>%
  group_by(Country, OsHV_var) %>% 
  reframe(mean_Int = mean(Intensity),
          SD_Int = sd(Intensity),
          SE_Int = SD_Int/sqrt(n())) %>% 
  arrange(mean_Int)

View(Int_Country)

Int_Country.plot <- ggplot(aes(x = Country, y = mean_Int, group = OsHV_var, fill = OsHV_var), data = Int_Country) +
  facet_wrap(OsHV_var~.) +
  geom_col(position=position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean_Int-SE_Int, ymax = mean_Int + SE_Int), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FFDB58", "#D53E4F")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

Int_Country.plot

#### ** PPT: Intensity - Countries ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Int_Country.plot_fig <- read_pptx()
Int_Country.plot_fig <- add_slide(Int_Country.plot_fig , layout = "Title and Content", master = "Office Theme")
Int_Country.plot_fig <-  ph_with(x = Int_Country.plot_fig, value = Int_Country.plot, location = ph_location_fullsize() )
Int_Country.plot_fig  <- ph_with(x = Int_Country.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Int_Country.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Int_Country.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################


Int_SppCountry <- OsHV1 %>%
  filter(Intensity != "NA") %>% 
  select(Species, Country, OsHV_var, Intensity) %>%
  group_by(Species, Country, OsHV_var) %>% 
  reframe(mean_Int = mean(Intensity),
          SD_Int = sd(Intensity),
          SE_Int = SD_Int/sqrt(n())) %>% 
  arrange(mean_Int)

View(Int_SppCountry)

Int_SppCountry.plot <- ggplot(aes(x = Country, y = mean_Int, group = OsHV_var, fill = OsHV_var), data = Int_SppCountry) +
  facet_grid(OsHV_var~Species) +
  geom_col(position=position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean_Int-SE_Int, ymax = mean_Int + SE_Int), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FFDB58", "#D53E4F", "#a700a7")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

Int_SppCountry.plot



#### Appendix of all papers ====
OsHV1 %>% 
  select(Study_numb, URL, Paper, Title, Year_Sampled, Species, OsHV_var) %>% 
  group_by(Paper) %>% 
  arrange(Paper) %>% 
  as_gt() %>% 
  gt::gtsave(filename = "data/Appendix.docx")

##### XXX =========