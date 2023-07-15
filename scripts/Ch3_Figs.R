###### Figures for Chapter 3

library(tidyverse)




### load data
OsHV1 <- read_csv("data/OsHV1infections_14Jul2023.csv")
glimpse(OsHV1)

unique(OsHV1$Taxa)

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

#### M. gigas ====
Mgigas <- OsHV1 %>% 
  filter(Taxa == "Pacific Oyster")

#### Other spp. ====
