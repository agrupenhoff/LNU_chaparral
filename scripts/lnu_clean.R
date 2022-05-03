library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(plyr)

TOT_species_LNU <- read.csv("data/raw/TOT_species_LNU.csv")
subplot_species_LNU <- read.csv("data/raw/SubPlot_SPP_LNU.csv")
plot.description_LNU <- read.csv("data/raw/PlotDescription_LNU.csv")
severity_LNU <- read.csv("data/raw/Severity_LNU.csv")
GroundCover_LNU <- read.csv("data/raw/GroundCover_LNU.csv")
Mortality_LNU <- read.csv("data/raw/Mortality_LNU.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_LNU.csv")


plot.description.short <- plot.description_LNU %>% 
  filter(Quad..if.applicable. == "") %>% 
  select(PlotID, num_burn, cool.warm_slope)
species.short <- SpeciesList %>% 
  select(Species, Lifeform, Native_nonnative)

LNU_species_join <- left_join(TOT_species_LNU, plot.description.short, by= "PlotID")
LNU_species_join <- left_join(LNU_species_join, species.short, by= "Species")

LNU_species <- LNU_species_join %>% 
  mutate(plotid_time=paste(PlotID, num_burn, cool.warm_slope, year, sep= " ")) %>% 
  mutate(species_type=paste(Species, SEEDLING.RESPROUT, sep="_")) %>% 
  add_count(plotid_time, name="n_species") %>% 
  add_count(plotid_time, Native_nonnative, name="n_species_native.non") %>% 
  add_count(plotid_time, Lifeform, name="n_species_lifeform")

str(LNU_species)
LNU_species$num_burn <- as.factor(LNU_species$num_burn)

LNU_richness <- ddply(LNU_species,~plotid_time,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))})

ggplot(LNU_species, aes(x = num_burn, y = n_species, colour = cool.warm_slope)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) 

ggplot(LNU_species, aes(x = num_burn, y = n_species_native.non, colour = Native_nonnative)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) 

##########SUBPLOT
LNU_subplot <- subplot_species_LNU %>% 
  mutate_if(is.numeric, ~replace_na(., 0))


##WIDER???
  select(plotid_time, species_type) 
  pivot_wider(id_cols="plotid_time", names_from="species",
              values_from="percent",
              values_fill = 0,
              values_fn=sum)

glimpse(springs_species)

export(springs_species,"SiteLocation/Springs_Fire/data/clean/springs_species.csv" )
