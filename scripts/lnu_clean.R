library(tidyr)
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(plyr)
library(readr)

TOT_species_LNU <- read.csv("data/raw/TOT_species_LNU.csv")
subplot_species_LNU <- read.csv("data/raw/SubPlot_SPP_LNU.csv")
plot.description_LNU <- read.csv("data/raw/PlotDescription_LNU.csv")
severity_LNU <- read.csv("data/raw/Severity_LNU.csv")
GroundCover_LNU <- read.csv("data/raw/GroundCover_LNU.csv")
Mortality_LNU <- read.csv("data/raw/Mortality_LNU.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_LNU.csv")

str(subplot_species_LNU)
subplot_species_LNU$Q1 = as.numeric(subplot_species_LNU$Q1)
subplot_species_LNU$Q2 = as.numeric(subplot_species_LNU$Q2)
subplot_species_LNU$Q3 = as.numeric(subplot_species_LNU$Q3)
subplot_species_LNU$Q4 = as.numeric(subplot_species_LNU$Q4)
subplot_species_LNU$Q5 = as.numeric(subplot_species_LNU$Q5)

#SEVERITY DATA

severity_LNU_byplot <- severity_LNU %>% 
  group_by(PlotID) %>% 
  dplyr::summarise(Mean_diam_cm = mean(Mean_diam)) %>% 
  mutate(mean_diam_mm = Mean_diam_cm *10)


plot.description.short <- plot.description_LNU %>% 
  filter(Quad..if.applicable. == "") %>% 
  dplyr::select(plotid, num_burn, cool.warm_slope) %>% 
  dplyr::rename(PlotID = plotid)
species.short <- SpeciesList %>% 
  dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 
severity.plotdescription.LNU <- left_join(severity_LNU_byplot, plot.description.short, by= "PlotID")

#AVERAGE OF ALL SPECIES
str(subplot_species_LNU)
str(LNU_subplot_wide)

#pivot wider & add zeros to species not found in entire plot 
LNU_subplot_wide <- subplot_species_LNU %>% 
  filter(cover_count_ht == "cover"|
           cover_count_ht == "") %>% 
  dplyr::select(PlotID, year, spp ,Q1, Q2, Q3, Q4, Q5)  %>% 
  mutate(across(c(Q1:Q5), ~ifelse(. == "TR" | . == "tr", "0.05", .))) %>% 
  mutate(across(c(Q1:Q5), ~as.numeric(.))) %>% 
  mutate(avg_Q = (Q1+Q2+Q3+Q4+Q5)/5) %>% 
  dplyr::select(PlotID, spp, avg_Q) %>% 
  pivot_wider(names_from = "spp", values_from = "avg_Q",
              values_fill = 0, values_fn = sum)




#NATIVE SPECIES
LNU_subplot_native <- subplot_species_LNU %>% 
  filter(cover_count_ht == "cover"|
           cover_count_ht == "") %>% 
  filter(Native_nonnative == "native") %>% 
  mutate(species_seed.resp = paste(spp, seedling_resp, sep ="_")) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, species_seed.resp, by=" ")) %>% 
  dplyr::select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  pivot_longer(!plot_spp, names_to = 'quad', values_to = 'cover') %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") 

LNU_subplot_native$cover[LNU_subplot_native$cover == "tr"] <- "0.05"
LNU_subplot_native$cover[LNU_subplot_native$cover == "TR"] <- "0.05"

str(LNU_subplot_native)
LNU_subplot_native$cover <- as.numeric(LNU_subplot_native$cover)

LNU_sub_native_wide <- LNU_subplot_native %>% 
  mutate(plot_yr_quad = paste(plot_year, quad, sep=" ")) %>% 
  select(plot_yr_quad, spp, cover) %>% 
  pivot_wider(names_from="spp",
              values_from="cover",
              values_fill = 0, values_fn = sum)

write.csv(LNU_subplot_native,"data/clean/LNU_subplot_native_long.csv" )
write.csv(LNU_sub_native_wide,"data/clean/LNU_subplot_native_wide.csv" )

#NONNATIVE SPECIES
LNU_subplot_nonnative <- subplot_species_LNU %>% 
  filter(cover_count_ht == "cover"|
           cover_count_ht == "") %>% 
  filter(Native_nonnative == "non-native"|
           Native_nonnative == "invasive non-native") %>% 
  mutate(species_seed.resp = paste(spp, seedling_resp, sep ="_")) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, species_seed.resp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  pivot_longer(!plot_spp, names_to = 'quad', values_to = 'cover') %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") 

LNU_subplot_nonnative$cover[LNU_subplot_nonnative$cover == "tr"] <- "0.05"
LNU_subplot_nonnative$cover[LNU_subplot_nonnative$cover == "TR"] <- "0.05"

str(LNU_subplot_nonnative)
LNU_subplot_nonnative$cover <- as.numeric(LNU_subplot_nonnative$cover)

LNU_sub_nonnative_wide <- LNU_subplot_nonnative %>% 
  mutate(plot_yr_quad = paste(plot_year, quad, sep=" ")) %>% 
  select(plot_yr_quad, spp, cover) %>% 
  pivot_wider(names_from="spp",
              values_from="cover",
              values_fill = 0, values_fn = sum)

write.csv(LNU_subplot_nonnative,"data/clean/LNU_subplot_nonnative_long.csv" )
write.csv(LNU_sub_nonnative_wide,"data/clean/LNU_subplot_nonnative_wide.csv" )

#SHRUB DENSITY


shrub_density_long <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COUNT") %>% 
  mutate(species_seed.resp = paste(spp, seedling_resp, sep ="_")) %>%   
  mutate(plot_year = paste(PlotID, year, sep="_")) %>%  
  mutate(plot_spp = paste(plot_year, species_seed.resp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(avg_count = (Q1+Q2+Q3+Q4+Q5)/5) %>% 
  mutate(count_ha = avg_count/0.0001) %>% 
  select(plot_spp, count_ha) %>% 
  separate(plot_spp, c("plot_year", "spp.seed.resp"), sep=" ") 

str(shrub_density_long)


shrub_density_wide <- shrub_density_long %>% 
  pivot_wider(
    names_from = "spp.seed.resp",
    values_from = "count_ha",
    values_fill = 0,
    values_fn = sum
  )  
str(shrub_density_wide)

shrub_density_long <- shrub_density_wide %>% 
  pivot_longer(!plot_year,
               names_to = "spp", 
               values_to = "density_ha") %>% 
  separate(plot_year, c("PlotID","year"),sep="_") %>% 
  separate(spp, c("spp","seed.resp"),sep="_")

write.csv(shrub_density_wide, "data/clean/LNU_subplot_shrub_density_wide.csv")
write.csv(shrub_density_long, "data/clean/LNU_subplot_shrub_density_long.csv")

#SHRUB COVER
shrub_cover_long <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COVER") %>% 
  mutate(species_seed.resp = paste(spp, seedling_resp, sep ="_")) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, species_seed.resp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
  mutate_all(na_if,"") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  pivot_longer(!plot_spp, names_to = 'quad', values_to = 'cover') %>% 
  separate(plot_spp, c("plot_year", "spp.seed.resp"), sep=" ") 

str(shrub_cover_long)
shrub_cover_long$cover[shrub_cover_long$cover == "tr"] <- "0.05"
shrub_cover_long$cover[shrub_cover_long$cover == "TR"] <- "0.05"
shrub_cover_long$cover <- as.numeric(shrub_cover_long$cover)

shrub_cover_plot <- shrub_cover_long %>% 
  group_by(plot_year, spp.seed.resp) %>% 
  dplyr::summarise(cover = mean(cover)) %>% 
  separate(spp.seed.resp, c("spp", "seed.resprout"), sep="_") %>% 
  separate(plot_year, c("PlotID", "year"), sep="_") 

write.csv(shrub_cover_plot, "data/clean/LNU_subplot_shrub_cover.csv")
    


    
    