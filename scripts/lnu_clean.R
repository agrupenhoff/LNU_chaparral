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
SpeciesList <- read.csv("data/raw/SpeciesList_jan23.csv")


str(subplot_species_LNU)
subplot_species_LNU$Q1 = as.numeric(subplot_species_LNU$Q1)
subplot_species_LNU$Q2 = as.numeric(subplot_species_LNU$Q2)
subplot_species_LNU$Q3 = as.numeric(subplot_species_LNU$Q3)
subplot_species_LNU$Q4 = as.numeric(subplot_species_LNU$Q4)
subplot_species_LNU$Q5 = as.numeric(subplot_species_LNU$Q5)
subplot_species_LNU$seedling_resp = toupper(subplot_species_LNU$seedling_resp)
subplot_species_LNU$cover_count_ht = toupper(subplot_species_LNU$cover_count_ht)

#SEVERITY DATA
severity_LNU_byplot <- severity_LNU %>% 
  group_by(PlotID) %>% 
  dplyr::summarise(Mean_diam_cm = mean(Mean_diam)) %>% 
  mutate(mean_diam_mm = Mean_diam_cm *10)

str(plot.description_LNU)
plot.description.short <- plot.description_LNU %>% 
  filter(Quad..if.applicable. == "") %>% 
  dplyr::select(PlotID, Location, num_burn, cool.warm_slope,
                fire_preLNU_year, currentFRI, medianRefFRI, meanPFRID,
                meanCC_FRI, PFR) 

severity.plotdescription.LNU <- left_join(plot.description.short,severity_LNU_byplot, by= "PlotID")

write.csv(plot.description.short, "data/clean/PlotDescription_clean.csv")
write.csv(severity.plotdescription.LNU,"data/clean/LNU_severity.plotdescription.csv" )


species.short <- SpeciesList %>% 
  dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 


#add species descriptions to data
subplot_species_LNU <- left_join(subplot_species_LNU, species.short,
                                 by = "spp")



str(subplot_species_LNU)
#file for NMDS

LNU_spp_totalSH <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COVER") %>% 
  mutate_if(is.numeric, ~replace_na(., 0) ) %>% 
  group_by(PlotID, year, spp) %>% 
  dplyr::summarise(across(c(Q1,Q2,Q3,Q4,Q5),sum),
            .groups = 'drop')


LNU_species_all_wide <- subplot_species_LNU %>% 
  filter(cover_count_ht == "") %>% 
  select(PlotID, year, spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0) ) %>% 
  rbind(LNU_spp_totalSH) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  dplyr::select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate(sum_5m2 = Q1+Q2+Q3+Q4+Q5) %>% 
  select(plot_spp, sum_5m2) %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") 

LNU_species_all_wide_final <- LNU_species_all_wide %>% 
  pivot_wider(names_from="spp",
              values_from="sum_5m2",
              values_fill = 0, values_fn = sum)
write.csv(LNU_species_all_wide_final,"data/clean/LNU_species_all_wide.csv" )


#NATIVE SPECIES
LNU_spp_totalSH <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COVER") %>% 
  filter(Native_nonnative == "native") %>% 
  mutate_if(is.numeric, ~replace_na(., 0) ) %>% 
  group_by(PlotID, year, spp) %>% 
  dplyr::summarise(across(c(Q1,Q2,Q3,Q4,Q5),sum),
                   .groups = 'drop')

LNU_subplot_native <- subplot_species_LNU %>% 
  filter(cover_count_ht == "") %>% 
  filter(Native_nonnative == "native") %>% 
  select(PlotID, year, spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0) ) %>% 
  rbind(LNU_spp_totalSH) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  dplyr::select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>%
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
  filter(cover_count_ht == "") %>% 
  filter(Native_nonnative == "non-native"|
           Native_nonnative == "invasive non-native") %>% 
  select(PlotID, year, spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0) ) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  dplyr::select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  pivot_longer(!plot_spp, names_to = 'quad', values_to = 'cover') %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") 


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

#make data frame with every combination of plot x shrub spp combination
sh_spp <- subplot_species_LNU %>% filter(Lifeform == "SH")
unique(sh_spp$spp)
nd <- expand_grid(PlotID =unique(subplot_species_LNU$PlotID),
                  spp =unique(sh_spp$spp),
                  year=unique(subplot_species_LNU$year))
str(nd)
nd$year <- as.numeric(nd$year)

#Shrub den - avg and sum count
str(shrub_density_long)
shrub_density_long <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COUNT",
         seedling_resp == "SEEDLING") %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>%  
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(avg.count = (Q1+Q2+Q3+Q4+Q5)/5,
         sum.count = Q1+Q2+Q3+Q4+Q5) %>% 
  select(plot_spp, avg.count, sum.count) %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
  separate(plot_year, c("PlotID", "year"), sep="_") 

shrub_density_long$year <- as.numeric(shrub_density_long$year)
#now add to nd to account for 0s
shrub_density_zeros <- left_join(nd, shrub_density_long, 
                                 by=c("PlotID","spp","year"))
shrub_density_final <- shrub_density_zeros %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 

write.csv(shrub_density_final, "data/clean/shrub_density_count.csv")

#SHRUB DENSITY - proportion of presence 
shrub_density_presence <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COUNT",
         seedling_resp == "SEEDLING") %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>%  
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  pivot_longer(!plot_spp, 
               names_to = 'quad',
               values_to = 'density') %>% 
  mutate(presence = case_when(
    density == 0 ~ 0,
    density > 0 ~ 1
  )) %>% 
  group_by(plot_spp) %>% 
  dplyr::summarise(presence = sum(presence),
            prop.presence = presence/5) %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
  separate(plot_year, c("PlotID", "year"), sep="_")

str(shrub_density_presence)
shrub_density_presence$year <- as.numeric(shrub_density_presence$year)
#now add to nd to account for 0s
shrub_presence_zeros <- left_join(nd, shrub_density_presence, 
                                 by=c("PlotID","spp","year"))

shrub_presence_final <- shrub_presence_zeros %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 
write.csv(shrub_presence_final, "data/clean/shrub_density_presence.csv")

#SHRUB COVER
shrub_cover_long <- subplot_species_LNU %>% 
  filter(cover_count_ht == "COVER",
         Lifeform == "SH",
         seedling_resp == "SEEDLING") %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(avg.cover = (Q1+Q2+Q3+Q4+Q5)/5,
         sum.cover = Q1+Q2+Q3+Q4+Q5) %>% 
  select(plot_spp, avg.cover, sum.cover) %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
  separate(plot_year, c("PlotID", "year"), sep="_")

str(shrub_cover_long)
shrub_cover_long$year <- as.numeric(shrub_cover_long$year)
#now add to nd to account for 0s
shrub_cover_zeros <- left_join(nd, shrub_cover_long, 
                                  by=c("PlotID","spp","year"))

shrub_cover_final <- shrub_cover_zeros %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 

write.csv(shrub_cover_final, "data/clean/shrub_seedling_cover.csv")
    
shrub_seedling_data <- left_join(shrub_density_final, shrub_presence_final,
                                 by=c("PlotID", "year", "spp"))
shrub_seedling_data <- left_join(shrub_seedling_data, shrub_cover_final,
                                 by=c("PlotID", "year", "spp"))

write.csv(shrub_seedling_data, "data/clean/shrub_seedling_data_ALL.csv")


#SHRUB RESPROUT HEIGHT??

shrub_resp_ht <- subplot_species_LNU %>% 
  filter(cover_count_ht == "HEIGHT",
         Lifeform == "SH",
         seedling_resp == "RESPROUT") %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
  rowwise() %>% 
 dplyr::mutate(mean.ht = mean(c_across(starts_with("Q")),na.rm = TRUE)) %>% 
  select(plot_spp, mean.ht) %>% 
  separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
  separate(plot_year, c("PlotID", "year"), sep="_")

str(shrub_resp_ht)
shrub_resp_ht$year <- as.numeric(shrub_resp_ht$year)

write.csv(shrub_resp_ht, "data/clean/shrub_resprout_height_ALL.csv")
