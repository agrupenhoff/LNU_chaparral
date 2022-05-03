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
  select(plotid, num_burn, cool.warm_slope)
species.short <- SpeciesList %>% 
  select(spp, Lifeform, Native_nonnative, fac.obl) 

LNU_species_join <- left_join(TOT_species_LNU, plot.description.short, by= "PlotID")
LNU_species_join <- left_join(LNU_species_join, species.short, by= "spp")

LNU_species <- LNU_species_join %>% 
  mutate(plotid_time=paste(PlotID, num_burn, cool.warm_slope, year, sep= " ")) %>% 
  mutate(species_type=paste(Species, SEEDLING.RESPROUT, sep="_")) %>% 
  add_count(plotid_time, name="n_species") %>% 
  add_count(plotid_time, Native_nonnative, name="n_species_native.non") %>% 
  add_count(plotid_time, Lifeform, name="n_species_lifeform")

str(LNU_species)
LNU_species$num_burn <- as.numeric(LNU_species$num_burn)

ggplot(LNU_species, aes(x = num_burn, y = n_species, colour = cool.warm_slope)) +
  geom_smooth(method = "lm")
ggplot(LNU_species, aes(x = num_burn, y = n_species, colour = Native_nonnative)) +
  geom_smooth(method = "lm")+
  facet_wrap(~ Lifeform)


##########SUBPLOT
subplot_species_LNU$seedling_resp <- tolower(subplot_species_LNU$seedling_resp)
subplot_species_LNU$cover_count_ht <- tolower(subplot_species_LNU$cover_count_ht)
subplot_species_LNU <- left_join(subplot_species_LNU, species.short, by="spp")

unique(subplot_species_LNU$Native_nonnative)

#NATIVE SPECIES
LNU_subplot_native <- subplot_species_LNU %>% 
  filter(cover_count_ht == "cover"|
           cover_count_ht == "") %>% 
  filter(Native_nonnative == "native") %>% 
  mutate(species_seed.resp = paste(spp, seedling_resp, sep ="_")) %>% 
  mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
  mutate(plot_spp = paste(plot_year, species_seed.resp, by=" ")) %>% 
  select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
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

write.csv(LNU_sub_native_wide,"data/clean/LNU_subplot_native.csv" )

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

write.csv(LNU_sub_nonnative_wide,"data/clean/LNU_subplot_nonnative.csv" )

######RICHNESS
#sum up the number of non-zero entries per row 
#first column ignored ([,-1]) since this is the site name, not species count
LNU_richness_nonnative <- ddply(LNU_sub_nonnative_wide,~plot_yr_quad,function(x) {
  data.frame(NONNATIVE_RICHNESS=sum(x[-1]>0.0))})

LNU_richness_native <- ddply(LNU_sub_native_wide,~plot_yr_quad,function(x) {
  data.frame(NATIVE_RICHNESS=sum(x[-1]>0.0))})

LNU_richness <- left_join(LNU_richness_native, LNU_richness_nonnative, by="plot_yr_quad")
LNU_richness <- LNU_richness %>% 
  pivot_longer(!plot_yr_quad,names_to = "native.nonnative",
               values_to = "richness") %>% 
  separate(plot_yr_quad, c("plot_year", "quad"), sep=" ") %>% 
  separate(plot_year, c("plotid", "year"), sep="_") 
LNU_richness <- left_join(LNU_richness, plot.description.short, by="plotid")

LNU_richness$num_burn <- as.factor(LNU_richness$num_burn)

ggplot(data=LNU_richness, aes(x=num_burn, y=richness, colour = native.nonnative))+
  geom_smooth()+
  #facet_wrap(~cool.warm_slope)
  xlab('Fire Frequency') +
  ylab('Species Frequency')

######SHANNON DIVERSITY
#sum up the number of non-zero entries per row 
#first column ignored ([,-1]) since this is the site name, not species count
LNU_shan_nonnative <- ddply(LNU_sub_nonnative_wide,~plot_yr_quad,function(x) {
  data.frame(NONNATIVE_shanDiv=diversity(x[-1], index="shannon"))})

LNU_shan_native <- ddply(LNU_sub_native_wide,~plot_yr_quad,function(x) {
  data.frame(NATIVE_shanDiv=diversity(x[-1], index="shannon"))})

LNU_shanDiv <- left_join(LNU_shan_native, LNU_shan_nonnative, by="plot_yr_quad")
LNU_shanDiv <- LNU_shanDiv %>% 
  pivot_longer(!plot_yr_quad,names_to = "native.nonnative",
               values_to = "shanDiv") %>% 
  separate(plot_yr_quad, c("plot_year", "quad"), sep=" ") %>% 
  separate(plot_year, c("plotid", "year"), sep="_") 

LNU_shanDiv <- left_join(LNU_shanDiv, plot.description.short, by="plotid")

LNU_shanDiv$num_burn <- as.factor(LNU_shanDiv$num_burn)
str(LNU_shanDiv)

ggplot(data=LNU_shanDiv, aes(x=num_burn, y=shanDiv, colour = native.nonnative))+
  geom_point()+
  geom_smooth(se=TRUE,method = "lm")
  #facet_wrap(~cool.warm_slope)
  xlab('Fire Frequency') +
  ylab('Species Frequency')


#####SEEDLING DENSITY
  
  seedling_sh_count <- subplot_species_LNU %>% 
    filter(cover_count_ht == "count") %>% 
    filter(seedling_resp == "seedling") %>%
    mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
    mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
    select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
    mutate_all(na_if,"") %>% 
    mutate_all(~replace_na(., 0)) %>% 
    pivot_longer(!plot_spp, names_to = 'quad', values_to = 'density') %>% 
    separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
    separate(plot_year, c("plotid", "year"), sep="_") 
  
  str(sh_count)
seedling_sh_count <- left_join(seedling_sh_count, plot.description.short, by="plotid")
seedling_sh_count <- left_join(seedling_sh_count, species.short, by ="spp")
seedling_sh_count$num_burn <- as.numeric(seedling_sh_count$num_burn)
seedling_sh_count$density <- as.numeric(seedling_sh_count$density)

  ggplot(data=seedling_sh_count, aes(x=num_burn, y=density, colour=spp))+
    geom_smooth(method="lm")
    

seedling_sh_cover <- subplot_species_LNU %>% 
      filter(cover_count_ht == "cover") %>% 
      filter(seedling_resp == "seedling") %>%
      mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
      mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
      select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
      mutate_all(na_if,"") %>% 
      mutate_all(~replace_na(., 0)) %>% 
      pivot_longer(!plot_spp, names_to = 'quad', values_to = 'cover') %>% 
      separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
      separate(plot_year, c("plotid", "year"), sep="_") 
seedling_sh_cover$cover[seedling_sh_cover$cover == "tr"] <- "0.05"
seedling_sh_cover$cover[seedling_sh_cover$cover == "TR"] <- "0.05"
    
str(seedling_sh_cover)
    seedling_sh_cover <- left_join(seedling_sh_cover, plot.description.short, by="plotid")
    seedling_sh_cover <- left_join(seedling_sh_cover, species.short, by ="spp")
    seedling_sh_cover$num_burn <- as.numeric(seedling_sh_cover$num_burn)
    seedling_sh_cover$cover <- as.numeric(seedling_sh_cover$cover)

    ggplot(data=seedling_sh_cover, aes(x=num_burn, y=cover, colour=spp))+
      geom_smooth(method = "lm")

    
    
  sh_count <- subplot_species_LNU %>% 
      filter(cover_count_ht == "count") %>% 
      mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
      mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
      select(plot_spp, Q1, Q2, Q3, Q4, Q5)  %>% 
      mutate_all(na_if,"") %>% 
      mutate_all(~replace_na(., 0)) %>% 
      pivot_longer(!plot_spp, names_to = 'quad', values_to = 'density') %>% 
      separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
      separate(plot_year, c("plotid", "year"), sep="_") 
  
    
    
    sh_count <- left_join(sh_count, plot.description.short, by="plotid")
   sh_count <- left_join(sh_count, species.short, by ="spp")
    sh_count$num_burn <- as.numeric(sh_count$num_burn)
    sh_count$density <- as.numeric(sh_count$density)
    
    ggplot(data=sh_count, aes(x=num_burn, y=density, colour=spp))+
      geom_smooth(method = "lm")
    