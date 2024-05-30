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

species.short <- SpeciesList %>% 
  dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 


#add species descriptions to data
subplot_species_LNU <- left_join(subplot_species_LNU, species.short,
                                 by = "spp")


  # SEVERITY DATA ----
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

  # file for NMDS ----

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


  # NATIVE SPECIES ----
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
            
            LNU_subplot_native_long <- LNU_subplot_native %>% 
              group_by(plot_year, spp) %>% 
             dplyr::summarise(cover = mean(cover))

          LNU_sub_native_wide <- LNU_subplot_native_long %>% 
            pivot_wider(names_from="spp",
                        values_from="cover",
                        values_fill = 0, values_fn = sum)
          
          write.csv(LNU_subplot_native_long,"data/clean/LNU_subplot_native_long.csv" )
          write.csv(LNU_sub_native_wide,"data/clean/LNU_subplot_native_wide.csv" )

  # NONNATIVE SPECIES ----
          
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
          
          LNU_subplot_nonnative_long <- LNU_subplot_nonnative %>% 
            group_by(plot_year, spp) %>% 
            dplyr::summarise(cover = mean(cover))
          
          LNU_sub_nonnative_wide <- LNU_subplot_nonnative_long %>% 
            pivot_wider(names_from="spp",
                        values_from="cover",
                        values_fill = 0, values_fn = sum)
          
          write.csv(LNU_subplot_nonnative_long,"data/clean/LNU_subplot_nonnative_long.csv" )
          write.csv(LNU_sub_nonnative_wide,"data/clean/LNU_subplot_nonnative_wide.csv" )




  # SHRUB DENSITY ----
          
          #make data frame with every combination of plot x shrub spp combination
          sh_spp <- subplot_species_LNU %>% filter(Lifeform == "SH")
          unique(sh_spp$spp)
          nd <- expand_grid(PlotID =unique(subplot_species_LNU$PlotID),
                            spp =unique(sh_spp$spp),
                            year=unique(subplot_species_LNU$year))
          str(nd)
          nd$year <- as.numeric(nd$year)

    ## Shrub density of seedling - avg and sum count ----
            str(shrub_density_long)
            shrub_density_long <- subplot_species_LNU %>% 
              filter(cover_count_ht == "COUNT",
                     seedling_resp == "SEEDLING") %>% 
              mutate(plot_year = paste(PlotID, year, sep="_")) %>%  
              mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
              select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
              mutate_if(is.numeric, ~replace_na(., 0)) %>% 
              mutate(avg.count.1m2 = (Q1+Q2+Q3+Q4+Q5)/5,
                     sum.count.5m2 = Q1+Q2+Q3+Q4+Q5,
                     count.250m2 = avg.count.1m2 * 250) %>% 
              select(plot_spp, avg.count.1m2, sum.count.5m2, count.250m2) %>% 
              separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
              separate(plot_year, c("PlotID", "year"), sep="_") 
            
            shrub_density_long$year <- as.numeric(shrub_density_long$year)
            #now add to nd to account for 0s
            shrub_density_zeros <- left_join(nd, shrub_density_long, 
                                             by=c("PlotID","spp","year"))
            shrub_density_final <- shrub_density_zeros %>% 
              mutate_if(is.numeric, ~replace_na(., 0)) 
            
            write.csv(shrub_density_final, "data/clean/shrub_density_count.csv")

    ## Shrub seedling - proportion of presence----
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

    ## Shrub seedling cover ----
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

    #   SHRUB COVER ----     
            
            shrub_cover_ALL <- subplot_species_LNU %>% 
              filter(cover_count_ht == "COVER",
                     Lifeform == "SH",
                     seedling_resp == "RESPROUT") %>%  
              #mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
              #mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
              select(PlotID, year, spp, Q1, Q2, Q3, Q4, Q5) %>% 
              mutate_if(is.numeric, ~replace_na(., 0)) %>% 
              mutate(avg.cover.ALL = (Q1+Q2+Q3+Q4+Q5)/5,
                     sum.cover.ALL = Q1+Q2+Q3+Q4+Q5) 
            
            str(shrub_cover_ALL)
            shrub_cover_ALL$year <- as.numeric(shrub_cover_ALL$year)
            #now add to nd to account for 0s
            shrub_cover_ALL_zeros <- left_join(nd, shrub_cover_ALL, 
                                           by=c("PlotID","spp","year"))
            
            shrub_cover_ALL_final <- shrub_cover_ALL_zeros %>% 
              mutate_if(is.numeric, ~replace_na(., 0)) 
            
            write.csv(shrub_cover_ALL_final, "data/clean/shrub_TOTAL_cover.csv")
            
            plot.description_LNU %>% 
              ggplot(aes(x=num_burn, y= TSLF,
                         color = as.factor(num_burn)))+
              geom_point()
            
            shrubcover_LNU <- left_join(shrub_cover_ALL_final, 
                                        plot.description_LNU, by="PlotID")
            
            shrubcover_LNU %>% 
              filter(spp == "ADFA") %>% 
              ggplot(aes(x=as.factor(num_burn), y=sum.cover.ALL,
                         color = as.factor(year)))+
              geom_boxplot()
            
            
            shrubcover_mean <- shrubcover_LNU %>% 
              group_by(num_burn, year) %>% 
              dplyr::summarise(mean_cover = mean(sum.cover.ALL),
                               sd = sd(sum.cover.ALL),
                               n_obs = n(),
                               se = sd/sqrt(n_obs))
            
            ggplot(data=shrubcover_mean, 
                   aes(x=as.factor(num_burn), y=mean_cover))+
              geom_bar(stat = "identity", aes(fill= as.factor(num_burn)))
              geom_errorbar(aes(ymin=mean_diam_mm_avg-se, ymax=mean_diam_mm_avg+se), width=.1, size=.7)+
              #geom_point(size=3, aes(colour=factor(num_burn)))+
              scale_fill_manual(values = cal_palette("sierra1")) +
              xlab("Fire frequency")+
              ylab("Fire Severity (mm)")+
              theme_bw()+
              theme(legend.title = element_blank(),
                    axis.text = element_text(size=15),
                    text = element_text(size = 16),
                    panel.grid = element_blank())
            
        #   SHRUB RESPROUT COUNT ----     
            
            shrub_resp_count <- subplot_species_LNU %>% 
              filter(cover_count_ht == "COUNT",
                     Lifeform == "SH",
                     seedling_resp == "RESPROUT") %>% 
            mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
              mutate(plot_spp = paste(plot_year, spp, by=" ")) %>% 
              select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
            mutate_if(is.numeric, ~replace_na(., 0)) %>% 
              mutate(avg.cover.ALL = (Q1+Q2+Q3+Q4+Q5)/5,
                     sum.cover.ALL = Q1+Q2+Q3+Q4+Q5) %>% 
              select(plot_spp, avg.cover.ALL, sum.cover.ALL) %>% 
              separate(plot_spp, c("plot_year", "spp"), sep=" ") %>% 
              separate(plot_year, c("PlotID", "year"), sep="_")
            
            shrubcount_LNU <- left_join(shrub_resp_count, 
                                        plot.description_LNU, by="PlotID")
            
            shrubcount_LNU %>% 
              filter(spp == "ADFA") %>% 
              ggplot(aes(x=as.factor(num_burn), y=avg.cover.ALL,
                         color = as.factor(year)))+
              geom_boxplot()

            shrub_resp_count$year <- as.numeric(shrub_resp_count$year)
            #now add to nd to account for 0s
            shrub_resp_count_zeros <- left_join(nd,  shrub_resp_count, 
                                               by=c("PlotID","spp","year"))
            
            shrub_cover_ALL_final <- shrub_cover_ALL_zeros %>% 
              mutate_if(is.numeric, ~replace_na(., 0)) 
            
            write.csv(shrub_cover_ALL_final, "data/clean/shrub_TOTAL_cover.csv")
            

    # SHRUB REGEN PRESENCE 250m2 plot ----
    plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
    total_data <- left_join(TOT_species_LNU, plot.description_LNU, by="PlotID")
    total_data <- left_join(total_data, species.short, by ="spp")
    total_data$spp <- toupper(total_data$spp)
    
    total_seedling <- total_data %>% 
      filter(SEEDLING.RESPROUT == "SEEDLING") %>% 
      mutate(presence = 1) %>% 
      dplyr::select(PlotID, spp, year, presence)
    
    unique(total_seedling$spp)
    
    nd <- expand_grid(PlotID =unique(total_data$PlotID),
                      spp =unique(total_seedling$spp),
                      year=unique(total_data$year))
    str(nd)
    nd$year <- as.numeric(nd$year)
    
    total_density_zeros <- left_join(nd, total_seedling, 
                                    by=c("PlotID","spp","year"))
    shrub_density_final <- total_density_zeros %>% 
      mutate_if(is.numeric, ~replace_na(., 0)) 
    
    shrub_density_250 <- left_join(shrub_density_final, plot.description_LNU, by="PlotID")
    shrub_density_250 <- left_join(shrub_density_250, species.short, by ="spp")
    
    write.csv(shrub_density_250, "data/clean/shrub_seedling_presence_250.csv")
    
    
    
    # SHRUB RESPROUT HEIGHT ----
    unique(subplot_species_LNU$cover_count_ht)
    
    shrub_height <- subplot_species_LNU %>% 
      filter(cover_count_ht == "HEIGHT",
             Lifeform == "SH") %>% 
      mutate(plot_year = paste(PlotID, year, sep="_")) %>% 
      mutate(plot_spp = paste(plot_year, spp, seedling_resp, by=" ")) %>% 
      dplyr::select(plot_spp, Q1, Q2, Q3, Q4, Q5) %>% 
      pivot_longer(!plot_spp, names_to = "quad", values_to = "postfire_ht_cm") %>% 
      drop_na(postfire_ht_cm) %>% 
      select(plot_spp, postfire_ht_cm) %>% 
      separate(plot_spp, c("plot_year", "spp", "seedling_resp"), sep=" ") %>% 
      separate(plot_year, c("PlotID", "year"), sep="_") %>% 
      #filter out seedlings - only care about resprouts!!
      filter(seedling_resp == "RESPROUT") %>% 
      filter(postfire_ht_cm >0) %>% 
      mutate(status = "L") %>% 
      mutate(postfire_ht_m = postfire_ht_cm/100) %>% 
      select(PlotID, year, spp, status, postfire_ht_m) %>% 
      mutate(prefire_ht_m = " ",
             diam_largest_stem_cm = " ",
             obs = "ARG") %>% 
     as.data.frame()
    
    mortality <- Mortality_LNU %>% 
      mutate(year = 2021,
             obs = "HDS") 
    
    str(shrub_height)
    str(mortality)
    
    shrub_height_all <- rbind(mortality, shrub_height)
    
    shrub_height_all$year <- as.numeric(shrub_height_all$year)
    
    write.csv(shrub_height_all, "data/clean/shrub_height.csv")

    
    
    
    
    