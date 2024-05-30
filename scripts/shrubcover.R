
library(ggplot2)
library(tidyverse)
library(dplyr)


plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
shrub_cover_ALL_final <- read.csv("data/clean/shrub_TOTAL_cover.csv")

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
  group_by(PlotID, year) %>% 
  dplyr::summarise(mean_diam_mm = mean(Mean_diam),
                   sd = sd(Mean_diam),
                   n_obs = n(),
                   se = sd/sqrt(n_obs),
                   max_diam_mm = max(Mean_diam))

severity_LNU_clean <- left_join(severity_LNU_clean, 
                                plot.description_LNU, by="PlotID")

sevdata <- ddply(severity_LNU_clean, c("num_burn"),
                 summarise,
                 N = sum(!is.na(mean_diam_mm)),
                 mean_diam_mm_avg = mean(mean_diam_mm, na.rm=TRUE),
                 sd= sd(mean_diam_mm, na.rm = TRUE),
                 se = sd/sqrt(N),
                 cv = 100*sd/mean_diam_mm_avg,
                 
                 max = mean(max_diam_mm, na.rm = TRUE),
                 sd_max = sd(max_diam_mm, na.rm = TRUE),
                 se_max = sd_max/sqrt(N),
                 cv_max = 100*sd_max/max
) 