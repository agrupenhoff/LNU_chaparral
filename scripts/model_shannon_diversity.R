library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(plyr)
library(brms)
library(rstan)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(emmeans)
library(calecopal)
library(modelr)
library(brmsmargins)
library(ROCR) #calculate area under curve
library(stringr) #str_detect
library(forcats) #fct_rev
library(ggridges)
library(broom)            # Convert model objects to data frames
library(broom.mixed)  
library(marginaleffects)

LNU_shan_5m2 <- read.csv("data/clean/LNU_subplot_SHANNON_5m2.csv")


LNU_shan_5m2 %>% 
  filter(PFR != "oak woodland") %>% 
  ggplot(aes(x=num_burn, y=prop.nativeshan, color = as.factor(year)))+
  geom_point()

#visualize raw data - not updated

shan_sum <- mean_shan %>% 
  group_by(num_burn, native.nonnative.prop, year) %>% 
  dplyr::summarise(mean_shan = mean(shannon, na.rm=TRUE),
                   max_shan = max(shannon, na.rm=TRUE),
                   min_shan = min(shannon, na.rm=TRUE),
                   n_obs = n(),
                   sd = sd(shannon, na.rm=TRUE),
                   se= sd/sqrt(n_obs))

ggplot(shan_sum , 
       aes(x=as.factor(num_burn), y=mean_shan, fill=native.nonnative.prop))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = cal_palette("sierra1")) +
  geom_errorbar(aes(ymax=mean_shan + se, ymin=mean_shan-se),
                width=.2,
                position=position_dodge(.9))+
  facet_wrap(~year)+
  labs(x="Fire Frequency", y= "Average Shannon Diversity",
       fill="")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))

shannon <- LNU_shan_5m2 %>% 
  filter(PFR != "oak woodland") %>% 
  filter(num_burn != 7) %>% 
  mutate(prop.nativeshan = ifelse(prop.nativeshan == 1, 0.999, prop.nativeshan))

shannon$num_burn <- as.numeric(shannon$num_burn)
shannon$year <- as.factor(shannon$year)
shannon$elevation <- as.numeric(shannon$elevation)
shannon$cool_warm_slope <- as.factor(shannon$cool_warm_slope)


#CENTER AND SCALE COVARIATES
shannon$ppt.scale <- scale(shannon$ppt, 
                           center = TRUE, scale = TRUE)
shannon$tmean.scale <- scale(shannon$tmean, 
                             center = TRUE, scale = TRUE)
shannon$hli.scale <- scale(shannon$hli, 
                           center = TRUE, scale = TRUE)
shannon$slope.scale <- scale(shannon$slope, 
                             center = TRUE, scale = TRUE)
shannon$elevation.scale <- scale(shannon$elevation, 
                                 center = TRUE, scale = TRUE)

######## run model with each predictor variable seperately 

m.propnatShan <- brm(
  bf(prop.nativeshan ~ 1 + num_burn, 
     phi ~ num_burn),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)

save(m.propnatShan, file= "models/nativeShan_numburn.rda")
load("models/nativeShan_numburn.rda")
summary(m.propnatShan)
conditional_effects(m.propnatShan)

m.propnatShan_year <- brm(
  bf(prop.nativeshan ~ 1 + year, 
     phi ~ year),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatShan_year, file= "models/nativeShan_year.rda")
load("models/nativeShan_year.rda")

m.propnatShan_slope <- brm(
  bf(prop.nativeshan ~ 1 + cool_warm_slope, 
     phi ~ cool_warm_slope),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatShan_slope, file= "models/nativeShan_slope.rda")
load("models/nativeShan_slope.rda")

m.propnatShan_hli <- brm(
  bf(prop.nativeshan ~ 1 + hli.scale, 
     phi ~ hli.scale),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatShan_hli, file= "models/nativeShan_hli.rda")
load("models/nativeShan_hli.rda")

loo(m.propnatShan, m.propnatShan_slope, m.propnatShan_year, m.propnatShan_hli)

m.propnatShan2 <- brm(
  bf(prop.nativeshan ~ 1 + num_burn + year, 
     phi ~ num_burn + year),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)

save(m.propnatShan2, file= "models/nativeShan2.rda")
load("models/nativeShan2.rda")

summary(m.propnatShan2)
loo(m.propnatShan, m.propnatShan2)

m.propnatShan3 <- brm(
  bf(prop.nativeshan ~ 1 + num_burn + year + cool_warm_slope, 
     phi ~ num_burn + year + cool_warm_slope),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)

save(m.propnatShan3, file= "models/nativeShan3.rda")
load("models/nativeShan3.rda")

summary(m.propnatShan2)
loo(m.propnatShan, m.propnatShan2, m.propnatShan3)

#CREATE FIGURE

nd <- shannon %>% 
  data_grid(num_burn=seq(1,7,by=.01),
            year=c(2021, 2022))

shannon.fitted <- 
  fitted(m.propnatShan2, 
         newdata= nd,
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) 


ggplot(shannon.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  geom_point(data=shannon, 
             aes(x=num_burn, y=prop.nativeshan),alpha=.2)+
  geom_jitter(data=shannon, 
              aes(x=num_burn, y=prop.nativeshan), alpha =.2)+
  xlim(1,6)+
  labs(x="Fire Frequency", y= "Proportion Native Shannon Diversity",
       fill="credible interval")+
  scale_color_manual(values = cal_palette("sierra1")) +
  scale_fill_manual(values = cal_palette("sierra1")) +
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))+
  guides(fill = FALSE)
