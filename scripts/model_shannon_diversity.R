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
LNU_simp_5m2 <- read.csv("data/clean/LNU_subplot_SIMPSON_5m2.csv")

LNU_shan_5m2 %>% 
  filter(PFR != "oak woodland") %>% 
  ggplot(aes(x=num_burn, y=prop.nativeshan, color = as.factor(year)))+
  geom_point()+
  geom_smooth(method = "lm")

shannon <- LNU_shan_5m2 %>% 
  filter(PFR != "oak woodland") %>% 
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
bayes_R2(m.propnatShan)

m.propnatShan_TSLF <- brm(
  bf(prop.nativeshan ~ 1 + TSLF, 
     phi ~ TSLF),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatShan_TSLF, file= "models/nativeShan_TSLF.rda")
load("models/nativeShan_TSLF.rda")
summary(m.propnatShan_TSLF)
bayes_R2(m.propnatShan_TSLF)
pp_check(m.propnatShan_TSLF, ndraw=100)


m.propnatShan_TSLFbin <- brm(
  bf(prop.nativeshan ~ 1 + TSLF_bin, 
     phi ~ TSLF_bin),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatShan_TSLFbin, file= "models/nativeShan_TSLFbin.rda")
load("models/nativeShan_TSLFbin.rda")
summary(m.propnatShan_TSLFbin)
bayes_R2(m.propnatShan_TSLFbin)

loo(m.propnatShan, m.propnatShan_TSLFbin)

m.propnatShan_sq <- brm(
  bf(prop.nativeshan ~ 1 + num_burn + I(num_burn^2), 
     phi ~ num_burn + I(num_burn^2)),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)

save(m.propnatShan_sq, file= "models/nativeShan_sq.rda")
load("models/nativeShan_sq.rda")
summary(m.propnatShan_sq)
pp_check(m.propnatShan_sq, ndraws = 100)
loo(m.propnatShan, m.propnatShan_sq)

m.propnatShan_sq_year <- brm(
  bf(prop.nativeshan ~ 1 + num_burn + I(num_burn^2)+ year, 
     phi ~ num_burn +I(num_burn^2)+ year),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 3000, warmup = 1000,
  cores=4,
  seed = 1234)

save(m.propnatShan_sq_year, file= "models/nativeShan_sq_year.rda")
load("models/nativeShan_sq_year.rda")
summary(m.propnatShan_sq_year)
pp_check(m.propnatShan_sq_year, ndraw=100)
conditional_effects(m.propnatShan_sq_year)
bayes_R2(m.propnatShan_sq_year)

loo(m.propnatShan, m.propnatShan_sq, m.propnatShan_sq_year)

m.propnatShan_sq_year_TSLF <- brm(
  bf(prop.nativeshan ~ 1 + num_burn + I(num_burn^2)+ year+TSLF_bin, 
     phi ~ num_burn +I(num_burn^2)+ year+TSLF_bin),
  data = shannon,
  family = Beta(),
  chains = 4, iter = 3000, warmup = 1000,
  cores=4,
  seed = 1234)


save(m.propnatShan_sq_year_TSLF, file= "models/nativeShan_sq_year_TSLF.rda")
load("models/nativeShan_sq_year_TSLF.rda")

summary(m.propnatShan_sq_year_TSLF)
pp_check(m.propnatShan_sq_year_TSLF, ndraw=100)
conditional_effects(m.propnatShan_sq_year_TSLF)

loo(m.propnatShan, m.propnatShan_sq, m.propnatShan_sq_year, m.propnatShan_sq_year_TSLF)

#CREATE FIGURE
load("models/nativeShan_sq_year.rda")
summary(m.propnatShan_sq_year)
bayes_R2(m.propnatShan_sq_year)

#overall average trend = -0.0407
emtrends(m.propnatShan_sq_year, ~ 1, var = "num_burn", 
         regrid = "response")


m.propnatShan_sq_year %>% 
  emtrends(~ num_burn, var = "num_burn",
           at = list(num_burn = c(1,2,3, 4,5, 6)),
           regrid = "response") 

#slope depends on levels of FF
m.propnatShan_sq_year %>% 
  emtrends(~ num_burn, var = "num_burn",
           at = list(num_burn = c(1,2,3, 4,5, 6)),
           regrid = "response") 

nd <- shannon %>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

shannon.fitted <- 
  fitted(m.propnatShan_sq_year, 
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

#average marginal effect at different frequencies
m.propnatShan_sq_year %>% 
  emtrends(~ num_burn, var = "num_burn",
           at = list(num_burn = c(1,2,3, 4,5, 6)),
           regrid = "response") %>% 
  gather_emmeans_draws() %>% 
  ggplot(aes(x = .value, fill = factor(num_burn))) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi",
               slab_alpha = 0.75)+
  scale_fill_manual(values = cal_palette("sierra1")) +
  labs(x = "Average marginal effect of fire frequency", 
       y = "Density", fill = "Fire Frequency",
       caption = "80% and 95% credible intervals shown in black")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))+
  ggtitle("Proportion Native Shannon Diversity")
