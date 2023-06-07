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

#run faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



LNU_richness_5m2 <- read.csv("data/clean/LNU_subplot_RICHNESS_5m2.csv")

ggplot(LNU_richness_5m2 %>%  filter(PFR != "oak woodland") , 
       aes(x=num_burn, y=prop.native, color = TSLF_bin))+
  geom_point()+
  geom_smooth(method="loess")

ggplot(LNU_richness_5m2 %>%  filter(PFR != "oak woodland") , 
       aes(x=TSLF_bin, y=prop.native, color = TSLF_bin))+
  geom_point()+
  geom_smooth(method="loess")

propnativeRich_5m2 <- LNU_richness_5m2 %>% 
  filter(PFR != "oak woodland") 

propnativeRich_5m2 %>% 
  dplyr::count(TSLF_bin)

propnativeRich_5m2$num_burn <- as.numeric(propnativeRich_5m2$num_burn)
propnativeRich_5m2$year <- as.factor(propnativeRich_5m2$year)
propnativeRich_5m2$elevation <- as.numeric(propnativeRich_5m2$elevation)
propnativeRich_5m2$cool_warm_slope <- as.factor(propnativeRich_5m2$cool_warm_slope)
propnativeRich_5m2$TSLF <- as.factor(propnativeRich_5m2$TSLF)


propnativeRich_small_5m2 <- propnativeRich_5m2 %>% 
  dplyr::select(NATIVE_RICHNESS, NONNATIVE_RICHNESS,
                prop.native, year, Site, num_burn, mCC_FRI, TSLF,TSLF_bin,
                ppt, tmean, elevation, slope, Distance_km, hli,
                Geology, aspect_name, cool_warm_slope) %>% 
  #can't have exactly 0 or 1
  mutate(prop.native = ifelse(prop.native == 1, 0.999, prop.native))

#CENTER AND SCALE COVARIATES
propnativeRich_small_5m2$ppt.scale <- scale(propnativeRich_small_5m2$ppt, 
                                            center = TRUE, scale = TRUE)
propnativeRich_small_5m2$tmean.scale <- scale(propnativeRich_small_5m2$tmean, 
                                              center = TRUE, scale = TRUE)
propnativeRich_small_5m2$hli.scale <- scale(propnativeRich_small_5m2$hli, 
                                            center = TRUE, scale = TRUE)
propnativeRich_small_5m2$slope.scale <- scale(propnativeRich_small_5m2$slope, 
                                              center = TRUE, scale = TRUE)
propnativeRich_small_5m2$elevation.scale <- scale(propnativeRich_small_5m2$elevation, 
                                                  center = TRUE, scale = TRUE)


######## run model with each predictor variable seperately 
m.propnatRich_numburn <- brm(
  bf(prop.native ~ 1 + num_burn, 
     phi ~ num_burn),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_numburn, file= "models/nativeRich_numburn_5m2.rda")
load("models/nativeRich_numburn_5m2.rda")
summary(m.propnatRich_numburn)

m.propnatRich_TSLF <- brm(
  bf(prop.native ~ 1 + TSLF, 
     phi ~ TSLF),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_TSLF, file= "models/nativeRich_TSLF_5m2.rda")
load("models/nativeRich_numburn_5m2.rda")
summary(m.propnatRich_TSLF)
conditional_effects(m.propnatRich_TSLF)
pp_check(m.propnatRich_TSLF)

m.propnatRich_TSLFbin <- brm(
  bf(prop.native ~ 1 + TSLF_bin, 
     phi ~ TSLF_bin),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_TSLFbin, file= "models/nativeRich_TSLFbin_5m2.rda")
summary(m.propnatRich_TSLFbin)
conditional_effects(m.propnatRich_TSLFbin)
pp_check(m.propnatRich_TSLFbin)

m.propnatRich_year <- brm(
  bf(prop.native ~ 1 + year, 
     phi ~ year),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_year, file= "models/nativeRich_year_5m2.rda")
load("models/nativeRich_year_5m2.rda")
summary(m.propnatRich_year)

loo(m.propnatRich_numburn, m.propnatRich_TSLFbin, m.propnatRich_year)

m.propnatRich_sq <- brm(
  bf(prop.native ~ 1 + num_burn + I(num_burn^2), 
     phi ~ num_burn+ I(num_burn^2)),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_sq, file= "models/nativeRich_numburn_sq_5m2.rda")
load("models/nativeRich_numburn_sq_5m2.rda")
summary(m.propnatRich_sq)
conditional_effects(m.propnatRich_sq)

m.propnatRich_sq_year <- brm(
  bf(prop.native ~ 1 + num_burn + I(num_burn^2) + year, 
     phi ~ num_burn+ I(num_burn^2) + year),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_sq_year, file= "models/nativeRich_numburn_sq_year_5m2.rda")
load("models/nativeRich_numburn_sq_year_5m2.rda")
summary(m.propnatRich_sq_year)

loo(m.propnatRich_numburn, m.propnatRich_sq, m.propnatRich_sq_year)

summary(m.propnatRich_sq_year)
conditional_effects(m.propnatRich_sq_year)
plot(m.propnatRich_sq)
pp_check(m.propnatRich_sq_year, ndraw=100)
bayes_R2(m.propnatRich_sq_year)


m.propnatRich_sq_year_TSLF <- brm(
  bf(prop.native ~ 1 + num_burn + I(num_burn^2) + year + TSLF_bin, 
     phi ~ num_burn+ I(num_burn^2) + year + TSLF_bin),
  data = propnativeRich_small_5m2,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatRich_sq_year_TSLF, file= "models/nativeRich_numburn_sq_year_TSLF_5m2.rda")
load("models/nativeRich_numburn_sq_year_TSLF_5m2.rda")

summary(m.propnatRich_sq_year_TSLF)
conditional_effects(m.propnatRich_sq_year_TSLF)
plot(m.propnatRich_sq_TSLF)
pp_check(m.propnatRich_sq_year_TSLF, ndraws = 100)

loo(m.propnatRich_sq_year_TSLF, m.propnatRich_sq, m.propnatRich_sq_year)

#CREATE FIGURE for m.propnatrich_sq_year

plot_predictions(m.propnatRich_sq_year_TSLF,
                 condition = "num_burn",
                 vcov = TRUE)+
  geom_point(data = propnativeRich_small_5m2,
             aes(num_burn, prop.native))


nd <- propnativeRich_5m2 %>% 
  data_grid(num_burn=seq(1,7,by=.01),
            year=c(2021, 2022),
            TSLF_bin=unique(propnativeRich_5m2$TSLF_bin))

prop.rich.5m2.fitted <- 
  fitted(m.propnatRich_sq_year_TSLF, 
         newdata= nd,
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) 


ggplot(prop.rich.5m2.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  facet_wrap(~TSLF_bin)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  geom_point(data=propnativeRich_small_5m2, 
             aes(x=num_burn, y=prop.native),alpha=.2)+
  geom_jitter(data=propnativeRich_small_5m2, 
              aes(x=num_burn, y=prop.native), alpha =.2)+
  #facet_wrap(~TSLF)+
  labs(x="Fire Frequency", y= "Proportion Native richness",
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











