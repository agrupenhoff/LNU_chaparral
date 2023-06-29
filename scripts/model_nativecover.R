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



LNU_cover_final <- read.csv("data/clean/LNU_speciesCover_FINAL.csv")

ggplot(LNU_cover_final %>%  filter(PFR != "oak woodland" &
                                     native.nonnative.prop == "prop_native_cover") , 
       aes(x=num_burn, y=cover))+
  geom_point()+
  geom_smooth(method="loess")

ggplot(LNU_cover_final %>%  filter(PFR != "oak woodland" &
                                     native.nonnative.prop == "prop_native_cover"), 
       aes(x=TSLF, y=cover, color = TSLF_bin))+
  geom_point()+
  geom_smooth(method="loess")

propnativeCover <- LNU_cover_final %>% 
  filter(PFR != "oak woodland"&
           native.nonnative.prop == "prop_native_cover") 

propnativeCover %>% 
  dplyr::count(TSLF)

propnativeCover$num_burn <- as.numeric(propnativeCover$num_burn)
propnativeCover$year <- as.factor(propnativeCover$year)
propnativeCover$elevation <- as.numeric(propnativeCover$elevation)
propnativeCover$cool_warm_slope <- as.factor(propnativeCover$cool_warm_slope)
propnativeCover$TSLF_bin <- as.factor(propnativeCover$TSLF_bin)

str(propnativeCover)
range(propnativeCover$cover)

propnativeCover <- propnativeCover %>% 
  #can't have exactly 0 or 1
  mutate(cover = ifelse(cover == 1, 0.999, cover))

#CENTER AND SCALE COVARIATES
propnativeCover$ppt.scale <- scale(propnativeCover$ppt, 
                                            center = TRUE, scale = TRUE)
propnativeCover$tmean.scale <- scale(propnativeCover$tmean, 
                                              center = TRUE, scale = TRUE)
propnativeCover$hli.scale <- scale(propnativeCover$hli, 
                                            center = TRUE, scale = TRUE)
propnativeCover$slope.scale <- scale(propnativeCover$slope, 
                                              center = TRUE, scale = TRUE)
propnativeCover$elevation.scale <- scale(propnativeCover$elevation, 
                                                  center = TRUE, scale = TRUE)


######## run model with each predictor variable seperately 
m.propnatCover_numburn <- brm(
  bf(cover ~ 1 + num_burn, 
     phi ~ num_burn),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_numburn, file= "models/nativeCover_numburn.rda")
load("models/nativeCover_numburn.rda")
summary(m.propnatCover_numburn)
conditional_effects(m.propnatCover_numburn)
pp_check(m.propnatCover_numburn, ndraw=100)
tidy(m.propnatCover_numburn)
bayes_R2(m.propnatCover_numburn)

m.propnatCover_TSLF <- brm(
  bf(cover ~ 1 + TSLF, 
     phi ~ TSLF),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_TSLF, file= "models/nativeCover_TSLF.rda")
load("models/nativeCover_TSLF.rda")
summary(m.propnatCover_TSLF)
pp_check(m.propnatCover_TSLF, ndraws = 100)
conditional_effects(m.propnatCover_TSLF)
bayes_R2(m.propnatCover_TSLF)

m.propnatCover_TSLFbin <- brm(
  bf(cover ~ 1 + TSLF_bin, 
     phi ~ TSLF_bin),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_TSLFbin, file= "models/nativeCover_TSLFbin.rda")
load("models/nativeCover_TSLFbin.rda")
summary(m.propnatCover_TSLFbin)
conditional_effects(m.propnatCover_TSLFbin)
pp_check(m.propnatCover_TSLFbin, ndraw=100)

loo(m.propnatCover_numburn, m.propnatCover_TSLF)

m.propnatCover_sq <- brm(
  bf(cover ~ 1 + num_burn + I(num_burn^2), 
     phi ~ num_burn+ I(num_burn^2)),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_sq, file= "models/nativeCover_numburn_sq.rda")
load("models/nativeCover_numburn_sq.rda")
summary(m.propnatCover_sq)
conditional_effects(m.propnatCover_sq)
pp_check(m.propnatCover_sq, ndraw=100)
bayes_R2(m.propnatCover_sq)

m.propnatCover_sq_year <- brm(
  bf(cover ~ 1 + num_burn + I(num_burn^2) + year, 
     phi ~ num_burn+ I(num_burn^2) + year),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_sq_year, file= "models/nativeCover_numburn_sq_year.rda")
load("models/nativeCover_numburn_sq_year.rda")
summary(m.propnatCover_sq_year)
pp_check(m.propnatCover_sq_year, ndraw=100)
bayes_R2(m.propnatCover_sq_year)

loo(m.propnatCover_numburn, m.propnatCover_sq, m.propnatCover_sq_year)


m.propnatCover_sq_year_TSLF <- brm(
  bf(cover ~ 1 + num_burn + I(num_burn^2) + year + TSLF_bin, 
     phi ~ num_burn+ I(num_burn^2) + year + TSLF_bin),
  data = propnativeCover,
  family = Beta(),
  chains = 4, iter = 2000, warmup = 1000,
  cores=4,
  seed = 1234)
save(m.propnatCover_sq_year_TSLF, file= "models/nativeCover_numburn_sq_year_TSLF.rda")
load("models/nativeCover_numburn_sq_year_TSLF.rda")

summary(m.propnatCover_sq_year_TSLF)
conditional_effects(m.propnatCover_sq_year_TSLF)
pp_check(m.propnatCover_sq_year_TSLF, ndraws = 100)
bayes_R2(m.propnatCover_sq_year_TSLF)

loo(m.propnatCover_sq_year_TSLF, m.propnatCover_sq, m.propnatCover_sq_year)

#CREATE FIGURE 

plot_predictions(m.propnatCover_sq_year,
                 condition = "num_burn",
                 vcov = TRUE)+
  geom_point(data = propnativeCover,
             aes(num_burn, cover))

summary(m.propnatCover_sq_year)

nd <- propnativeCover %>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

prop.cover.fitted <- 
  fitted(m.propnatCover_sq_year, 
         newdata= nd,
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) 


ggplot(prop.cover.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  geom_point(data=propnativeCover, 
             aes(x=num_burn, y=cover),alpha=.2)+
  geom_jitter(data=propnativeCover, 
              aes(x=num_burn, y=cover), alpha =.2)+
  labs(x="Fire Frequency", y= "Proportion Native cover",
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











