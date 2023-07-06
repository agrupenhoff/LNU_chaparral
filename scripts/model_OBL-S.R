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



#usebernoulli prior and constrain the priors

#prior1 <- prior("normal(0,1)", class = "b") +
# prior("student_t(3, 0, 1)", class = "sds", coef = "s(transect_dist_m, by = Plot)")
# only 1 coef for this. t(3,0,1) is aki vehtarhi's favorite 
        #weakly informative prior.
#I changed this from t(3, 0, 2.5)


#when no prior used, model cannot be fit, 
#rhat ~1.04 and very low estimated sample size. 
#This is probably because one or more of the predictors causes 
#full separation of the outcome. See page 256 of Regression and other 
#stories for more information.



#####DENSITY
shrub_density_250 <- read.csv("data/clean/shrub_seedling_presence_250.csv")

shrub_seedling_data <- read.csv("data/clean/shrub_seedling_data_ALL.csv")

plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_jan23.csv")

species.short <- SpeciesList %>% 
  dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 

seedling_data <- left_join(shrub_seedling_data, plot.description_LNU, by="PlotID")
seedling_data <- left_join(seedling_data, species.short, by ="spp")



#look at OBL-S only
shrub_density_250_OBLS <- shrub_density_250 %>% 
  filter(fac.obl == "OBL-S") 

shrub_density_OBLS <- seedling_data %>%
  filter(fac.obl == "OBL-S") %>% 
  mutate(prop.presence= ifelse(prop.presence == 1, 0.999, prop.presence)) %>% 
  mutate(presence = ifelse(presence >0, 1, presence))

shrub_density_OBLS$year <- as.factor(shrub_density_OBLS$year)
shrub_density_250_OBLS$year <- as.factor(shrub_density_250_OBLS$year)

shrub_density_OBLS %>% 
  dplyr::count(prop.presence == 0) %>% 
  mutate(prop = n/sum(n)) 

cor(shrub_density_OBLS[,c("num_burn","TSLF",
                          "hli","slope",
                          "ppt","tmean","elevation")])
pairs(shrub_density_OBLS[,c("num_burn","TSLF",
                            "hli","slope",
                            "ppt","tmean","elevation")])

ggplot(shrub_density_250_OBLS, aes(x=TSLF, y=presence,
                               color=spp))+
  geom_point()+
  geom_jitter()


priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("student_t(3, 0, 1)", class = "Intercept"))
# only 1 coef for this. t(3,0,1) is aki vehtarhi's favorite weakly informative prior.
# I changed this from t(3, 0, 2.5)

m.shrubDensityOBL <- brm(data = shrub_density_OBLS,
                         presence ~ 1 + num_burn,
                         family = bernoulli(),
                         control = list(adapt_delta = 0.998, max_treedepth = 15),
                         prior = priors,
                         sample_prior = TRUE,
                         iter = 2000,
                         warmup = 1000, 
                         cores = 4, chains = 4)

save(m.shrubDensityOBL, file = "models/shrubDensityOBL.rda")
load("models/shrubDensityOBL.rda")

tidy(m.shrubDensityOBL, effects = "fixed")
summary(m.shrubDensityOBL)
conditional_effects(m.shrubDensityOBL)
plot(m.shrubDensityOBL)
pp_check(m.shrubDensityOBL, ndraws = 100)
bayes_R2(m.shrubDensityOBL)

m.shrubDensityOBL_250 <- brm(data = shrub_density_250_OBLS,
                         presence ~ 1 + num_burn,
                         family = bernoulli(),
                         control = list(adapt_delta = 0.998, max_treedepth = 15),
                         prior = priors,
                         sample_prior = TRUE,
                         iter = 2000,
                         warmup = 1000, 
                         cores = 4, chains = 4)

save(m.shrubDensityOBL_250, file = "models/shrubDensityOBL_250.rda")
load("models/shrubDensityOBL_250.rda")

tidy(m.shrubDensityOBL_250, effects = "fixed")
summary(m.shrubDensityOBL_250)
conditional_effects(m.shrubDensityOBL_250)
plot(m.shrubDensityOBL_250)
pp_check(m.shrubDensityOBL_250, ndraws = 100)
bayes_R2(m.shrubDensityOBL_250)


m.shrubDensityOBL2 <- brm(data = shrub_density_OBLS,
                           presence ~ 1 + num_burn + year,
                           family = bernoulli(),
                           control = list(adapt_delta = 0.998, max_treedepth = 15),
                           prior = priors,
                           sample_prior = TRUE,
                           iter = 2000,
                           warmup = 1000, 
                           cores = 4, chains = 4)

save(m.shrubDensityOBL2, file = "models/shrubDensityOBL2.rda")
load("models/shrubDensityOBL2.rda")
tidy(m.shrubDensityOBL2, effects = "fixed")
summary(m.shrubDensityOBL2)
conditional_effects(m.shrubDensityOBL2)
plot(m.shrubDensityOBL2)
pp_check(m.shrubDensityOBL2, ndraws = 100)
bayes_R2(m.shrubDensityOBL2)

m.shrubDensityOBL2_250 <- brm(data = shrub_density_250_OBLS,
                          presence ~ 1 + num_burn + year,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 2000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityOBL2_250, file = "models/shrubDensityOBL2_250.rda")
load("models/shrubDensityOBL2_250.rda")
tidy(m.shrubDensityOBL2_250, effects = "fixed")
summary(m.shrubDensityOBL2_250)
conditional_effects(m.shrubDensityOBL2_250)
plot(m.shrubDensityOBL2_250)
pp_check(m.shrubDensityOBL2_250, ndraws = 100)
bayes_R2(m.shrubDensityOBL2_250)

loo(m.shrubDensityOBL_250,m.shrubDensityOBL2_250)

m.shrubDensityOBL3 <- brm(data = shrub_density_OBLS,
                          presence ~ 1 + num_burn  + year + cool_warm_slope ,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 2000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityOBL3, file = "models/shrubDensityOBL3.rda")
load("models/shrubDensityOBL3.rda")

summary(m.shrubDensityOBL3)
conditional_effects(m.shrubDensityOBL3)
plot(m.shrubDensityOBL3)
pp_check(m.shrubDensityOBL3, ndraws = 100)
bayes_R2(m.shrubDensityOBL3)
loo(m.shrubDensityOBL, m.shrubDensityOBL2, m.shrubDensityOBL3)



datagrid(model=m.shrubDensityOBL3,
         num_burn=seq(1,6,by=.1)) %>% 
  add_epred_draws(m.shrubDensityOBL3,
                  re_formula = NA) %>% 
  ggplot(aes(x=num_burn, y=.epred))+
  stat_lineribbon()+
  scale_fill_brewer(palette = "Blues")

datagrid(model=m.shrubDensityOBL3,
         year=unique(shrub_density_OBLS$year)) %>% 
  add_epred_draws(m.shrubDensityOBL3,
                  re_formula = NA) %>% 
  ggplot(aes(x=year, y=.epred))+
  stat_gradientinterval(width = 0.25) +
  scale_fill_brewer(palette = "Blues")


#CREATE FIGURE
load("models/shrubDensityOBL2.rda")
summary(m.shrubDensityOBL2)
bayes_R2(m.shrubDensityOBL2)

## CALCULATE AREA UNDER THE CURVE
# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityOBL2, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_OBLS, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#area under the curve = 0.819 which seems super good!!
summary(m.shrubDensityOBL2)
tab_model(m.shrubDensityOBL2, transform = NULL)
tab_model(m.shrubDensityOBL2)

exp(-0.92)
plogis((-0.49 + -0.92)) - plogis(-0.49)
#fire frequency decreases obligate seedling species on average by 18%

nd <- shrub_density_OBLS %>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

OBL.fitted <- 
  fitted(m.shrubDensityOBL2, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd)

OBL.fitted.250 <- 
  fitted(m.shrubDensityOBL2_250, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd)


OBL.plot <- ggplot(OBL.fitted.250, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  xlim(1,6)+
  labs(x="Fire Frequency", y= "Proability of occurence",
       fill="credible interval")+
  scale_color_manual(values = c("mediumpurple", "seagreen4")) +
  scale_fill_manual(values = c("mediumpurple", "seagreen4")) +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))+
  guides(fill = FALSE)
OBL.plot


#####
#####
# BY INDIVIDUAL SPECIES

shrub_density_CEACUN <- shrub_density_250 %>%
  filter(spp == "CEACUN")
shrub_density_CEACUN$year <- as.factor(shrub_density_CEACUN$year)

m.shrubDensityCEACUN <- brm(data = shrub_density_CEACUN,
                          presence ~ 1 + num_burn + year,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 4000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityCEACUN, file = "models/shrubDensityCEACUN.rda")
load("models/shrubDensityCEACUN.rda")

summary(m.shrubDensityCEACUN)
conditional_effects(m.shrubDensityCEACUN)
bayes_R2(m.shrubDensityCEACUN)
pp_check(m.shrubDensityCEACUN, ndraws = 100)
plogis((1.45 + -1.10)) - plogis(1.45)


# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityCEACUN, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_CEACUN, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#area under the curve = 0.872 which seems super good!!

shrub_density_CEAOLI <- shrub_density_250 %>%
  filter(spp == "CEAOLI") 

shrub_density_CEAOLI$year <- as.factor(shrub_density_CEAOLI$year)

m.shrubDensityCEAOLI <- brm(data = shrub_density_CEAOLI,
                            presence ~ 1 + num_burn + year,
                            family = bernoulli(),
                            control = list(adapt_delta = 0.998, max_treedepth = 15),
                            prior = priors,
                            sample_prior = TRUE,
                            iter = 4000,
                            warmup = 1000, 
                            cores = 4, chains = 4)

save(m.shrubDensityCEAOLI, file = "models/shrubDensityCEAOLI.rda")
load("models/shrubDensityCEAOLI.rda")

summary(m.shrubDensityCEAOLI)
plogis((-0.49 + -1.20)) - plogis(-0.49)
conditional_effects(m.shrubDensityCEAOLI)
bayes_R2(m.shrubDensityCEAOLI)
pp_check(m.shrubDensityCEAOLI, ndraws = 100)

# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityCEAOLI, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_CEAOLI, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#area under the curve = 0.900 which seems super good!!

nd <- shrub_density_OBLS %>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

CEACUN.fitted <- 
  fitted(m.shrubDensityCEACUN, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) %>% 
  mutate(spp = "CEACUN")

CEACUN.plot <- ggplot(CEACUN.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  xlim(1,6)+
  labs(x="Fire Frequency", y= "Proability of occurence",
       fill="credible interval",
       title = "Ceanothus cuneatus")+
  scale_color_manual(values = c("#63605F", "#985E5C")) +
  scale_fill_manual(values = c("#63605F", "#985E5C")) +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        plot.title=element_text(face="italic"),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))+
  guides(fill = FALSE)
CEACUN.plot


CEAOLI.fitted <- 
  fitted(m.shrubDensityCEAOLI, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) %>% 
  mutate(spp = "CEAOLI")

CEAOLI.plot <- ggplot(CEAOLI.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  fill = as.factor(year), 
                  color = as.factor(year)),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate, color = as.factor(year)),
             size = 2/3)+
  xlim(1,6)+
  labs(x="Fire Frequency", y= "Proability of occurence",
       fill="credible interval",
       title = "Ceanothus oliganthus")+
  scale_color_manual(values = c("#63605F", "#985E5C")) +
  scale_fill_manual(values = c("#63605F", "#985E5C")) +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        plot.title=element_text(face="italic"),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text=element_text(size=12))+
  guides(fill = FALSE)
CEAOLI.plot



### LOOK AT TSLF
str(shrub_density_250_OBLS)
m.shrubDensityOBL_TSLF <- brm(data = shrub_density_OBLS,
                          presence ~ 1 + TSLF + year ,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 2000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityOBL_TSLF, file = "models/shrubDensityOBL_TSLF.rda")
load("models/shrubDensityOBL_TSLF.rda")

summary(m.shrubDensityOBL_TSLF)
conditional_effects(m.shrubDensityOBL_TSLF)
plot(m.shrubDensityOBL3)
pp_check(m.shrubDensityOBL3, ndraws = 100)
bayes_R2(m.shrubDensityOBL3)
loo(m.shrubDensityOBL, m.shrubDensityOBL2, m.shrubDensityOBL3)




## CALCULATE AREA UNDER THE CURVE
# Compute AUC for predicting Class with the model
Prob <- predict(Bayes_Model_Binary, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(ThaiEdu_New, REPEAT)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
