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
shrub_density_250_FAC <- shrub_density_250 %>% 
  filter(fac.obl == "FAC") %>% 
  filter(spp != "MIMAUR")

shrub_density_FAC <- seedling_data %>%
  filter(fac.obl == "FAC") %>% 
  mutate(prop.presence= ifelse(prop.presence == 1, 0.999, prop.presence)) %>% 
  mutate(presence = ifelse(presence >0, 1, presence))

shrub_density_FAC$year <- as.factor(shrub_density_FAC$year)
shrub_density_250_FAC$year <- as.factor(shrub_density_250_FAC$year)

shrub_density_FAC %>% 
  dplyr::count(prop.presence == 0) %>% 
  mutate(prop = n/sum(n)) 


ggplot(shrub_density_FAC, aes(x=num_burn, y=presence,
                               color=spp))+
  geom_point()+
  geom_jitter()


ggplot(shrub_density_250_FAC, aes(x=num_burn, y=presence,
                              color=spp))+
  geom_point()+
  geom_jitter()


priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("student_t(3, 0, 1)", class = "Intercept"))
# only 1 coef for this. t(3,0,1) is aki vehtarhi's favorite weakly informative prior.
# I changed this from t(3, 0, 2.5)

m.shrubDensityFAC <- brm(data = shrub_density_FAC,
                         presence ~ 1 + num_burn,
                         family = bernoulli(),
                         control = list(adapt_delta = 0.998, max_treedepth = 15),
                         prior = priors,
                         sample_prior = TRUE,
                         iter = 2000,
                         warmup = 1000, 
                         cores = 4, chains = 4)

save(m.shrubDensityFAC, file = "models/shrubDensityFAC.rda")
load("models/shrubDensityFAC.rda")

tidy(m.shrubDensityFAC, effects = "fixed")
summary(m.shrubDensityFAC)
conditional_effects(m.shrubDensityFAC)
plot(m.shrubDensityFAC)
pp_check(m.shrubDensityFAC, ndraws = 100)
bayes_R2(m.shrubDensityFAC)

m.shrubDensityFAC_250 <- brm(data = shrub_density_250_FAC,
                             presence ~ 1 + num_burn,
                             family = bernoulli(),
                             control = list(adapt_delta = 0.998, max_treedepth = 15),
                             prior = priors,
                             sample_prior = TRUE,
                             iter = 2000,
                             warmup = 1000, 
                             cores = 4, chains = 4)

save(m.shrubDensityFAC_250, file = "models/shrubDensityFAC_250.rda")
load("models/shrubDensityFAC_250.rda")

tidy(m.shrubDensityFAC_250, effects = "fixed")
summary(m.shrubDensityFAC_250)
conditional_effects(m.shrubDensityFAC_250)
plot(m.shrubDensityFAC_250)
pp_check(m.shrubDensityFAC_250, ndraws = 100)
bayes_R2(m.shrubDensityFAC_250)


m.shrubDensityFAC2 <- brm(data = shrub_density_FAC,
                          presence ~ 1 + num_burn + year,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 2000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityFAC2, file = "models/shrubDensityFAC2.rda")
load("models/shrubDensityFAC2.rda")
tidy(m.shrubDensityFAC2, effects = "fixed")
summary(m.shrubDensityFAC2)
conditional_effects(m.shrubDensityFAC2)
plot(m.shrubDensityFAC2)
pp_check(m.shrubDensityFAC2, ndraws = 100)
bayes_R2(m.shrubDensityFAC2)

m.shrubDensityFAC2_250 <- brm(data = shrub_density_250_FAC,
                              presence ~ 1 + num_burn + year,
                              family = bernoulli(),
                              control = list(adapt_delta = 0.998, max_treedepth = 15),
                              prior = priors,
                              sample_prior = TRUE,
                              iter = 2000,
                              warmup = 1000, 
                              cores = 4, chains = 4)

save(m.shrubDensityFAC2_250, file = "models/shrubDensityFAC2_250.rda")
load("models/shrubDensityFAC2_250.rda")
tidy(m.shrubDensityFAC2_250, effects = "fixed")
summary(m.shrubDensityFAC2_250)
conditional_effects(m.shrubDensityFAC2_250)
plot(m.shrubDensityFAC2_250)
pp_check(m.shrubDensityFAC2_250, ndraws = 100)
bayes_R2(m.shrubDensityFAC2_250)

loo(m.shrubDensityFAC,m.shrubDensityFAC2)
loo(m.shrubDensityFAC_250,m.shrubDensityFAC2_250)

m.shrubDensityFAC3 <- brm(data = shrub_density_FAC,
                          presence ~ 1 + num_burn + year + cool_warm_slope,
                          family = bernoulli(),
                          control = list(adapt_delta = 0.998, max_treedepth = 15),
                          prior = priors,
                          sample_prior = TRUE,
                          iter = 2000,
                          warmup = 1000, 
                          cores = 4, chains = 4)

save(m.shrubDensityFAC3, file = "models/shrubDensityFAC3.rda")
load("models/shrubDensityOBL3.rda")

summary(m.shrubDensityFAC3)
conditional_effects(m.shrubDensityFAC3)
plot(m.shrubDensityFAC3)
pp_check(m.shrubDensityFAC3, ndraws = 100)
bayes_R2(m.shrubDensityFAC3)
loo(m.shrubDensityFAC,m.shrubDensityFAC2, m.shrubDensityFAC3)


datagrid(model=m.shrubDensityFAC2_250,
         num_burn=seq(1,6,by=.1)) %>% 
  add_epred_draws(m.shrubDensityFAC2,
                  re_formula = NA) %>% 
  ggplot(aes(x=num_burn, y=.epred))+
  stat_lineribbon()+
  scale_fill_brewer(palette = "Blues")

datagrid(model=m.shrubDensityFAC2,
         year=unique(shrub_density_FAC$year)) %>% 
  add_epred_draws(m.shrubDensityFAC2,
                  re_formula = NA) %>% 
  ggplot(aes(x=year, y=.epred))+
  stat_gradientinterval(width = 0.25) +
  scale_fill_brewer(palette = "Blues")


# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityFAC2_250, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_250_FAC, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#CREATE FIGURE
summary(m.shrubDensityFAC2_250)
plogis((-0.47 + -0.33)) - plogis(-0.47)

nd <- shrub_density_FAC %>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

FAC.fitted.250 <- 
  fitted(m.shrubDensityFAC2_250, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd)


FAC.plot <- ggplot(FAC.fitted.250, aes(x=num_burn))+
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
FAC.plot


#####
#####
# BY INDIVIDUAL SPECIES

shrub_density_ADFA <- shrub_density_250 %>%
  filter(spp == "ADFA")
shrub_density_ADFA$year <- as.factor(shrub_density_ADFA$year)
str(shrub_density_ADFA)

m.shrubDensityADFA <- brm(data = shrub_density_ADFA,
                            presence ~ 1 + num_burn + year,
                            family = bernoulli(),
                            control = list(adapt_delta = 0.998, max_treedepth = 15),
                            prior = priors,
                            sample_prior = TRUE,
                            iter = 4000,
                            warmup = 1000, 
                            cores = 4, chains = 4)

save(m.shrubDensityADFA, file = "models/shrubDensityADFA.rda")
load("models/shrubDensityADFA.rda")

summary(m.shrubDensityADFA)
conditional_effects(m.shrubDensityADFA)
bayes_R2(m.shrubDensityADFA)
pp_check(m.shrubDensityADFA, ndraws = 100)

# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityADFA, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_ADFA, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#CREATE FIGURE
summary(m.shrubDensityADFA)
plogis((1.20 + -0.70)) - plogis(1.20)

shrub_density_ERICAL<- shrub_density_250 %>%
  filter(spp == "ERICAL") 
shrub_density_ERICAL$year <- as.factor(shrub_density_ERICAL$year)

m.shrubDensityERICAL <- brm(data = shrub_density_ERICAL,
                            presence ~ 1 + num_burn + year,
                            family = bernoulli(),
                            control = list(adapt_delta = 0.998, max_treedepth = 15),
                            prior = priors,
                            sample_prior = TRUE,
                            iter = 4000,
                            warmup = 1000, 
                            cores = 4, chains = 4)

save(m.shrubDensityERICAL, file = "models/shrubDensityERICAL.rda")
load("models/shrubDensityERICAL.rda")

summary(m.shrubDensityERICAL)
conditional_effects(m.shrubDensityERICAL)
bayes_R2(m.shrubDensityERICAL)
pp_check(m.shrubDensityERICAL, ndraws = 100)
# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityERICAL, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_ERICAL, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
summary(m.shrubDensityERICAL)
plogis((-1.03 + 0.05)) - plogis(-1.03)

shrub_density_LEPCAL<- shrub_density_250 %>%
  filter(spp == "LEPCAL") 
shrub_density_LEPCAL$year <- as.factor(shrub_density_LEPCAL$year)

m.shrubDensityLEPCAL <- brm(data = shrub_density_LEPCAL,
                            presence ~ 1 + num_burn + year,
                            family = bernoulli(),
                            control = list(adapt_delta = 0.998, max_treedepth = 15),
                            prior = priors,
                            sample_prior = TRUE,
                            iter = 4000,
                            warmup = 1000, 
                            cores = 4, chains = 4)

save(m.shrubDensityLEPCAL, file = "models/shrubDensityLEPCAL.rda")
load("models/shrubDensityLEPCAL.rda")

summary(m.shrubDensityLEPCAL)
conditional_effects(m.shrubDensityLEPCAL)
bayes_R2(m.shrubDensityLEPCAL)
pp_check(m.shrubDensityLEPCAL, ndraws = 100)

# Compute AUC for predicting Class with the model
Prob <- predict(m.shrubDensityLEPCAL, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(shrub_density_LEPCAL, presence)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
#CREATE FIGURE
summary(m.shrubDensityLEPCAL)
plogis((0.16 + -0.66)) - plogis(0.16)

nd <- shrub_density_FAC%>% 
  data_grid(num_burn=seq(1,6,by=.01),
            year=c(2021, 2022))

ADFA.fitted <- 
  fitted(m.shrubDensityADFA, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) %>% 
  mutate(spp = "ADFA")

ADFA.plot <- ggplot(ADFA.fitted, aes(x=num_burn))+
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
       title = "Adenostoma fasciculatum")+
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
ADFA.plot


ERICAL.fitted <- 
  fitted(m.shrubDensityERICAL, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) %>% 
  mutate(spp = "ERICAL")

ERICAL.plot <- ggplot(ERICAL.fitted, aes(x=num_burn))+
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
       title = "Eriodictyon californicum")+
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
ERICAL.plot

LEPCAL.fitted <- 
  fitted(m.shrubDensityLEPCAL, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd) %>% 
  mutate(spp = "LEPCAL")

LEPCAL.plot <- ggplot(LEPCAL.fitted, aes(x=num_burn))+
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
       title = "Lepechinia calycina")+
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
LEPCAL.plot
