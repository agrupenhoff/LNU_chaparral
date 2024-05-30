
library(ggplot2)
library(brms)
library(dplyr)
library(rstan)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(emmeans)
library(calecopal)
library(modelr)
library(brmsmargins)
library(tidyr)
library(broom)            # Convert model objects to data frames
library(broom.mixed)  
library(sjPlot)
library(scales)
library(ROCR)
library(lme4)

shrub_ht <- read.csv("data/clean/shrub_height.csv")

plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_jan23.csv")
        species.short <- SpeciesList %>% 
          dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 

shrub_ht <- left_join(shrub_ht, plot.description_LNU, by="PlotID")
shrub_ht <- left_join(shrub_ht, species.short, by ="spp")


shrub_ht$hli.scale <- scale(shrub_ht$hli, center = TRUE, scale = TRUE)
shrub_ht$tmean.scale <- scale(shrub_ht$tmean, center = TRUE, scale = TRUE)
shrub_ht$ppt.scale <- scale(shrub_ht$ppt, center = TRUE, scale = TRUE)
shrub_ht$prefire_ht_m <- as.numeric(shrub_ht$prefire_ht_m)
shrub_ht$postfire_ht_m <- as.numeric(shrub_ht$postfire_ht_m)
shrub_ht$diam_largest_stem_cm <- as.numeric(shrub_ht$diam_largest_stem_cm)

#basic visualization of mortality data 2021-2022
resprout_mortality<- shrub_ht %>% 
  filter(status == "L") %>% 
  filter(fac.obl == "FAC") %>% 
  filter(spp != "AESCAL" &
           spp != "RIBMAL"&
           spp != "MIMAUR")

# NUM BURN ~ POST FIRE HEIGHT 
resprout_mortality %>% 
  filter(obs=="ARG") %>% 
  ggplot(aes(x=num_burn, y=postfire_ht_m, color=as.factor(spp)))+
  geom_smooth(method = "lm")+
  geom_jitter()
resprout_mortality %>% 
  filter(obs=="HDS") %>% 
  ggplot(aes(x=num_burn, y=postfire_ht_m, color=as.factor(spp)))+
  geom_smooth(method = "lm")+
  geom_jitter()

resprout_mortality %>% 
  filter(spp=="ADFA") %>% 
  filter(obs=="ARG") %>% 
  ggplot(aes(x=num_burn, y=postfire_ht_m, color=as.factor(spp)))+
  geom_smooth(method = "lm")+
  geom_jitter()
#generally we so no reduction in postfire resprout height due to 
#fire frequency

# NUM BURN ~ NUMBER OF RESPROUTS

resprout_mortality %>% 
  filter(spp=="ADFA") %>% 
  filter(obs=="ARG") %>% 
  group_by(PlotID, year, num_burn) %>% 
  tally()
  ggplot(aes(x=as.factor(num_burn), y= n, color= as.factor(year)))+
    geom_point()

# PREFIRE HT ~ POST FIRE HEIGHT
resprout_mortality %>% 
  ggplot(aes(x=prefire_ht_m, y=postfire_ht_m, color=as.factor(spp)))+
  geom_smooth(method = "lm")+
  geom_jitter()
#we don't really see an effect of prefire height here

# NUM BURN ~ PREFIRE HEIGHT
resprout_mortality %>% 
  ggplot(aes(x=num_burn, y=prefire_ht_m, color=as.factor(spp)))+
  geom_smooth(method = "lm")+
  geom_jitter()
#prefire height wayyy heigher in low FF areas. Unfortunatley 
#we don't have this for quail or cold canyon though.

resprout_mortality %>% 
  filter(spp=="ADFA") %>% 
  ggplot(aes(x=num_burn, y=postfire_ht_m, color=as.factor(year)))+
  geom_smooth(method = "lm")+
  geom_jitter()

ADFA_mortality_2021 <- resprout_mortality %>% 
  filter(spp=="ADFA") %>% 
  filter(obs== "HDS")

ADFA_mortality_2021 %>% 
  ggplot(aes(x=num_burn, y=postfire_ht_m, color=as.factor(year)))+
  geom_smooth(method = "lm")+
  geom_jitter()

mean(ADFA_mortality_2021$postfire_ht_m)

ADFA_mortality_2021 %>% 
  ggplot(aes(x=tmean.scale, y=postfire_ht_m, color=as.factor(year)))+
  geom_smooth(method = "lm")+
  geom_jitter()
ADFA_mortality_2021 %>% 
  ggplot(aes(x=prefire_ht_m, y=postfire_ht_m, color=as.factor()))+
  geom_smooth(method = "lm")+
  geom_jitter()
ADFA_mortality_2021 %>% 
  ggplot(aes(x=TSLF_bin, y=postfire_ht_m, color=as.factor(obs)))+
  geom_smooth(method = "lm")+
  geom_jitter()

hist((resprout_mortality$postfire_ht_m))
hist(sqrt(resprout_mortality$postfire_ht_m))


ADFA_resp_model <- brm(sqrt(postfire_ht_m) ~ 1 + num_burn, 
                      data=ADFA_mortality_2021, 
                      family=gaussian(),
                      prior = c(prior(normal(0,1), class = Intercept)),
                      chains=2,iter=2000, cores=4)
save(ADFA_resp_model, file = "models/ADFA_resp_model.rda")
load("models/ADFA_resp_model.rda")
ADFA_resp_model_TSLF <- brm(sqrt(postfire_ht_m) ~ 1 + TSLF_bin, 
                       data=ADFA_mortality_2021, 
                       family=gaussian(),
                       prior = c(prior(normal(0,1), class = Intercept)),
                       chains=2,iter=2000, cores=4)
save(ADFA_resp_model_TSLF, file = "models/ADFA_resp_model_TSLF.rda")
load("models/ADFA_resp_model_TSLF.rda")
ADFA_resp_model2 <- brm(sqrt(postfire_ht_m) ~ 1 + prefire_ht_m, 
                       data=ADFA_mortality_2021, 
                       family=gaussian(),
                       prior = c(prior(normal(0,1), class = Intercept)),
                       chains=2,iter=2000, cores=4)
save(ADFA_resp_model2, file = "models/ADFA_resp_model2.rda")
load("models/ADFA_resp_model2.rda")
ADFA_resp_model3 <- brm(sqrt(postfire_ht_m) ~ 1 + diam_largest_stem_cm, 
                        data=ADFA_mortality_2021, 
                        family=gaussian(),
                        prior = c(prior(normal(0,1), class = Intercept)),
                        chains=2,iter=2000, cores=4)
save(ADFA_resp_model3, file = "models/ADFA_resp_model3.rda")
load("models/ADFA_resp_model3.rda")
ADFA_resp_model4 <- brm(sqrt(postfire_ht_m) ~ 1 + hli.scale, 
                        data=ADFA_mortality_2021, 
                        family=gaussian(),
                        prior = c(prior(normal(0,1), class = Intercept)),
                        chains=2,iter=2000, cores=4)
save(ADFA_resp_model4, file = "models/ADFA_resp_model4.rda")
load("models/ADFA_resp_model4.rda")
ADFA_resp_model5 <- brm(sqrt(postfire_ht_m) ~ 1 + ppt.scale, 
                        data=ADFA_mortality_2021, 
                        family=gaussian(),
                        prior = c(prior(normal(0,1), class = Intercept)),
                        chains=2,iter=2000, cores=4)
save(ADFA_resp_model5, file = "models/ADFA_resp_model5.rda")
load("models/ADFA_resp_model5.rda")
ADFA_resp_model6 <- brm(sqrt(postfire_ht_m) ~ 1 + tmean.scale, 
                        data=ADFA_mortality_2021, 
                        family=gaussian(),
                        prior = c(prior(normal(0,1), class = Intercept)),
                        chains=2,iter=2000, cores=4)
save(ADFA_resp_model6, file = "models/ADFA_resp_model6.rda")
load("models/ADFA_resp_model6.rda")

loo(ADFA_resp_model6)
##NOW ADD UP COVARIATES THAT ADD ELPD
ADFA_resp_model7 <- brm(sqrt(postfire_ht_m) ~ 1 + num_burn +
                                             diam_largest_stem_cm +
                                              hli.scale,
                            data=ADFA_mortality_2021, 
                            family=gaussian(),
                            prior = c(prior(normal(0,1), class = Intercept)),
                            chains=2,iter=2000, cores=4)
save(ADFA_resp_model7, file = "models/ADFA_resp_model7.rda")
load("models/ADFA_resp_model7.rda")
print(ADFA_resp_model7)
loo(ADFA_resp_model7)


tab_model(ADFA_resp_model7)
print(ADFA_resp_model7)
plot(conditional_effects(ADFA_resp_model7), points =TRUE)
pp_check(ADFA_resp_model7, ndraws = 100)
bayes_R2(ADFA_resp_model7)



## perform one-sided hypothesis testing
resp_hypothesis <- c(resp_hypothesis = "num_burn < 0")
resp_answer <- hypothesis(ADFA_resp_model7, resp_hypothesis)
resp_answer
plot(resp_answer)

ADFA_resp_model7 %>% 
  gather_draws(b_Intercept, b_num_burn, b_diam_largest_stem_cm, b_hli.scale) %>% 
  ggplot(aes(x=.value, fill=.variable))+
  stat_halfeye(normalize = "xy")+
  facet_wrap(vars(.variable), scales = "free_x") 

ggplot(ADFA_mortality_2021, aes(x = num_burn, y = postfire_ht_m)) +
    geom_point(size = 1) + 
    geom_smooth(method = "lm", color = "#F012BE") 

ggplot(ADFA_mortality_2021, aes(x = hli, y = postfire_ht_m)) +
  geom_point(size = 1) + 
  geom_smooth(method = "lm", color = "#F012BE") 

tab_model(ADFA_resp_model7)
tidy(ADFA_resp_model7)
  # this tells us that a one burn increase in FF leads to a 13% decline 
  # in resproute height
tidy(ADFA_resp_model)
  # if temp isn't included, only a 4.4% decline in resprout height 


ADFA_resp_model7 |> 
  emmeans(~ num_burn, var = "num_burn", 
          regrid = "response", 
           type = "response",
          at = list(num_burn = seq(1,6, .01))) %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = (.value^2)) %>% 
  group_by(num_burn) %>% 
  median_hdi() %>% 
  ggplot(aes(x = num_burn)) +
  geom_smooth(aes(y = .value, 
                  ymin = .lower, ymax = .upper),
                  stat = "identity",
                    alpha = 1/4)+
  geom_point(aes(y = .value),
             size = 2/3)+
 #geom_jitter(data=ADFA_mortality_2021, 
#             aes(x=num_burn, y=(postfire_ht_m)), alpha =.2)+
  ylim(0,1)+
  labs(x="Fire Frequency", y= "Resprout Height (m)",
       fill="credible interval",
       title = "")+
  #scale_color_manual(values = c( "#985E5C")) +
  #cale_fill_manual(values = c( "#985E5C")) +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        plot.title=element_text(face="italic"),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "none")+
  guides(fill = FALSE)
  

ADFA_resp_model7 |> 
  emmeans(~ diam_largest_stem_cm, var = "diam_largest_stem_cm", 
          regrid = "response", 
          type = "response",
          at = list(diam_largest_stem_cm = seq(0,6, .01))) |> 
  gather_emmeans_draws() %>% 
  mutate(.value = (.value^2)) %>% 
  group_by(diam_largest_stem_cm) %>% 
  median_hdi() %>% 
  ggplot(aes(x = diam_largest_stem_cm)) +
  geom_smooth(aes(y = .value, 
                  ymin = .lower, ymax = .upper),
              stat = "identity", 
              fill =  "#985E5C",
              alpha = 1/4)+
  geom_point(aes(y = .value, color = "#985E5C" ),
             size = 2/3)+
  geom_point(data=ADFA_mortality_2021, 
             aes(x=diam_largest_stem_cm, y=(postfire_ht_m)),alpha=.2)+
  geom_jitter(data=ADFA_mortality_2021, 
              aes(x=diam_largest_stem_cm, y=(postfire_ht_m)), alpha =.2)+
  xlim(0,6)+
  labs(x="diam_largest_stem_cm", y= "Resprout Height (cm)",
       fill="credible interval",
       title = "")+
  scale_color_manual(values = c( "#985E5C")) +
  scale_fill_manual(values = c( "#985E5C")) +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom")+
  theme(panel.grid   = element_blank(),
        plot.title=element_text(face="italic"),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0, size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "none")+
  guides(fill = FALSE)



#total mortality 
shrub_mortality<- shrub_ht %>% 
  filter(fac.obl == "FAC") %>% 
  filter(spp != "AESCAL" &
           spp != "RIBMAL"&
           spp != "MIMAUR")

shrub_mortality %>% 
  filter(obs=="HDS") %>% 
  filter(spp == "ADFA") %>% 
  ggplot(aes(x=status, fill=as.factor(num_burn)))+
  geom_bar(position = "dodge")
  #geom_smooth(method = "lm")+
  geom_jitter()

