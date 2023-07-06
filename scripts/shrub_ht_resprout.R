
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

shrub_ht <- read.csv("data/clean/shrub_height.csv")
plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_jan23.csv")
        species.short <- SpeciesList %>% 
          dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 

shrub_ht <- left_join(shrub_ht, plot.description_LNU, by="PlotID")
shrub_ht <- left_join(shrub_ht, species.short, by ="spp")

shrub_ht$ppt.scale <- scale(shrub_ht$ppt, 
                                   center = TRUE, scale = TRUE)
shrub_ht$tmean.scale <- scale(shrub_ht$tmean, 
                                     center = TRUE, scale = TRUE)
shrub_ht$hli.scale <- scale(shrub_ht$hli, 
                                   center = TRUE, scale = TRUE)

unique(shrub_ht$spp)


resprout_ht_FAC <- shrub_ht %>% 
  filter(fac.obl == "FAC" &
          seedling_resp == "RESPROUT" &
           spp != "MIMAUR") %>% 
  filter(avg.ht > 0) %>% 
  filter(year == "2022") %>% 
  mutate(log_ht = log(avg.ht))

resprout_ht_FAC %>% 
  ggplot(aes(x=num_burn, y=avg.ht, color = spp))+
  geom_smooth(method = "lm")+
  geom_jitter()



hist((resprout_ht_FAC$avg.ht))
hist(rnorm(1000, mean=0, sd=1))

FAC_resp_model <- brm(log(avg.ht) ~ 1 + num_burn, 
                      data=resprout_ht_FAC, 
                      family=gaussian(),
                      prior = c(prior(normal(0,1), class = Intercept)),
                     chains=2,iter=1000, cores=4)
save(FAC_resp_model, file = "models/FAC_resp_model.rda")
load("models/FAC_resp_model.rda")

tab_model(FAC_resp_model)
print(FAC_resp_model)
plot(conditional_effects(FAC_resp_model), points =TRUE)
pp_check(FAC_resp_model, ndraws = 100)
bayes_R2(FAC_resp_model)

FAC_resp_model_tmean <- brm(log(avg.ht) ~  num_burn + tmean.scale, 
                      data=resprout_ht_FAC, 
                      family=gaussian(),
                      prior = c(prior(normal(0,1), class = Intercept)),
                      chains=2,iter=1000, cores=4)

save(FAC_resp_model_tmean, file = "models/FAC_resp_model_tmean.rda")
load(file = "models/FAC_resp_model_tmean.rda")

print(FAC_resp_model_tmean)
tab_model(FAC_resp_model_tmean)
plot(conditional_effects(FAC_resp_model_tmean), points =TRUE)
pp_check(FAC_resp_model_tmean, ndraws = 100)
bayes_R2(FAC_resp_model_tmean)
loo(FAC_resp_model, FAC_resp_model_tmean)


FAC_resp_model_tmean_hli <- brm(log_ht ~  num_burn + tmean.scale + hli.scale, 
                         data=resprout_ht_FAC, 
                         family=gaussian(),
                         prior = c(prior(normal(0,1), class = Intercept)),
                         chains=2,iter=1000, cores=4)

save(FAC_resp_model_tmean_hli, file = "models/FAC_resp_model_tmean_hli.rda")
load(file = "models/FAC_resp_model_tmean_hli.rda")

loo(FAC_resp_model, FAC_resp_model_tmean, FAC_resp_model_tmean_hli)
loo(FAC_resp_model_tmean_hli)
kfold(FAC_resp_model_tmean_hli, K=10)
median(loo_R2(FAC_resp_model_tmean_hli))

tab_model(FAC_resp_model_tmean)
print(FAC_resp_model_tmean)
plot(conditional_effects(FAC_resp_model_tmean), points =TRUE)
pp_check(FAC_resp_model_tmean, ndraws = 100)
bayes_R2(FAC_resp_model_tmean)

## perform one-sided hypothesis testing
resp_hypothesis <- c(resp_hypothesis = "num_burn < 0")
resp_answer <- hypothesis(FAC_resp_model_tmean, resp_hypothesis)
resp_answer
plot(resp_answer)

FAC_resp_model_tmean %>% 
  gather_draws(b_Intercept, b_num_burn, b_tmean.scale) %>% 
  ggplot(aes(x=.value, fill=.variable))+
  stat_halfeye(normalize = "xy") 
  facet_wrap(vars(.variable), scales = "free_x") 

ggplot(resprout_ht_FAC, aes(x = num_burn, y = log_ht)) +
    geom_point(size = 1) + 
    geom_smooth(method = "lm", color = "#F012BE") +
    scale_y_continuous(labels = label_math(e^.x))

tab_model(FAC_resp_model_tmean)
tidy(FAC_resp_model_tmean)
  # this tells us that a one burn increase in FF leads to a 13% decline 
  # in resproute height
tidy(FAC_resp_model)
  # if temp isn't included, only a 4.4% decline in resprout height 


FAC_resp_model_tmean |> 
  emmeans(~ num_burn, var = "num_burn", 
          regrid = "response", 
          tran = "log", type = "response",
          at = list(num_burn = seq(1,6, .01))) |> 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% 
  group_by(num_burn) %>% 
  median_hdi() %>% 
  ggplot(aes(x = num_burn)) +
  geom_smooth(aes(y = .value, 
                  ymin = .lower, ymax = .upper),
                  stat = "identity", 
                  fill =  "#985E5C",
                    alpha = 1/4, size = 1/2)+
  geom_point(aes(y = .value, color = "#985E5C" ),
             size = 2/3)+
  geom_point(data=resprout_ht_FAC, 
             aes(x=num_burn, y=(avg.ht)),alpha=.2)+
  geom_jitter(data=resprout_ht_FAC, 
              aes(x=num_burn, y=(avg.ht)), alpha =.2)+
  labs(x="Fire Frequency", y= "Resprout Height (cm)",
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
  



  
  