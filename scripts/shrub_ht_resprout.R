
library(ggplot2)
library(brms)
library(dplyr)


shrub_ht <- read.csv("data/clean/shrub_height.csv")
plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
SpeciesList <- read.csv("data/raw/SpeciesList_jan23.csv")
        species.short <- SpeciesList %>% 
          dplyr::select(spp, Lifeform, Native_nonnative, fac.obl) 

shrub_ht <- left_join(shrub_ht, plot.description_LNU, by="PlotID")
shrub_ht <- left_join(shrub_ht, species.short, by ="spp")

unique(shrub_ht$spp)


shrub_ht %>% 
  filter(seedling_resp == "SEEDLING") %>% 
  ggplot(aes(x=num_burn, y=avg.ht, color=fac.obl))+
  geom_smooth(method = "lm")+
  geom_jitter()


resprout_ht_FAC <- shrub_ht %>% 
  filter(fac.obl == "FAC" &
          seedling_resp == "RESPROUT" &
           spp != "MIMAUR") %>% 
  filter(avg.ht > 0) %>% 
  filter(year == "2022")

resprout_ht_FAC %>% 
  ggplot(aes(x=num_burn, y=avg.ht))+
  geom_smooth(method = "lm")+
  geom_jitter()

resprout_ht_FAC %>% 
  ggplot(aes(x=cool_warm_slope, y=avg.ht))+
  geom_boxplot()+
  geom_jitter()

hist(resprout_ht_FAC$avg.ht)

#### GAMMA MODEL 
#values are contious, cannot be less than zero, and variance increases with the mean

plot(rgamma(100,shape = 0.01, scale = 0.01))

priors <- c(set_prior("normal(0,1)", class = "b"),
            set_prior("normal(0,1)", class = "Intercept"),
            set_prior("gamma(0.01,0.01)",class="shape"))


FAC_resp_model <- brm(avg.ht ~  num_burn, 
                      data=resprout_ht_FAC, 
                      family=Gamma(link="log"),
                  prior=priors,
                  chains=2,iter=1000, cores=4)
save(FAC_resp_model, file = "models/FAC_resp_model.rda")

print(FAC_resp_model)
plot(conditional_effects(FAC_resp_model), points =TRUE)
pp_check(FAC_resp_model, ndraws = 100)

FAC_resp_model_tmean <- brm(avg.ht ~  num_burn + tmean, 
                      data=resprout_ht_FAC, 
                      family=Gamma(link="log"),
                      prior=priors,
                      chains=2,iter=1000, cores=4)

save(FAC_resp_model_slope, file = "models/FAC_resp_model_slope.rda")

print(FAC_resp_model_slope)
plot(conditional_effects(FAC_resp_model_slope), points =TRUE)
pp_check(FAC_resp_model_slope, ndraws = 100)
loo(FAC_resp_model, FAC_resp_model_slope)

FAC_resp_model_yr <- brm(avg.ht ~  num_burn + year +tmean, 
                         data=resprout_ht_FAC, 
                         family=Gamma(link="log"),
                         prior=priors,
                         chains=2,iter=1000, cores=4)

save(FAC_resp_model_yr, file = "models/FAC_resp_model_yr.rda")

print(FAC_resp_model_yr)
plot(conditional_effects(FAC_resp_model_yr), points =TRUE)
pp_check(FAC_resp_model_yr, ndraws = 100)


loo(FAC_resp_model, FAC_resp_model_yr)


nd <- resprout_ht_FAC %>% 
  data_grid(num_burn=seq(1,6,by=.01))

FAC_RESP.fitted <- 
  fitted(FAC_resp_model, 
         newdata= nd,
         scale = "response",
         probs = c(0.05, 0.95)) %>% 
  as.data.frame() %>% 
  cbind(nd)

FAC_RESP.plot <- ggplot(FAC_RESP.fitted, aes(x=num_burn))+
  geom_smooth(aes(y = Estimate, 
                  ymin = Q5, ymax = Q95,
                  #fill = as.factor(year), 
                  #color = as.factor(year)
                  ),
              stat = "identity", 
              alpha = 1/4, size = 1/2)+
  geom_point(aes(y = Estimate),
             size = 2/3)+
  xlim(1,6)+
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
        legend.text=element_text(size=12))+
  guides(fill = FALSE)
FAC_RESP.plot
