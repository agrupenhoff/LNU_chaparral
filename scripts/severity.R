
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(calecopal)
library(devtools)
library(tidybayes)
library(brms)

plot.description_LNU <- read.csv("data/clean/LNU_plot_description_CLEAN.csv")
severity_LNU <- read.csv("data/raw/Severity_LNU.csv")

severity_LNU_join <- left_join(severity_LNU, 
                          plot.description_LNU, by="PlotID")

severity_LNU_plot <- severity_LNU_join %>% 
  group_by(PlotID, year) %>% 
  dplyr::summarise(mean_diam_mm = mean(Mean_diam),
                   sd = sd(Mean_diam),
                   n_obs = n(),
                   se = sd/sqrt(n_obs),
                   max_diam_mm = max(Mean_diam),
                   cv_plot = 100*sd/mean_diam_mm)

severity_LNU_plot_join <- left_join(severity_LNU_plot, 
                               plot.description_LNU, by="PlotID")


# VISUALIZE RAW DATA ####
ggplot(data=severity_LNU_plot_join %>%  filter(num_burn != "2"), 
                       aes(x=as.factor(num_burn), y=mean_diam_mm))+
  geom_boxplot(fill = "#BD973D")+
  xlab("Fire frequency")+
  ylab("Diameter of stem terminus (mm)")+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=15),
        text = element_text(size = 16),
        panel.grid = element_blank())

# AVERAGE RAW DATA ####
sevdata <- ddply(severity_LNU_plot_join, c("num_burn"),
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

## CREATE FIGURE OF MEAN AND RAW DATA ####
severity_LNU <- ggplot(data=sevdata %>%  
         filter(num_burn != "2"), 
       aes(x=as.factor(num_burn), y=mean_diam_mm_avg))+
  geom_bar(stat = "identity", fill = "#BD973D")+
  geom_errorbar(aes(ymin=mean_diam_mm_avg-se, ymax=mean_diam_mm_avg+se), width=.1, size=.7)+
  geom_point(data=severity_LNU_plot_join%>%  
               filter(num_burn != "2"), 
             aes(x=as.factor(num_burn), y=mean_diam_mm),alpha=.4)+
  #scale_fill_manual(values = cal_palette("sierra1")) +
  xlab("Fire frequency")+
  ylab("Diameter of stem terminus (mm)")+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=15),
        text = element_text(size = 16),
        panel.grid = element_blank())
severity_LNU

ggsave("figures/fig2_severity.png", severity_LNU,
       #width = 10, 
       #height = 12,
       #units = 'in',
       dpi=600)

# MODEL SEVERITY ####
severity_LNU_plot_join$num_burn <- as.factor(severity_LNU_plot_join$num_burn)

fire_severity_brm <- brm(mean_diam_mm ~ num_burn,
                         data = severity_LNU_clean %>% filter(num_burn !="2"),
                         chains = 4, iter = 2000, warmup = 1000,
                         cores=4,
                         seed = 1234)
save(fire_severity_brm, file="models/fire_severity_brm.rda")
load("models/fire_severity_brm.rda")

summary(fire_severity_brm)
pp_check(fire_severity_brm, ndraws = 100)
conditional_effects(fire_severity_brm)
emmeans(fire_severity_brm , specs = pairwise ~ num_burn)

severity_LNU_clean %>% 
  ggplot(aes(x=as.factor(num_burn), y=mean_diam_mm))+
 # geom_point()+
  geom_boxplot()


# CV SEVERITY ####


