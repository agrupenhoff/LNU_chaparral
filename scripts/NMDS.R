library(vegan)
library(dplyr)
library(tidyverse)
library(ggrepel)

#Insert data
# NOTE data has species as column titles, with one column as "plot_year"

LNU_combine_wide <- read.csv("data/clean/LNU_species_all_wide.csv")

#make community matrix on which to base ordination
LNUspecies = LNU_combine_wide %>% 
  dplyr::select(!c("plot_year","X")) 

#columns that contain descriptive data (not species names)
LNUplot_ord = LNU_combine_wide %>% 
  dplyr::select(plot_year) %>% 
  separate(plot_year, c("PlotID", "year"), sep="_") 

plot.description.short <- plot.description_LNU %>% 
  dplyr::select(PlotID, num_burn, ppt, tmean, hli, cool_warm_slope, TSLF)

LNUplot_env <- left_join(LNUplot_ord, plot.description.short %>% distinct(), by = "PlotID")

set.seed(123) #reproduce same results
NMDS <- metaMDS(LNUspecies, distance="bray", k=3) #no transformation of species data is made here prior to bray curtis dissimilarities being calculated. 
NMDS

LNU.envfit <- envfit(NMDS, LNUplot_env, permutations = 999) # this fits environmental vectors


#env.fit
nmds_plot <- cbind(NMDS$point, LNUplot_env)
nmds_species <- cbind(as.data.frame(NMDS$species), 
                      as.data.frame(row.names(as.data.frame(NMDS$species)))) %>% 
  dplyr::rename(spp = "row.names(as.data.frame(NMDS$species))")
nmds_species <- left_join(nmds_species, species.short, by = "spp")
nmds_plot$num_burn <- as.factor(nmds_plot$num_burn)


#pull out dominant species 
LNU.spp.fit <- envfit(NMDS, LNUspecies, permutations = 999 )
spp.scrs <- as.data.frame(scores(LNU.spp.fit, display = "vectors")) 
#save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) 
#add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = LNU.spp.fit$vectors$pvals) 
#add pvalues to dataframe so you can select species which are significant
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)
nmds_plot
#plot
ggplot(nmds_plot, aes(x=MDS1, y=MDS2))+
  geom_point(size = 4, aes(x=MDS1, y=MDS2, color = num_burn))+
  geom_segment(data = sig.spp.scrs, 
               aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), 
               colour = "grey10", lwd=0.3) +
  #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, 
                          aes(x=NMDS1, y=NMDS2, label = Species), 
                          cex = 3, direction = "both", segment.size = 0.25)+ 
  #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  #geom_point(data= nmds_species, size = 1.8, alpha = 0.5, aes(x=MDS1, y=MDS2, 
  #                                               shape = Native_nonnative))+
  stat_ellipse(aes(x=MDS1, y=MDS2, color = num_burn))+
  #scale_color_viridis_d(option = "magma")+
  scale_color_manual(values = cal_palette("kelp1")) +
  guides(col=guide_legend("Fire frequency"))+
  theme(axis.text = element_text(size=15),
        text = element_text(size = 16),
        panel.grid = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=11))

ggsave(file="figures/fig4_NMDS.png", 
       width = 8, height = 6,
       dpi = 600)
