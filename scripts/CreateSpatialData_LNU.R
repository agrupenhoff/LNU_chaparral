library(terra)
library(sf)
library(spData)
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(tmap)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(viridis)
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")


lnu_plot <- read.csv("data/raw/Plot_Locations_LNU.csv")


lnu_plot_sf <- st_as_sf(lnu_plot, 
                          coords = c("X","Y"),
                          crs = 32610)
str(lnu_plot_sf)
#assign WGS84 projection which has EPSG code 
lnu_plot_sf <- st_set_crs(lnu_plot_sf, 26910)
st_crs(lnu_plot_sf)

#create bounding box around attribute data
bbox_lnu_plot <- st_bbox(lnu_plot_sf)
bbox_lnu_plot

ggplot()+
  geom_sf(data = lnu_plot_sf)

#save as shapefile
st_write(attributes_sf, "data/clean/fuels_spatialData", 
         driver = "ESRI Shapefile", delete_layer = TRUE)

#read in shapefile
attributes_sf <- st_read("data/clean/fuels_spatialData")
str(attributes_sf)


#import RESOLVE data
sf_use_s2(FALSE)
resolve_data <- st_read("/Users/ashleygrup/Documents/LOCAL_REPO_DATA/Ecoregions2017/Ecoregions2017.shp")
st_crs(resolve_data) <- 4326
st_crs(resolve_data)
st_crs(attributes_sf)


resolve_cropped <- st_crop(resolve_data, bbox_attributes)
str(resolve_cropped)


#get resolve dat for points 
attributes_resolvedata <- st_join(attributes_sf,
                                  left=FALSE,
                                  resolve_cropped)



#grab only ecoregion name and combine with clean attribute file
attributes_resolve_trim = attributes_resolvedata
st_geometry(attributes_resolve_trim)= NULL
attributes_resolve_trim <- attributes_resolve_trim %>% 
  select(UniqueID, ECO_NAME, BIOME_NAME) 


attributes_resolve_final <- left_join(attributes, attributes_resolve_trim, by = "UniqueID")
write.csv(attributes_resolve_final, "data/clean/attributes_resolve_final.csv", row.names = FALSE)


