
### ------------------------------------------------------------------------------------------------------------

### 07/05/2024 - ©Fabio Benedetti (Plant Ecology group, IPS, Uni Bern)

### R script to re-map the SCA exploratory by adding: 
### - jitter in coordinates to better separate those that overlap
### - change some colours (yellow to light green and pink too yellow)
### - add polygon of the UNESCO's SCA biosphere to cut the map

# Basic libraries 
library("raster")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("lubridate")
library("viridis")
library("pals")
library("rasterVis")
library("maps")
library("ggrepel")

### ------------------------------------------------------------------------------------------------------------

# Define land cover types and 
covers <- data.frame(code = c(10,20,30,40,50,60,70,80,90,95,100), 
        Class = c("Tree cover","Shrubland","Grassland","Cropland",
                "Built-up","Bare","Snow & Ice","Permanent water bodies",
                "Herbaceous wetland","Mangroves","Moss & lichen"),
        RBG = c(rgb(0,100,0, maxColorValue = 255),
            rgb(255,187,34, maxColorValue = 255),
            rgb(255,255,76, maxColorValue = 255),
            rgb(240,150,255, maxColorValue = 255),
            rgb(250,0,0, maxColorValue = 255),
            rgb(180,180,180, maxColorValue = 255),
            rgb(240,240,240, maxColorValue = 255),
            rgb(0,100,200, maxColorValue = 255),
            rgb(0,150,160, maxColorValue = 255),
            rgb(0,207,117, maxColorValue = 255),
            rgb(250,230,160, maxColorValue = 255))
) # eo ddf 

# Define the position (long,lat) of the 3 Exploratories
explos <- data.frame(Name = c('Schorfheide-Chorin','Hainich-Dün','Schwäbische Alb'), 
        x = c(13.85,10.45,9.4),
        y = c(53.0,51.15,48.42)
)

# Spatial extent of the Schwäbische Alb exploratory
e.SCA <- as(extent(9.2,9.6,48.35,48.54), 'SpatialPolygons')
crs(e.SCA) <- "+proj=longlat +datum=WGS84 +no_defs"

# Get the landcover .tiff of the SCA exploratory 
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_SCHWALB/ESA_WorldCover_10m_2021_v200_N48E009_Map")
map <- raster::raster("ESA_WorldCover_10m_2021_v200_N48E009_Map.tif")
sub <- crop(map, e.SCA)
# Convert to data.frame and crop to extent
ddf <- as.data.frame(sub, xy = T)
rm(sub,map); gc()
colnames(ddf) <- c("Longitude","Latitude","Cover_type")

# Map cover types with appropriate colours
codes2keep <- unique(ddf$Cover_type); codes2keep
covers <- covers[which(covers$code %in% codes2keep),]; covers
cols <- c("10" = "#006400", "30" = "#FFFF4C", "40" = "#F096FF", "50" = "#FA0000", "60" = "#B4B4B4", "80" = "#0064C8", "90" = "#0096A0")

### And same map by adding the plots' location on top
map.sca.plots <- ggplot() + geom_tile(data = ddf,
        aes(x = Longitude, y = Latitude, fill = factor(Cover_type)), alpha = .5) + 
    geom_point(data = plots[plots$Exploratory == "ALB" & plots$EP_Plot_ID != 'na' & plots$VIP == "yes",],
        aes(x = Longitude, y = Latitude, shape = factor(Landuse)), colour = "black", fill = "black") + 
    geom_point(data = plots[plots$Exploratory == "ALB" & plots$EP_Plot_ID != 'na' & plots$VIP == "no",],
        aes(x = Longitude, y = Latitude, shape = factor(Landuse)), colour = "black", fill = "white") +    
    scale_shape_manual(name = "", values = c(24,22)) + 
    scale_fill_manual(name = "Cover type\n(WorldCover 2021)", values = cols) +
    xlab("Longitude") + ylab("Latitude") + coord_quickmap() + theme_minimal()

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024")
ggsave(plot = map.sca.plots, filename = "map_cover_types_10m_SCA+plots_24.04.24v2.jpg", dpi = 300, width = 7, height = 7)


### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------