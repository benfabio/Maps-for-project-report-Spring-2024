
### ------------------------------------------------------------------------------------------------------------

### 10/04/2024 - ©Fabio Benedetti (Plant Ecology group, IPS, Uni Bern)

### R script to use the fine resolution (10 m) layers from EESA's WorldCover database (https://viewer.esa-worldcover.org/) and overlay the spatial locations of the plots on top.
### Make maps for the project's report (due April-May 2024) and combine them with a simple map from Germany 
### Add spatial boxes (or just green points) representing the 3 different Exploratories' position 
### Add position of the grassland and forest EPs as white points on top of the maps

# Basic libraries 
#install.packages("rasterVis")
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

### 1°) Make the simple map of Germany and add the position fo the 3 exploratories with their names (ggrepel?)
deut <- map_data("world", region = "Germany")

ggplot(deut, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", colour = "black") +
  coord_quickmap() + theme_void()

# Define the position (long,lat) of the 3 Exploratories
explos <- data.frame(Name = c('Schorfheide-Chorin','Hainich-Dün','Schwäbische Alb'), 
        x = c(13.85,10.45,9.4),
        y = c(53.0,51.15,48.42)
)
# Or define their bounding boxes based on Fig. 1 of Bazzichetto et al. (2024)

# Schorfheide-Chorin/ NE Germany
e.SCH <- as(extent(13.5,14.1,52.8,53.16), 'SpatialPolygons')
crs(e.SCH) <- "+proj=longlat +datum=WGS84 +no_defs" # same as the WorldCover's products' CRS

# Hainich-Dün/ Central Germany
e.HAD <- as(extent(10.1,10.8,50.9,51.4), 'SpatialPolygons')
crs(e.HAD) <- "+proj=longlat +datum=WGS84 +no_defs"

# Schwäbische Alb/ SW Germany
e.SCA <- as(extent(9.2,9.6,48.35,48.51), 'SpatialPolygons')
crs(e.SCA) <- "+proj=longlat +datum=WGS84 +no_defs"

# Map of Germany with exploratories as points
ggplot() + geom_polygon(data = deut, aes(x = long, y = lat, group = group), fill = "grey95", colour = "black") +
  geom_point(aes(x = x, y = y), size = 2, pch = 21, colour = "black", fill = "#a6d96a", data = explos) + 
  geom_text_repel(aes(x = x, y = y, label = Name), data = explos) + 
  coord_quickmap() + theme_void()

# Map of Germany with exploratories as spatial boxes
p <- ggplot() + geom_polygon(data = deut, aes(x = long, y = lat, group = group), fill = "grey95", colour = "black") +
    geom_rect(aes(xmax = 14.1, xmin = 13.5, ymax = 53.16, ymin = 52.8), fill = "#a6d96a", alpha = .5, colour = "#1a9850") + 
    geom_rect(aes(xmax = 10.8, xmin = 10.1, ymax = 51.4, ymin = 50.9), fill = "#a6d96a", alpha = .5, colour = "#1a9850") + 
    geom_rect(aes(xmax = 9.6, xmin = 9.2, ymax = 48.51, ymin = 48.35), fill = "#a6d96a", alpha = .5, colour = "#1a9850") + 
    #geom_text_repel(aes(x = x, y = y, label = Name), data = explos) + 
    coord_quickmap() + theme_void()
# Save
ggsave(plot = p, filename = "map_deutschland_17.04.24.jpg", dpi = 300, width = 3, height = 4)

### ------------------------------------------------------------------------------------------------------------

### 2°) Download the land cover layers of interest from: https://viewer.esa-worldcover.org/worldcover/ or https://viewer.terrascope.be/
### For each of the exploratories: 
# Download the relevant .tif
# Crop to proper bounding box (otherwise too heavy for plotting in ggplot)
# Use 'covers' code below to use official land cover types' colours (those from EESA's WorldCover)

# To associate the numerical map code to an actual land cover class: https://worldcover2020.esa.int/data/docs/WorldCover_PUM_V1.1.pdf
# Retrieved the rgb color code too to use the same standard colors as in WorldCover 2021 V2
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

### Also load the spatial coordinates of the grassland/forest experimental plots (EPs, MIPs, VIPs)
### to overlay them on cover type maps.

### ----------------------------------------------

### Option A) from: 
# https://stackoverflow.com/questions/33359284/r-plot-background-map-from-geotiff-with-ggplot2
# map <- raster::raster("ESA_WorldCover_10m_2021_v200_N51E012_Map.tif")
# map # class(map)
# plot(map)
# str(map)

# gplot(map, maxpixels = 5e5) +
#   geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
#   scale_fill_gradientn(name = "Species\nrichness",colours = parula(100), guide = "colourbar") +
#   coord_equal()
# Works well! But we can do better...

  
### Option B) Convert raster to to ddf and use ggplot2
### Issue, the raster is too big for now (too large extent) --> need to crop first based on a boundary box
### Use the bounding coordinates from Fig. 1 of Bazzichetto et al. (2024) 

### B.1) SCH
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_SCHORF")
map <- raster::raster("ESA_WorldCover_10m_2021_v200_N51E012_Map.tif")
map # class(map)
# Crop based on geographical extent of SCH exploratory
sub <- crop(map, e.SCH)
# sub
# plot(sub)
gplot(sub, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
  scale_fill_gradientn(name = "", colours = parula(20), guide = "colourbar") +
  coord_quickmap() + theme_void()
### Nice. You can visually identify the same features as in Fig. 1 of Bazzichetto et al. (2024) (orange are water bodies)

# Convert to ddf
ddf <- as.data.frame(sub, xy = T)
rm(sub,map); gc()
# dim(ddf); str(ddf); summary(ddf)
# unique(ddf$ESA_WorldCover_10m_2021_v200_N51E012_Map) # 10 40 30 80 50 90 60 need to associate those values to cover types later
colnames(ddf) <- c("Longitude","Latitude","Cover_type")
# !!! Watchout, resolution is 10m so plot below takes a long time to display! 
# ggplot(ddf) + geom_tile(aes(x = Longitude, y = Latitude, fill = factor(Cover_type))) +
#     scale_fill_brewer(name = "Cover type\n(WorldCover V2)", palette = "Paired") +
#     xlab("Longitude") + ylab("Latitude") + coord_quickmap() + theme_void()
    
# Use 'covers' to set colour palette manually and map again 
codes2keep <- unique(ddf$Cover_type); codes2keep
covers <- covers[which(covers$code %in% codes2keep),]; covers
cols <- c("10" = "#006400", "30" = "#FFFF4C", "40" = "#F096FF", "50" = "#FA0000", "60" = "#B4B4B4", "80" = "#0064C8", "90" = "#0096A0")
ggplot(ddf) + geom_tile(aes(x = Longitude, y = Latitude, fill = factor(Cover_type))) + 
    scale_fill_manual(name = "Cover type\n(WorldCover 2021)", values = cols) +
    xlab("Longitude") + ylab("Latitude") + coord_quickmap() + theme_minimal()


### B.2) Hainich-Dun
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_HAINICH/ESA_WorldCover_10m_2021_v200_N48E009_Map")
map1 <- raster::raster("ESA_WorldCover_10m_2021_v200_N48E009_Map.tif")
# map1 
sub1 <- crop(map1, e.HAD)
gplot(sub1, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
  scale_fill_gradientn(name = "", colours = parula(20), guide = "colourbar") +
  coord_quickmap() + theme_void()

# And need both layers need to be joined
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_HAINICH/ESA_WorldCover_10m_2021_v200_N51E009_Map")
map2 <- raster::raster("ESA_WorldCover_10m_2021_v200_N51E009_Map.tif")
# map # class(map)
sub2 <- crop(map2, e.HAD)
gplot(sub2, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
  scale_fill_gradientn(name = "", colours = parula(20), guide = "colourbar") +
  coord_quickmap() + theme_void()

# Extent of e.HAD actually covers both layers. Need to merge/join them.
# Can do that after concerting to data.frame
ddf1 <- as.data.frame(sub1, xy = T)
ddf2 <- as.data.frame(sub2, xy = T)
rm(sub1,map1,sub2,map2); gc()
colnames(ddf1) <- c("Longitude","Latitude","Cover_type")
colnames(ddf2) <- c("Longitude","Latitude","Cover_type")
ddf <- rbind(ddf1,ddf2)
# dim(ddf); str(ddf); head(ddf)
# summary(ddf) # to check coordinates extent
rm(ddf1,ddf2); gc()

# Map cover types with appropriate colours
codes2keep <- unique(ddf$Cover_type); codes2keep
covers <- covers[which(covers$code %in% codes2keep),]; covers
cols <- c("10" = "#006400", "30" = "#FFFF4C", "40" = "#F096FF", "50" = "#FA0000", "60" = "#B4B4B4", "80" = "#0064C8", "90" = "#0096A0")
ggplot(ddf) + geom_tile(aes(x = Longitude, y = Latitude, fill = factor(Cover_type))) + 
    scale_fill_manual(name = "Cover type\n(WorldCover 2021)", values = cols) +
    xlab("Longitude") + ylab("Latitude") + coord_quickmap() + theme_minimal()


### B.3) Schabisch Alb
# setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_SCHWALB/ESA_WorldCover_10m_2021_v200_N48E006_Map/")
# map1 <- raster::raster("ESA_WorldCover_10m_2021_v200_N48E006_Map.tif")
# map1 # check extent and compare to e.SCA
# sub1 <- crop(map1, e.SCA)
# gplot(sub1, maxpixels = 5e5) + 
#   geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
#   scale_fill_gradientn(name = "", colours = parula(20), guide = "colourbar") +
#   coord_quickmap() + theme_void()
### Layer above is actually not part of the extent of the SCA exploratory

# Only need this one layer 
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/BEO-UniBern/Maps for project report Spring 2024/WORLDCOVER_MAPS_SCHWALB/ESA_WorldCover_10m_2021_v200_N48E009_Map")
map2 <- raster::raster("ESA_WorldCover_10m_2021_v200_N48E009_Map.tif")
sub2 <- crop(map2, e.SCA)
gplot(sub2, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
  scale_fill_gradientn(name = "", colours = parula(20), guide = "colourbar") +
  coord_quickmap() + theme_void()

# Extent of e.HAD actually covers both layers. Need to merge/join them.
# Can do that after concerting to data.frame
#ddf1 <- as.data.frame(sub1, xy = T)
ddf <- as.data.frame(sub2, xy = T)
rm(sub2,map2); gc()
colnames(ddf) <- c("Longitude","Latitude","Cover_type")
# summary(ddf)

# Map cover types with appropriate colours
codes2keep <- unique(ddf$Cover_type); codes2keep
covers <- covers[which(covers$code %in% codes2keep),]; covers
cols <- c("10" = "#006400", "30" = "#FFFF4C", "40" = "#F096FF", "50" = "#FA0000", "60" = "#B4B4B4", "80" = "#0064C8", "90" = "#0096A0")
ggplot(ddf) + geom_tile(aes(x = Longitude, y = Latitude, fill = factor(Cover_type))) + 
    scale_fill_manual(name = "Cover type\n(WorldCover 2021)", values = cols) +
    xlab("Longitude") + ylab("Latitude") + coord_quickmap() + theme_minimal()

    
### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------
