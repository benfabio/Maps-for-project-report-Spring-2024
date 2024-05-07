
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

# Define the position (long,lat) of the 3 Exploratories
explos <- data.frame(Name = c('Schorfheide-Chorin','Hainich-Dün','Schwäbische Alb'), 
        x = c(13.85,10.45,9.4),
        y = c(53.0,51.15,48.42)
)

# Spatial extent of the Schwäbische Alb exploratory
e.SCA <- as(extent(9.2,9.6,48.35,48.54), 'SpatialPolygons')
crs(e.SCA) <- "+proj=longlat +datum=WGS84 +no_defs"


### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------