# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located

GISlocation <- "C:/Users/thijs/Desktop/APCE2024"
setwd(GISlocation)

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot
library(raster)     # convert terra objects to raster data if terra does not work directly
library(cowplot)   # issues with patchwork
# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes

barplot(rep(1,10), col = grey.colors(10))
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")

#change this to the right file
studyarea<-terra::vect("./studyarea_reprojected.shp"
                       )


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")
soilcomp <- terra::rast("./Earth_engine/soiltype_0_20.tif")
nitrogen <- terra::rast("./Earth_engine/nitrogen_0_20.tif")
phospohrus <- terra::rast("./Earth_engine/phosphorus_0_20.tif")
hills <- terra::rast("./Earth_engine/hills.tif")
slope <- terra::rast("./Earth_engine/slope.tif")
distanceToRiver <- terra::rast("./Earth_engine/DistanceToRiver.tif")
Burnfreq <- terra::rast("./Earth_engine/BurnFreq.tif")
lastburn <- terra::rast("./Earth_engine/YearLastBurned.tif")

# inspect the data 
class(protected_areas)

plot(protected_areas)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict

woody_map <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = woodybiom) +
  ggplot2::scale_fill_gradientn(colours = rev(terrain.colors(6)),
                                name = "TBA/ha",
                                limits = c(0,8),
                                oob = squish
                                )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "bottom") +
  ggplot2::labs(title = "Woody biomass")
  

# plot the rainfall map

rainfall_map <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = rainfall) +
  ggplot2::scale_fill_gradientn(colours = pal_zissou1,
                                name = "ml/m^2?",
                                oob = squish)+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "bottom") +
  ggplot2::labs(title = "Rainfall")

# plot the elevation map

elevation_map <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = elevation) +
  ggplot2::scale_fill_gradientn(colours = pal_zissou2,
                                name = "altitude(m)",
                                oob = squish
  )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "bottom") +
  ggplot2::labs(title = "Elevation")


#plot soil composition map


# combine the different maps  into one composite map using the patchwork library
all_maps <- cowplot::plot_grid(woody_map, elevation_map, rainfall_map, nrow = 1)
all_maps

# and save it to a high resolution png
ggplot2::ggsave("./figures/all_maps.png",all_maps, width = 18, height = 18, units = "cm",dpi=300)

############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)

# crop the layers to the extent of the studyarea
woodybiom_sa<-  terra::crop(woodybiom,saExt)
rainfall_sa<-   terra::crop(rainfall, saExt)
elevation_sa<-  terra::crop(elevation,saExt)
soilcomp_sa<-   terra::crop(soilcomp, saExt)
nitrogen_sa<-   terra::crop(nitrogen, saExt)
phospohrus_sa <-terra::crop(phospohrus, saExt)
hills_sa <-     terra::crop(hills, saExt)
slope_sa <-     terra::crop(slope, saExt)
distanceToRiver_sa <- terra::crop(distanceToRiver, saExt)
Burnfreq_sa <-  terra::crop(Burnfreq, saExt)
lastburn_sa <- terra::crop(lastburn, saExt)

# plot the woody biomass
woody_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = woodybiom_sa) +
  ggplot2::scale_fill_gradientn(colours = rev(terrain.colors(6)),
                                name = "TBA/ha",
                                limits = c(0,8),
                                oob = squish
  )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
  
  
# make maps also for the other layers that you found

#interpolate rainfall
rainfall_30m <- terra::rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area

# plot the rainfall map
rainfall_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = rainfall_sa) +
  ggplot2::scale_fill_gradientn(colours = pal_zissou1,
                                name = "ml/m^2?",
                                oob = squish)+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
                    datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

# plot the elevation map

elevation_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = elevation_sa) +
  ggplot2::scale_fill_gradientn(colours = pal_zissou2,
                                name = "altitude(m)",
                                oob = squish
  )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
                    datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)



#how to make a a SpatRaster discrete??
#plot soil composition map
soilcomp_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = as.factor(soilcomp_sa)) +
  ggplot2::scale_fill_manual(values = c("0" = "#000000","1" = "#d5c36b"	,"2" = "#b96947","3" = "#9d3706",
                                        "4" = "#ae868f"	,"5" = "#f86714","6"= "#46d143","7" = "#368f20","8"= "#3e5a14"), #what goes wrong here?
                             breaks = c("0","1","2","3","4","5","6","7","8"),
                                name = "soil composition",
                             labels = c("NA", "Silty Clay", "Sandy Clay", "Clay Loam", "Silty Clay Loam", "Sandy Clay Loam", "Loam", "Silt Loam", "Sandy Loam"))+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
                    datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

#plot slope
  
slope_map_sa <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = slope_sa) +
    ggplot2::scale_fill_gradientn(colours = pal_zissou2,
                                  name = "slope(degrees)",
                                  oob = squish
    )+
    tidyterra::geom_spatvector(data = lakes,
                               fill = "lightblue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = rivers,
                               color = "blue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = protected_areas,
                               fill = NA,
                               color = "black",
                               linewidth = 0.3)  +
    tidyterra::geom_spatvector(data = studyarea,
                               fill = NA,
                               linewidth = 0.7, color = "red") +
    ggplot2::coord_sf(xlim = xlimits,
                      ylim = ylimits) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                   legend.position = "right") +
    ggplot2::coord_sf(xlimits,ylimits,expand=F,
                      datum = sf::st_crs(32736)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggspatial::annotation_scale(location="bl",width_hint=0.2)

#hills 
  
hills_map_sa <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = as.factor(hills_sa)) +
    ggplot2::scale_fill_manual(values = c("#000000","#d5c36b"),
                               labels = c("no", "yes"),
                               name = "hills?")+
    tidyterra::geom_spatvector(data = lakes,
                               fill = "lightblue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = rivers,
                               color = "blue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = protected_areas,
                               fill = NA,
                               color = "black",
                               linewidth = 0.3)  +
    tidyterra::geom_spatvector(data = studyarea,
                               fill = NA,
                               linewidth = 0.7, color = "red") +
    ggplot2::coord_sf(xlim = xlimits,
                      ylim = ylimits) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                   legend.position = "right") +
    ggplot2::coord_sf(xlimits,ylimits,expand=F,
                      datum = sf::st_crs(32736)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggspatial::annotation_scale(location="bl",width_hint=0.2)
  
#DistanceToRiver
DistanceToRiver_map_sa <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data =distanceToRiver_sa) +
    ggplot2::scale_fill_gradientn(colours = c("blue", "orange"),
                                  name = "Distance to River",
                                  oob = squish
    )+
    tidyterra::geom_spatvector(data = lakes,
                               fill = "lightblue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = rivers,
                               color = "blue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = protected_areas,
                               fill = NA,
                               color = "black",
                               linewidth = 0.3)  +
    tidyterra::geom_spatvector(data = studyarea,
                               fill = NA,
                               linewidth = 0.7, color = "red") +
    ggplot2::coord_sf(xlim = xlimits,
                      ylim = ylimits) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                   legend.position = "right") +
    ggplot2::coord_sf(xlimits,ylimits,expand=F,
                      datum = sf::st_crs(32736)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggspatial::annotation_scale(location="bl",width_hint=0.2)

#Nitrogen
nitrogen_map_sa <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data =nitrogen_sa) +
    ggplot2::scale_fill_gradientn(colours = c('#000005',"#DC4A69" , '#FEFEE7'),
                                  name = "Nitrogen content",
                                  oob = squish
    )+
    tidyterra::geom_spatvector(data = lakes,
                               fill = "lightblue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = rivers,
                               color = "blue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = protected_areas,
                               fill = NA,
                               color = "black",
                               linewidth = 0.3)  +
    tidyterra::geom_spatvector(data = studyarea,
                               fill = NA,
                               linewidth = 0.7, color = "red") +
    ggplot2::coord_sf(xlim = xlimits,
                      ylim = ylimits) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                   legend.position = "right") +
    ggplot2::coord_sf(xlimits,ylimits,expand=F,
                      datum = sf::st_crs(32736)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggspatial::annotation_scale(location="bl",width_hint=0.2)
  
#Phosphorus
phosphorus_map_sa <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data =phospohrus_sa) +
    ggplot2::scale_fill_gradientn(colours = c('#000005',"#F0F921"),
                                  name = "Phosphorus content",
                                  oob = squish
    )+
    tidyterra::geom_spatvector(data = lakes,
                               fill = "lightblue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = rivers,
                               color = "blue",
                               linewidth = 0.3) +
    tidyterra::geom_spatvector(data = protected_areas,
                               fill = NA,
                               color = "black",
                               linewidth = 0.3)  +
    tidyterra::geom_spatvector(data = studyarea,
                               fill = NA,
                               linewidth = 0.7, color = "red") +
    ggplot2::coord_sf(xlim = xlimits,
                      ylim = ylimits) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                   legend.position = "right") +
    ggplot2::coord_sf(xlimits,ylimits,expand=F,
                      datum = sf::st_crs(32736)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggspatial::annotation_scale(location="bl",width_hint=0.2)

#lastburn
lastburn_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data =lastburn_sa) +
  ggplot2::scale_fill_gradientn(colours = c("white",'yellow',"red"),
                                name = "last burned",
                                oob = squish,
                                limits = c(2001, 2016)
  )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
                    datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

#burnfreq
burnfreq_map_sa <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data =Burnfreq_sa) +
  ggplot2::scale_fill_gradientn(colours = c("white",'yellow',"red"),
                                name = "burnfreq",
                                oob = squish
  )+
  tidyterra::geom_spatvector(data = lakes,
                             fill = "lightblue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers,
                             color = "blue",
                             linewidth = 0.3) +
  tidyterra::geom_spatvector(data = protected_areas,
                             fill = NA,
                             color = "black",
                             linewidth = 0.3)  +
  tidyterra::geom_spatvector(data = studyarea,
                             fill = NA,
                             linewidth = 0.7, color = "red") +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits) +
  ggplot2::theme_classic() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
                 legend.position = "right") +
  ggplot2::coord_sf(xlimits,ylimits,expand=F,
                    datum = sf::st_crs(32736)) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)


#combine and save sa maps
# combine the different maps  into one composite map using the patchwork library #done with cowplot
all_maps_sa <- cowplot::plot_grid(woody_map_sa, elevation_map_sa, rainfall_map_sa, soilcomp_map_sa,
                                  slope_map_sa, hills_map_sa, DistanceToRiver_map_sa, nitrogen_map_sa, phosphorus_map_sa,
                                  lastburn_map_sa, burnfreq_map_sa ,
                                  nrow = 4,
                                  align = "hv")
all_maps_sa

# and save it to a high resolution png
ggplot2::ggsave("./figures/all_maps_sa.png",all_maps_sa, width = 18, height = 18, units = "cm",dpi=300)



# create 500 random points in our study area
set.seed(510)

rpoints <- terra::spatSample(studyarea, size = 250, method = "random")

# and add them to the previous map
ggplot2::ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  ggplot2::coord_sf(xlim = xlimits,
                    ylim = ylimits)



# make distance to river map

### put all maps together
#go through all 

# extract your the values of the different raster layers to the points


woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  dplyr::as_tibble() |>
  dplyr::rename(woody = "TBA_gam_utm36s")
woody_points

elevation_points <- terra::extract(elevation_sa, rpoints) |> 
  dplyr::as_tibble()
elevation_points

rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  dplyr::as_tibble() |>
  dplyr::rename(rainfall = "CHIRPS_MeanAnnualRainfall")
rainfall_points

slope_points <- terra::extract(slope_sa, rpoints) |> 
  dplyr::as_tibble() 
slope_points

hills_points <- terra::extract(hills_sa, rpoints) |> 
  dplyr::as_tibble() |>
  dplyr::rename(hills = remapped)
hills_points

d2R_points <- terra::extract(distanceToRiver_sa, rpoints) |> 
  dplyr::as_tibble()
d2R_points

N_points <- terra::extract(nitrogen_sa, rpoints) |> 
  dplyr::as_tibble() |>
  dplyr::rename(Ncontent = mean_0_20)
N_points

P_points <- terra::extract(phospohrus_sa, rpoints) |> 
  dplyr::as_tibble() |>
  dplyr::rename(Pcontent = mean_0_20)
P_points

#comb all
pointdata<-cbind(elevation_points[,2],rainfall_points[,2],
                 slope_points[,2],hills_points[,2], 
                 d2R_points[,2],N_points[,2],
                 P_points[,2],woody_points[,2]) |>
  dplyr::as_tibble()
pointdata
# make long format

# plot how woody cover is predicted by different variables


