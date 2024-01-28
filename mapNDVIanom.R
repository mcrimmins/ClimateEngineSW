# map Climate Engine NDVI data
# requires processed and downloaded NDVI data from climEng_NDVI_download.R
# MAC 01/25/24

library(ggplot2)
library(rasterVis)
library(scales)
library(raster)
#library(cowplot)

# district level
#ogrListLayers("./shapes/RangerDistrict.gdb")
ShapeFile2<-rgdal::readOGR("./shapes/RangerDistrict.gdb")
Level2Data<-subset(ShapeFile2, FORESTNAME=="Kaibab National Forest")
Level2Data<-spTransform(Level2Data, CRS("+proj=longlat +datum=WGS84"))

# load downloaded rasters
#anomNDVI<-raster("./downloads/MODIS_AQUA_16DAY_anom.tif")
anomNDVI<-raster("./downloads/LANDSAT_SR/LANDSAT_SR_anom.tif")

# load time series
NDVIts <- readr::read_csv("downloads/NDVIts.csv")

# crop to shape extent
anomNDVI<-crop(anomNDVI,Level2Data)

# set NAs
anomNDVI[anomNDVI < (-9000)] <- NA

# load shapefile for maps
poly<-fortify(Level2Data)

# make maps
pNDVI<-rasterVis::gplot(anomNDVI) + geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  scale_fill_gradient2(low = 'brown', mid="grey87", high = 'forestgreen', midpoint=0, limits=c(-0.15, 0.15), oob=squish, name="NDVI-anomaly") +
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("MODIS_AQUA_16DAY NDVI Anomaly \n- ",NDVIts$Date[nrow(NDVIts)-3]," to ",NDVIts$Date[nrow(NDVIts)]))

#####

