# map Climate Engine RAP data
# requires processed and downloaded RAP data from climEng_RAP_download.R
# MAC 02/14/24
# choropleth map version

library(ggplot2)
library(rasterVis)
library(scales)
library(raster)
#library(cowplot)

# Allotments level
rgdal::ogrListLayers("./shapes/rmu.gdb")
Level3Data<-rgdal::readOGR("./shapes/rmu.gdb")
Level3Data<-subset(Level3Data, ADMIN_FOREST_NUM=="07")
Level3Data<-sp::spTransform(Level3Data, CRS("+proj=longlat +datum=WGS84"))

rap<-raster("./downloads/RAP/SW_rawRAP.tif")
anom<-raster("./downloads/RAP/SW_anomRAP.tif")

# load time series
#RAPts <- readr::read_csv("downloads/RAP/RAPts.csv")
#NDVIts <- readr::read_csv("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_perc_ts.csv")
dateRange <- readr::read_csv("downloads/RAP/dateRange.csv")

# crop to shape extent
rap<-crop(rap,(extent(Level3Data)*1.075))
anom<-crop(anom,(extent(Level3Data)*1.075))


# get mean value by allotment
spatStats<-list()
for(i in 1:length(Level3Data@polygons)){
  # rpms data
  clip1 <- crop(rap, extent(Level3Data[i,])) #crop to extent of polygon
  clip2 <- rasterize(Level3Data[i,], clip1, mask=TRUE) #crops to polygon edge & converts to raster
  ext <- getValues(clip2)
  # perc data
  clip1 <- crop(anom, extent(Level3Data[i,])) #crop to extent of polygon
  clip2 <- rasterize(Level3Data[i,], clip1, mask=TRUE) #crops to polygon edge & converts to raster
  ext2 <- getValues(clip2)
  
  
  spatStats[[i]]<- cbind.data.frame(
    min(ext, na.rm = TRUE),
    max(ext, na.rm = TRUE),
    mean(ext, na.rm =TRUE),
    median(ext, na.rm = TRUE),
    min(ext2, na.rm = TRUE),
    max(ext2, na.rm = TRUE),
    mean(ext2, na.rm =TRUE),
    median(ext2, na.rm = TRUE),
    length(ext),
    Level3Data[i,]@data)
  print(i)
}

spatStats<-do.call(rbind,spatStats)
colnames(spatStats)[1:9]<-c("minRPMS","maxRPMS","meanRPMS","medianRPMS",
                            "minPERC","maxPERC","meanPERC","medianPERC","countRPMS")
spatStats[,1:8]<-round(spatStats[,1:8],1)

# merge back into spdf
tempLev3<-Level3Data
tempLev3@data<-merge(tempLev3@data, spatStats[,c(1:9,45)], by="CRC")


