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
#Level2Data<-subset(ShapeFile2, FORESTNAME=="Coronado National Forest")
Level2Data<-spTransform(Level2Data, CRS("+proj=longlat +datum=WGS84"))

# download files
#download.file("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_anom.tif",
#              destfile = paste0(tempDir,"LANDSAT_SR_anom.tif"), extra = "--no-verbose", mode='wb')

# load downloaded rasters
#anomNDVI<-raster("./downloads/MODIS_AQUA_16DAY_anom.tif")
anomNDVI<-raster("./downloads/LANDSAT_SR/LANDSAT_SR_anom.tif")

# load time series
#NDVIts <- readr::read_csv("downloads/NDVIts.csv")
NDVIts <- readr::read_csv("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_ts.csv")

# crop to shape extent
anomNDVI<-crop(anomNDVI,(extent(Level2Data)*1.075))

# set NAs
anomNDVI[anomNDVI < (-9000)] <- NA

anomNDVI[anomNDVI < (-100)] <- NA
anomNDVI[anomNDVI > (100)] <- NA


# load shapefile for maps
poly<-fortify(Level2Data)

# make maps
# pNDVI<-rasterVis::gplot(anomNDVI) + geom_tile(aes(fill = value)) +
#   #facet_wrap(~ variable) +
#   scale_fill_gradient2(low = 'brown', mid="grey87", high = 'forestgreen', midpoint=0, limits=c(-0.15, 0.15), oob=squish, name="NDVI-anomaly") +
#   geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0)) +
#   coord_equal()+
#   theme_bw()+
#   ggtitle(paste0("MODIS_AQUA_16DAY NDVI Anomaly \n- ",NDVIts$Date[nrow(NDVIts)-3]," to ",NDVIts$Date[nrow(NDVIts)]))

#####

##### pure ggplot mapping ----

#anomNDVI<-raster::aggregate(anomNDVI, 5, fun=mean)



df <- as.data.frame(anomNDVI, xy= TRUE)
#rm(anomNDVI)

# color bins for ggplot and leaflet
#bins <-c(-1,-0.25,-0.15,-0.05,0.05,0.15,0.25,1)
bins <-c(-50,-25,-10,-5,5,10,25,50)
#bins <-c(0,10,30,45,55,70,90,100)
colN<-length(bins)-1

p1<-ggplot(data = df, aes(x = x, y =y, fill=LANDSAT_SR_anom))+                   #plot map
  geom_raster()+
  #scale_fill_manual(values = RColorBrewer::brewer.pal(n=7, 'BrBG'), labels = bins, na.value = "grey60")+
  #scale_fill_stepsn(colors=RColorBrewer::brewer.pal(n=7, 'BrBG'),breaks=bins, na.value = "grey60")+
  #scale_fill_gradient2(low = "#8C510A", mid="#F5F5F5", high = "#01665E", midpoint=0, limits=c(-0.15, 0.15), oob=squish, name="NDVI-anomaly") +
  #scale_fill_distiller(type="div",palette="BrBG")+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) RColorBrewer::brewer.pal(n=colN, 'BrBG'),
               breaks = bins,
               name = "NDVI-Anomaly",
               limits = c(bins[1], bins[length(bins)]),
               na.value = "grey60",
               show.limits = TRUE, 
               guide = "colorsteps")+
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()+
  ggtitle(paste0("NDVI Difference from Avg. \n (",NDVIts$Date[nrow(NDVIts)-4]," to ",NDVIts$Date[nrow(NDVIts)],")"))

#plotly::ggplotly(p1)

# leaflet map
library(leaflet)
library(leafem)

crs(anomNDVI) <- "+proj=longlat +datum=WGS84"
# get setView center
centLat<-mean(Level2Data@bbox[2,1:2])
centLon<-mean(Level2Data@bbox[1,1:2])

# color bins

pal <- colorBin(RColorBrewer::brewer.pal(n=colN, 'BrBG'),
                bins = bins, na.color = "grey60")

mapTitle<-paste0("NDVI Anom:<br>", format(NDVIts$Date[nrow(NDVIts)-4], "%b-%d-%y"),"<br>to ",
                           format(NDVIts$Date[nrow(NDVIts)], "%b-%d-%y"))

# develop colorramp
#pal<- colorNumeric(RColorBrewer::brewer.pal(n=11, 'BrBG'), domain = c(-1,1), reverse = FALSE)
# make map
leafMap<-leaflet() %>%
  #addProviderTiles("Esri.WorldTopoMap", group="topomap") %>%
  addProviderTiles("CartoDB.Voyager", group="topomap") %>%
  setView(centLon, centLat, zoom = 8) %>%
  addPolygons(data=Level2Data, color="black", weight=0.75, fillOpacity = 0)%>%
  addRasterImage(anomNDVI, colors=pal, opacity = 0.7,layerId = "NDVI-anom", group="NDVI-anom", maxBytes=Inf) %>%
  addLegend(pal = pal, values = c(-1,1), title = mapTitle)%>%
  addImageQuery(anomNDVI, type="mousemove", project=TRUE, digits=2,layerId = "NDVI-anom", prefix = "")

# title=paste0(month.abb[currMo]," ",currYr,"</br>",BMT,"-Mo. SPI")
library(htmlwidgets)
saveWidget(leafMap, file="test_leaflet.html", selfcontained = TRUE)


