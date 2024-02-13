# map Climate Engine RAP data
# requires processed and downloaded RAP data from climEng_RAP_download.R
# MAC 02/7/24

library(ggplot2)
library(rasterVis)
library(scales)
library(raster)
#library(cowplot)

# district level
#ogrListLayers("./shapes/RangerDistrict.gdb")
ShapeFile2<-rgdal::readOGR("./shapes/RangerDistrict.gdb")
Level2Data<-subset(ShapeFile2, FORESTNAME=="Kaibab National Forest")
Level2Data<-subset(ShapeFile2, FORESTNAME=="Coronado National Forest")
Level2Data<-sp::spTransform(Level2Data, sp::CRS("+proj=longlat +datum=WGS84"))

# download files
#download.file("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_anom.tif",
#              destfile = paste0(tempDir,"LANDSAT_SR_anom.tif"), extra = "--no-verbose", mode='wb')

# load downloaded rasters
#anomNDVI<-raster("./downloads/MODIS_AQUA_16DAY_anom.tif")
#anomNDVI<-raster("./downloads/LANDSAT_SR/LANDSAT_SR_perc.tif")
rap<-raster("./downloads/RAP/SW_rawRAP.tif")
anom<-raster("./downloads/RAP/SW_anomRAP.tif")

#rap<-raster("./downloads/RAP/SW_rawRAP_010123_111723.tif")
#anom<-raster("./downloads/RAP/SW_anomRAP_010123_111723.tif")


# load time series
#RAPts <- readr::read_csv("downloads/RAP/RAPts.csv")
#NDVIts <- readr::read_csv("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_perc_ts.csv")
dateRange <- readr::read_csv("downloads/RAP/dateRange.csv")

# crop to shape extent
rap<-crop(rap,(extent(Level2Data)*1.075))
anom<-crop(anom,(extent(Level2Data)*1.075))

# aspect ratio
extRap<-extent(rap)
asp<-(extRap@xmax-extRap@xmin)/(extRap@ymax-extRap@ymin)


# load shapefile for maps
poly<-fortify(Level2Data)

##### pure ggplot mapping ----
dfRAP <- as.data.frame(rap, xy= TRUE)
  #colnames(dfRAP)[3]<-"SW_rawRAP"
dfanom <- as.data.frame(anom, xy= TRUE)
  #colnames(dfanom)[3]<-"SW_anomRAP"


# color bins for ggplot and leaflet
#bins <-c(0,10,30,45,55,70,90,100)
#bins <-c(0,10,20,30,40,50,60,70,80,90,100)
bins <-c(0,5,10,50,100,150,300,500,1000)
colN<-length(bins)-1

p1<-ggplot(data = dfRAP, aes(x = x, y =y, fill=SW_rawRAP))+                   #plot map
  geom_raster()+
  #scale_fill_manual(values = RColorBrewer::brewer.pal(n=7, 'BrBG'), labels = bins, na.value = "grey60")+
  #scale_fill_stepsn(colors=RColorBrewer::brewer.pal(n=7, 'BrBG'),breaks=bins, na.value = "grey60")+
  #scale_fill_gradient2(low = "#8C510A", mid="#F5F5F5", high = "#01665E", midpoint=0, limits=c(-0.15, 0.15), oob=squish, name="NDVI-anomaly") +
  #scale_fill_distiller(type="div",palette="BrBG")+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) RColorBrewer::brewer.pal(n=colN, 'YlGn'),
               #palette = function(x) RColorBrewer::brewer.pal(n=colN, 'BrBG'),
               breaks = bins,
               name = "lbs/ac",
               limits = c(bins[1], bins[length(bins)]),
               na.value = "grey60",
               show.limits = TRUE, 
               guide = "colorsteps")+
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()
  # ggtitle(paste0("Total Herbaceous Production (lbs/ac) \n (",
  #                format(RAPts$Date[nrow(RAPts)-4],"%m-%d-%Y"),
  #                " to ",
  #                format(RAPts$Date[nrow(RAPts)],"%m-%d-%Y"),
  #                ")"))

p2<-ggplot(data = dfanom, aes(x = x, y =y, fill=SW_anomRAP))+                   #plot map
  geom_raster()+
  #scale_fill_manual(values = RColorBrewer::brewer.pal(n=7, 'BrBG'), labels = bins, na.value = "grey60")+
  #scale_fill_stepsn(colors=RColorBrewer::brewer.pal(n=7, 'BrBG'),breaks=bins, na.value = "grey60")+
  #scale_fill_gradient2(low = "#8C510A", mid="#F5F5F5", high = "#01665E", midpoint=0, limits=c(-0.15, 0.15), oob=squish, name="NDVI-anomaly") +
  #scale_fill_distiller(type="div",palette="BrBG")+
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               #palette = function(x) RColorBrewer::brewer.pal(n=colN, 'YlGn'),
               palette = function(x) RColorBrewer::brewer.pal(n=colN, 'BrBG'),
               breaks = bins,
               name = "% of Avg",
               limits = c(bins[1], bins[length(bins)]),
               na.value = "grey60",
               show.limits = TRUE, 
               guide = "colorsteps")+
  geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  coord_equal()+
  theme_bw()
  # ggtitle(paste0("Total Herbaceous Production (lbs/ac) \n (",
  #                format(RAPts$Date[nrow(RAPts)-4],"%m-%d-%Y"),
  #                " to ",
  #                format(RAPts$Date[nrow(RAPts)],"%m-%d-%Y"),
  #                ")"))

plotCol<-ifelse(asp<1,2,1)

library(cowplot)
rapPlot<-cowplot::plot_grid(p1, p2, labels = c('A', 'B'), label_size = 10, ncol = plotCol)

#dateRange$sDate<-as.Date("2023-01-01")
#dateRange$eDate<-as.Date("2023-11-17")

# now add the title
title <- ggdraw() + 
  draw_label(
    paste0("RAP Total Herbaceous Production \n (",
            format(dateRange$sDate,"%m-%d-%Y"),
            " to ",
            format(dateRange$eDate,"%m-%d-%Y"),
            ")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p3<-plot_grid(
          title, rapPlot,
          ncol = 1,
          # rel_heights values control vertical title margins
          rel_heights = c(0.1, 1)
        )
p3
ggsave(filename = "RAP_KNF_010124_011724.png", width = 8, height = 8, dpi = 600, bg='#ffffff')

# leaflet map
library(leaflet)
library(leafem)

# project raster to leaflet
#newproj<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"
#test<-raster::projectRaster(anomNDVI, crs=newproj)   
                                 
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
  #addProviderTiles("CartoDB.Voyager", group="topomap") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group="topomap") %>%
  setView(centLon, centLat, zoom = 8) %>%
  addPolygons(data=Level2Data, color="black", weight=0.75, fillOpacity = 0)%>%
  addRasterImage(anomNDVI, colors=pal, opacity = 0.6,layerId = "NDVI-anom", group="NDVI-anom", maxBytes=Inf,project=TRUE, method="ngb") %>%
  addLegend(pal = pal, values = c(0,100), title = mapTitle)%>%
  addImageQuery(anomNDVI, type="mousemove", project=TRUE, digits=0,layerId = "NDVI-anom", prefix = "") 

# title=paste0(month.abb[currMo]," ",currYr,"</br>",BMT,"-Mo. SPI")
library(htmlwidgets)
saveWidget(leafMap, file="test_leaflet.html", selfcontained = TRUE)


