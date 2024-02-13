# script to download and map 30-day NDVI percentiles processed by Climate Engine
# MAC 2/6/24
# requires climEng_NDVI_download_loRes_Landsat_perc.R to upload images for SW to
# https://cales.arizona.edu/climate/misc/ClimEng/Landsat/

# import functions
import::from(raster, crop, extent, as.data.frame, crs, raster)
import::from(ggplot2, ggplot, fortify, aes, geom_raster, binned_scale, geom_polygon,
             scale_x_continuous, scale_y_continuous, coord_equal, theme_bw, ggtitle,
             geom_sf, coord_sf, ggsave, xlab, ylab, theme, element_text)
import::from(RColorBrewer, brewer.pal)
import::from(sf, st_as_sf)
import::from(leaflet, colorBin, leaflet, addProviderTiles, setView, addPolygons, addRasterImage, addLegend,
             addLayersControl, layersControlOptions)
import::from(leafem, addImageQuery)
import::from(magrittr, "%>%")
import::from(htmlwidgets, saveWidget)

# set rasteroptions
raster::rasterOptions(progress = 'text')

# Set high timeout limit
options(timeout = 5000)

mapNDVI<-function(Level2Data, pathway){

  # create temp directory
  tmpName<-"tempDownload"
  dir.create(tmpName)
  
  # download files
  download.file("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_perc.tif",
               destfile = paste0("./",tmpName,"/LANDSAT_SR_perc.tif"), extra = "--no-verbose", mode='wb')
  
  # load time series
  #NDVIts <- readr::read_csv("downloads/LANDSAT_SR/LANDSAT_SR_perc_ts.csv")
  #NDVIts <- readr::read_csv("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/LANDSAT_SR_perc_ts.csv")
  #date1<-format(NDVIts$Date[nrow(NDVIts)-4],"%m-%d-%Y")
  #date2<-format(NDVIts$Date[nrow(NDVIts)],"%m-%d-%Y")
  
  # get dates from query
  qDates <- readr::read_csv("https://cales.arizona.edu/climate/misc/ClimEng/Landsat/dateRange.csv")  
  date1<-format(qDates$sDate,"%m-%d-%Y")
  date2<-format(qDates$eDate,"%m-%d-%Y")
  
  
  # load downloaded rasters
  #anomNDVI<-raster("./downloads/MODIS_AQUA_16DAY_anom.tif")
  anomNDVI<-raster(paste0("./",tmpName,"/LANDSAT_SR_perc.tif"))
  
  # pass mapbox from environment
  # crop to shape extent
  anomNDVI<-crop(anomNDVI,(extent(Level2Data)*1.075))
  
  ##### pure ggplot mapping ----
  df <- as.data.frame(anomNDVI, xy= TRUE)
  
  # load shapefile for maps
  #poly<-fortify(Level2Data)
  poly <- st_as_sf(Level2Data)
  
  # color bins for ggplot and leaflet
  #bins <-c(0,10,30,45,55,70,90,100)
  #bins <-c(0,10,20,30,40,50,60,70,80,90,100)
  bins <-c(0,2,5,10,20,30,70,80,90,95,98,100)
  colN<-length(bins)-1
  
  p1<-ggplot(data = df, aes(x = x, y =y, fill=LANDSAT_SR_perc))+                   #plot map
    geom_raster()+
    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn", 
                 palette = function(x) brewer.pal(n=colN, 'BrBG'),
                 breaks = bins,
                 name = "NDVI-Percentile",
                 limits = c(bins[1], bins[length(bins)]),
                 na.value = "grey60",
                 show.limits = TRUE, 
                 guide = "colorsteps")+
    #geom_polygon(data=poly, aes(x = long, y = lat, group = group), fill=NA, color="black")+
    geom_sf(data = poly, fill=NA, color="black", linewidth=0.3, inherit.aes = F)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0)) +
    xlab("Lon")+
    ylab("Lat")+
    coord_sf()+
    theme_bw()+
    ggtitle(paste0("NDVI Percentile Rank \n (",
                   date1,
                   " to ",
                   date2,
                   ")"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  
    p1
    ggsave(path = pathway, filename = "Landsat_NDVI_perc.png", scale=1.5, dpi = 600)
    #ggsave(path = pathway, filename = "Landsat_NDVI_perc.png", width = 8, height = 8, dpi = 600)
    
    
    ##### leaflet map
    
    raster::crs(anomNDVI) <- "+proj=longlat +datum=WGS84"
    # get setView center
    centLat<-mean(Level2Data@bbox[2,1:2])
    centLon<-mean(Level2Data@bbox[1,1:2])
    
    # color bins
    pal <- colorBin(brewer.pal(n=colN, 'BrBG'),
                    bins = bins, na.color = "grey60")
    # map elements
    mapTitle<-paste0("NDVI Anom:<br>", date1,"<br>to ",date2)
    
    # make raster map
    leafMap<-leaflet() %>%
      
      #addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
      #addMapPane("raster", zIndex = 420) %>%        # Level 2: middle
      #addMapPane("polygons", zIndex = 430) %>%          # Level 3: top
      
      addProviderTiles("Esri.WorldTopoMap", group="topomap") %>%
      #addProviderTiles("CartoDB.Voyager", group="topomap") %>%
      #addProviderTiles("OpenStreetMap.Mapnik", group="topomap") %>%
      #addProviderTiles("Esri.WorldImagery", group="satellite") %>%
      
      #addProviderTiles("CartoDB.Positron", group="basemap", options = leafletOptions(pane = "background_map")) %>%
      #addProviderTiles("Esri.WorldImagery", group="satellite", options = leafletOptions(pane = "background_map")) %>%
      #addProviderTiles("Esri.WorldTopoMap", group="topomap", options = leafletOptions(pane = "background_map")) %>%
      
      # addLayersControl(
      #   baseGroups = c("basemap","satellite","topomap"),
      #   options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE))%>%
      
      setView(centLon, centLat, zoom = 8) %>%
      addPolygons(data=Level2Data, color="black", weight=0.75, fillOpacity = 0)%>%
      #addPolygons(data=Level2Data, color="black", weight=0.75, fillOpacity = 0, options = leafletOptions(pane = "polygons"))%>%
      addRasterImage(anomNDVI, colors=pal, opacity = 0.6,layerId = "NDVI-anom",
                     group="NDVI-anom", maxBytes=Inf,project=TRUE, method="ngb") %>%
      addLegend(pal = pal, values = c(0,100), title = mapTitle)%>%
      addImageQuery(anomNDVI, type="mousemove", project=TRUE, digits=0,layerId = "NDVI-anom", prefix = "") 
      
      # save leaflet map
      saveWidget(leafMap, file=paste0(pathway,"/Landsat_NDVI_perc_leafMap.html"), selfcontained = TRUE)
    
      # cleanup files
      unlink(paste0("./",tmpName,"/LANDSAT_SR_perc.tif"))
      unlink("tempDownload", recursive = T)
      
      
}