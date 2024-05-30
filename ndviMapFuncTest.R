# testing NDVI map function
# MAC 2/6/24

source('~/RProjects/ClimateEngineSW/map_landsat_NDVI_perc.R', echo=TRUE)

# district level
#ogrListLayers("./shapes/RangerDistrict.gdb")
ShapeFile2<-rgdal::readOGR("./shapes/RangerDistrict.gdb")
ShapeFile2<-ShapeFile2[-(which(stringr::str_detect(ShapeFile2$DISTRICTNAME, 'Grassland')==TRUE)),]

Level2Data<-subset(ShapeFile2, FORESTNAME=="Kaibab National Forest")
#Level2Data<-subset(ShapeFile2, FORESTNAME=="Coronado National Forest")
Level2Data<-sp::spTransform(Level2Data, sp::CRS("+proj=longlat +datum=WGS84"))


mapNDVI(Level2Data, "./temp/")

