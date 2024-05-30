# download NDVI data from Climate Engine
# adapted from climEng_RAP_download.R
# MAC 01/25/24

#library(raster)
library(httr) # HTTP API requests
library(httr2) # HTTP API requests
library(googleCloudStorageR)
library(tidyverse)

# notification states
anomNDVIpb<-"?"

# set home dir
home<-"/home/crimmins/RProjects/ClimateEngineSW"

# specify ClimEng dataset
#datasetCE<-"MODIS_AQUA_16DAY"
datasetCE<-"LANDSAT_SR"

# load key, proj info
source(paste0(home,'/climEngKey.R'))
# google bucket info
bucket<-"clim-engine"
# data directory
dataDir<-"downloads"
dataDir<-paste0(dataDir,"/",datasetCE)

##### set bounding box
# small testing bbox
# n_lat<- 35.149530
# s_lat<- 35.141283
# w_lon<- (-111.809578)
# e_lon<- (-111.798720)

# medium testing bbox -112.401123,34.551811,-111.390381,35.585852
n_lat<- 35.585852
s_lat<- 34.551811
w_lon<- (-112.401123)
e_lon<- (-111.390381)

# SW region -114.927979,31.250378,-102.919922,37.090240
# n_lat<- 37.090240
# s_lat<- 31.250378
# w_lon<- (-114.927979)
# e_lon<- (-102.919922)

bbox<-paste0("[",w_lon,",",s_lat,",",e_lon,",",n_lat,"]")
centroid<-paste0("[[",(w_lon+e_lon)/2,",",(n_lat+s_lat)/2,"]]")


##### get time series -----
# get timeseries of NDVI data

# Define root url for Climate Engine API
root_url <- 'https://api.climateengine.org/'
# Define endpoint for initial data request
endpoint <- "timeseries/native/points"

# Define API arguments time-series endpoint to get long-term blend data 
query <- list(dataset = datasetCE,
              variable = "NDVI",
              start_date = '1981-01-01',
              end_date = Sys.Date(),
              #buffer = '1000',
              #coordinates = paste0("[[",centroids@coords[1,1],",",centroids@coords[1,2],"]]"),
              coordinates = centroid,
              area_reducer = 'mean')

# Run GET request to get data
getTS <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(getTS)

# Parse JSON returned from API
rapTS <- content(getTS, "parsed")
rapTS <- enframe(unlist(rapTS[[1]]$Data))
rapTS_cols <- rapTS$name %>% unique()

# Define simple function to iterate over "name" values in parsed dataframe and return single column dataframe with those values
generate_data_frame <- function(col, df){
  # Filter for name from parsed API return
  df_val <- df %>%
    filter(name == col)
  # Create output data frame based on name from parsed API return
  out_df <- tibble(val = df_val$value)
  # Provide names from parsed API return as column name
  colnames(out_df) <- c(col)
  return(out_df)
}

# Map generate_data_frame over list of columns and clean up resulting data frame
rapTS <- map(rapTS_cols, generate_data_frame, rapTS) %>%
  bind_cols() %>%
  mutate(Date = as.Date(Date),
         Value = as.numeric(`NDVI`),
         Variable = "NDVI") %>%
  #filter(Value > 0) %>%
  select(-`NDVI`)

# save table to csv
write.csv(rapTS, paste0(home,"/",dataDir,"/",datasetCE,"_ts.csv"),row.names = FALSE)  

# plot a time series
# ggplot(rapTS, aes(Date,Value))+
#   geom_line()+
#   ggtitle(paste0("NDVI at ",(n_lat+s_lat)/2,",",(w_lon+e_lon)/2))
#####

##### download anom raster ----
tempFile<-paste0(datasetCE,"_anom")
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/anomalies'
#endpoint = '/raster/export/percentiles' # for percentile


##### q1 -- NDVI Anomaly ----
query <- list(dataset = datasetCE,
              variable = "NDVI",
              temporal_statistic = "mean",
              #percentile_step = 1,
              bounding_box = bbox,
              export_path = exportPath,
              #start_date = "2023-09-01",
              #end_date = "2023-10-01",
              export_resolution = 120,
              start_date = rapTS$Date[nrow(rapTS)-3],
              end_date = rapTS$Date[nrow(rapTS)],
              start_year = format(rapTS$Date[1], "%Y"),
              end_year = format(rapTS$Date[nrow(rapTS)], "%Y"),
              #calculation = 'anom'
              calculation = 'anompercentchange'
)
#####

# Run GET request to get data
get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(get_raster)

# get raster if 200 success code
if(get_raster$status_code==200){
  # download file from google cloud
  objG<-gcs_list_objects(bucket)
  
  # wait for processing
  ptm <- proc.time()
  while(nrow(objG)==0){
    print("waiting for raster to process")
    objG<-gcs_list_objects(bucket)
    #gcs_list_objects(bucket)
    Sys.sleep(10)
  }
  anomTime<-proc.time() - ptm
  print(anomTime)
  # download from bucket
  #gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0(home,"/",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
  # download both files
  gcs_get_object(objG$name[1],
                 saveToDisk = paste0(home,"/",dataDir,"/",objG$name[1]),
                 bucket = bucket, overwrite = TRUE)
  # gcs_get_object(objG$name[2],
  #                saveToDisk = paste0(home,"/",dataDir,"/",objG$name[2]),
  #                bucket = bucket, overwrite = TRUE)
  anomNDVIpb<-"Y"
  # clean up Google Bucket
  objG<-gcs_list_objects(bucket)
    gcs_delete_object(objG$name[1], bucket=bucket)
    #gcs_delete_object(objG$name[2], bucket=bucket)
}else{
  print("NDVI anom download unsuccessful")
  anomNDVIpb<-"N"
}
#####

# # merge and resave files
# # https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
# temp1<-terra::rast(paste0(home,"/",dataDir,"/",objG$name[1]))
# temp2<-terra::rast(paste0(home,"/",dataDir,"/",objG$name[2]))
#   s <- terra::src(temp1, temp2)
#   m <- terra::merge(s)
# terra::writeRaster(m, paste0("./",dataDir,"/",datasetCE,"_anom.tif"), filetype = "GTiff", overwrite = TRUE)
# unlink(paste0(home,"/",dataDir,"/",objG$name[1]))
# unlink(paste0(home,"/",dataDir,"/",objG$name[2]))
# #####

# send notification     
#source("RAPpushNotify.R")
textPB<-paste0("ClimEng NDVI Download Status: NDVIanom(",anomNDVIpb,")")   
RPushbullet::pbPost("note", textPB, apikey =pbKey)


