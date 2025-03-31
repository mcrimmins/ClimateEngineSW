# download RAP data from Climate Engine
# adapted from climEng_RAP.R
# MAC 01/24/24

#library(raster)
library(httr) # HTTP API requests
library(httr2) # HTTP API requests
library(googleCloudStorageR)
library(tidyverse)

# notification states
rawRAPpb<-"?"
anomRAPpb<-"?"
percRAPpb<-"?"
SNOpb<-"?"

# set home dir
home<-"/home/crimmins/RProjects/ClimateEngineSW"

# load key, proj info
source(paste0(home,'/climEngKey.R'))
# google bucket info
bucket<-"clim-engine"
# data directory
dataDir<-"downloads/RAP"

##### set bounding box
# small testing bbox
# n_lat<- 35.149530
# s_lat<- 35.141283
# w_lon<- (-111.809578)
# e_lon<- (-111.798720)

# KNF region -114.927979,31.250378,-109,37.090240
# n_lat<- 36.9423
# s_lat<- 34.9886
# w_lon<- (-112.6718)
# e_lon<- (-111.6932)

# AZ region -114.927979,31.250378,-109,37.090240
# n_lat<- 37.090240
# s_lat<- 31.250378
# w_lon<- (-114.927979)
# e_lon<- (-109)

# SW region -114.927979,31.250378,-102.919922,37.090240
n_lat<- 37.090240
s_lat<- 31.250378
w_lon<- (-114.927979)
e_lon<- (-102.919922)

bbox<-paste0("[",w_lon,",",s_lat,",",e_lon,",",n_lat,"]")
centroid<-paste0("[[",(w_lon+e_lon)/2,",",(n_lat+s_lat)/2,"]]")

# set small bbox
#bbox<-"[-111.809578,35.141283,-111.798720,35.149530]"
#centroid<-paste0("[[",(-111.809578+-111.798720)/2,",",(35.141283+35.149530)/2,"]]")


##### get time series -----
# get timeseries of RAP data
# following https://github.com/Google-Drought/SupportSiteTutorials/blob/a038ee1e69fff32008d8619c0acc6db082d5795d/Timeseries/Blends_Example.Rmd
#centroids<-rgeos::gCentroid(Level2Data,byid=TRUE)

# Define root url for Climate Engine API
root_url <- 'https://api.climateengine.org/'
# Define endpoint for initial data request
endpoint <- "timeseries/native/coordinates"

# Define API arguments time-series endpoint to get long-term blend data 
query <- list(dataset = 'RAP_PRODUCTION_16DAY',
              variable = "herbaceousAGB",
              start_date = '1986-01-01',
              end_date = Sys.Date(),
              #buffer = '1000',
              #coordinates = paste0("[[",centroids@coords[1,1],",",centroids@coords[1,2],"]]"),
              coordinates = centroid,
              area_reducer = 'mean')

# Run GET request to get data
getTS <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(getTS)

# try again if not 200 status
while(getTS$status_code!=200){
  getTS <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
  print(getTS)
}

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
         Value = as.numeric(`herbaceousAGB (lbs/acre)`),
         Variable = "herbaceousAGB (lbs/acre)") %>%
  filter(Value > 0) %>%
  select(-`herbaceousAGB (lbs/acre)`)

# save table to csv
write.csv(rapTS, paste0(home,"/",dataDir,"/RAPts.csv"),row.names = FALSE)  

# plot a time series
# ggplot(rapTS, aes(Date,Value))+
#   geom_line()+
#   ggtitle(paste0("RAP herbAGB (lbs/ac) at ",(n_lat+s_lat)/2,",",(w_lon+e_lon)/2))
#####

#####
# set start/end dates based on current year
sDate<-paste0(format(rapTS$Date[nrow(rapTS)], "%Y"),"-01-01")
eDate<-rapTS$Date[nrow(rapTS)]
dateRange<-cbind.data.frame(sDate,eDate)
write.csv(dateRange, paste0(home,"/",dataDir,"/dateRange.csv"),row.names = FALSE)
#####


##

# ##### download raw RAP raster ----
# tempFile<-"SW_rawRAP"
# exportPath<-paste0(bucket,"/",tempFile)
# 
# print(paste0("Processing ", tempFile))
# 
# endpoint = '/raster/export/values'
# 
#     ##### q1 - raw data, latest period ----
#     # query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#     #               variable = "herbaceousAGB_mask",
#     #               temporal_statistic = "mean",
#     #               bounding_box = bbox,
#     #               export_path = exportPath,
#     #               start_date = rapTS$Date[nrow(rapTS)],
#     #               end_date = rapTS$Date[nrow(rapTS)],
#     #               start_year = '1986',
#     #               end_year = '2022'
#     # )
#     #####
#     
#     ##### q2 - total production since Jan 1st
#     query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#                   variable = "herbaceousAGB",
#                   temporal_statistic = "total",
#                   bounding_box = bbox,
#                   export_path = exportPath,
#                   export_resolution = 120,
#                   start_date = sDate,
#                   end_date = eDate
#                   #start_date = "2023-01-01",
#                   #end_date = "2023-11-17"
#                   
#     )
#     
#     # Run GET request to get data
#     get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
#     print(get_raster)
#     
#     # try again if not 200 status
#     while(get_raster$status_code!=200){
#       get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
#       print(get_raster)
#     }
#    
# # add in retry if 500 error    
#     
#     # get raster if 200 success code
#     if(get_raster$status_code==200){
#       # download file from google cloud
#       objG<-gcs_list_objects(bucket)
#       
#       # wait for processing
#       ptm <- proc.time()
#       while(length(objG)==0){
#         print("waiting for raster to process")
#         objG<-gcs_list_objects(bucket)
#         Sys.sleep(30)
#       }
#       rawTime<-proc.time() - ptm
#       print(rawTime)
#       # download from bucket
#       gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0(home,"/",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
#       # set flag
#       rawRAPpb<-"Y"
#       # clean up file
#       objG<-gcs_list_objects(bucket)
#       gcs_delete_object(objG$name[1], bucket=bucket)
#     }else{
#       print("RAW RAP download unsuccessful")
#       rawRAPpb<-"N"
#     }
#      
# #####

##### download anom raster ----
tempFile<-"SW_anomRAP"
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/anomalies'

    ##### q1 --- anomaly of most recent period -----
    # query <- list(dataset = 'RAP_PRODUCTION_16DAY',
    #               variable = "herbaceousAGB_mask",
    #               temporal_statistic = "mean",
    #               bounding_box = bbox,
    #               export_path = exportPath,
    #               start_date = rapTS$Date[nrow(rapTS)],
    #               end_date = rapTS$Date[nrow(rapTS)],
    #               start_year = '1986',
    #               end_year = '2022',
    #               calculation = 'anom'
    #              )
    #####

    ##### q2 -- anomaly of cumulative production ----
    query <- list(dataset = 'RAP_PRODUCTION_16DAY',
                  variable = "herbaceousAGB",
                  temporal_statistic = "total",
                  bounding_box = bbox,
                  export_path = exportPath,
                  export_resolution = 120,
                  start_date = sDate,
                  end_date = eDate,
                  #start_date = "2023-01-01",
                  #end_date = "2023-11-17",
                  start_year = '1986',
                  end_year = format(eDate, "%Y"),
                  calculation = 'anompercentof'
    )
    #####

    # Run GET request to get data
    get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
    print(get_raster)

    # try again if not 200 status
    while(get_raster$status_code!=200){
      get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
      print(get_raster)
    }


    # get raster if 200 success code
    if(get_raster$status_code==200){
      # download file from google cloud
      objG<-gcs_list_objects(bucket)

      # wait for processing
      ptm <- proc.time()
      while(length(which(objG$name==paste0(tempFile,".tif")))==0){
        print("waiting for raster to process")
        objG<-gcs_list_objects(bucket)
        #gcs_list_objects(bucket)
        Sys.sleep(30)
      }
      anomTime<-proc.time() - ptm
      print(anomTime)
      # download from bucket
      gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0(home,"/",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
      # set flag
      anomRAPpb<-"Y"
      # clean up file
      objG<-gcs_list_objects(bucket)
      gcs_delete_object(objG$name[1], bucket=bucket)
    }else{
      print("RAP anom download unsuccessful")
      anomRAPpb<-"N"
    }
#####

##

##### download percentile raster ----
# tempFile<-"SW_percRAP"
# exportPath<-paste0(bucket,"/",tempFile)
# 
# print(paste0("Processing ", tempFile))
# 
# endpoint = '/raster/export/percentiles'
# # 
# #     ##### q1 - percentiles of most recent period -----
# #     # query <- list(dataset = 'RAP_PRODUCTION_16DAY',
# #     #               variable = "herbaceousAGB_mask",
# #     #               temporal_statistic = "mean",
# #     #               bounding_box = bbox,
# #     #               export_path = exportPath,
# #     #               start_date = rapTS$Date[nrow(rapTS)],
# #     #               end_date = rapTS$Date[nrow(rapTS)],
# #     #               start_year = '1986',
# #     #               end_year = '2022'
# #     # )
# #     #####
# #     
# #     ##### q2 - percentiles of cumulative total -----
#     query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#                   variable = "herbaceousAGB",
#                   temporal_statistic = "total",
#                   bounding_box = bbox,
#                   export_path = exportPath,
#                   start_date = paste0(format(rapTS$Date[nrow(rapTS)], "%Y"),"-01-01"),
#                   end_date = rapTS$Date[nrow(rapTS)],
#                   start_year = '1986',
#                   end_year = format(rapTS$Date[nrow(rapTS)], "%Y")
#     )
#     #####
# 
#     # Run GET request to get data
#     get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
#     print(get_raster)
# 
#     # get raster if 200 success code
#     if(get_raster$status_code==200){
#       # download file from google cloud
#       objG<-gcs_list_objects(bucket)
#       
#       # wait for processing
#       ptm <- proc.time()
#       while(length(which(objG$name==paste0(tempFile,".tif")))==0){
#         print("waiting for raster to process")
#         objG<-gcs_list_objects(bucket)
#         Sys.sleep(10)
#       }
#       percTime<-proc.time() - ptm
#       print(percTime)
#       # download from bucket
#       gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0(home,"/",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
#       percRAPpb<-"Y"
#     }else{
#       print("RAP percentile download unsuccessful")
#       percRAPpb<-"N"
#     }
#####

##### download SNODAS raster for masking ----
tempFile<-"SW_SNODAS"
exportPath<-paste0(bucket,"/",tempFile)

print(paste0("Processing ", tempFile))

endpoint = '/raster/export/values'

##### q1 - raw data, latest period ----
# query <- list(dataset = 'RAP_PRODUCTION_16DAY',
#               variable = "herbaceousAGB_mask",
#               temporal_statistic = "mean",
#               bounding_box = bbox,
#               export_path = exportPath,
#               start_date = rapTS$Date[nrow(rapTS)],
#               end_date = rapTS$Date[nrow(rapTS)],
#               start_year = '1986',
#               end_year = '2022'
# )
#####

##### q2 - total production since Jan 1st
query <- list(dataset = 'SNODAS',
              variable = "Snow_Depth",
              temporal_statistic = "mean",
              bounding_box = bbox,
              export_path = exportPath,
              #export_resolution = 120,
              start_date = eDate-30,
              end_date = eDate
              #start_date = "2023-01-01",
              #end_date = "2023-11-17"
              
)

# Run GET request to get data
get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
print(get_raster)

# try again if not 200 status
while(get_raster$status_code!=200){
  get_raster <- GET(paste0(root_url, endpoint), config = add_headers(Authorization = key), query = query)
  print(get_raster)
}

# add in retry if 500 error    

# get raster if 200 success code
if(get_raster$status_code==200){
  # download file from google cloud
  objG<-gcs_list_objects(bucket)
  
  # wait for processing
  ptm <- proc.time()
  while(length(objG)==0){
    print("waiting for raster to process")
    objG<-gcs_list_objects(bucket)
    Sys.sleep(30)
  }
  rawTime<-proc.time() - ptm
  print(rawTime)
  # download from bucket
  gcs_get_object(objG$name[[which(objG$name==paste0(tempFile,".tif"))]], saveToDisk = paste0(home,"/",dataDir,"/",tempFile,".tif"), bucket = bucket, overwrite = TRUE)
  # set flag
  SNOpb<-"Y"
  # clean up file
  objG<-gcs_list_objects(bucket)
  gcs_delete_object(objG$name[1], bucket=bucket)
}else{
  print("SNODAS download unsuccessful")
  SNOpb<-"N"
}

#####
    
    
    
##### CLEAN UP GOOGLE BUCKET ----
objG<-gcs_list_objects(bucket)

if(nrow(objG)!=0){
  for(i in 1:nrow(objG)){
    gcs_delete_object(objG$name[i], bucket=bucket)
  }
}
    
######

# send notification     
#source("RAPpushNotify.R")
textPB<-paste0("RAP Download Status: raw(",rawRAPpb,"), anom(",anomRAPpb,"), perc(",percRAPpb,")","), snodas(",SNOpb,")")   
RPushbullet::pbPost("note", textPB, apikey =pbKey)
    
    
    