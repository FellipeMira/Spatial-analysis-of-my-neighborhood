pacman::p_load(char = c('reticulate', 'rgee', 'remotes', 'sf',
                        'magrittr', 'tigris', 'tibble', 'stars', 'raster', 
                        'dplyr', 'forecast', 'stars', 'st', 'lubridate',
                        'imputeTS', 'leaflet', 'classInt', 'RColorBrewer',
                        'ggplot2', 'googledrive', 'geojsonio', 'ggpubr'),
               install = T, update = F, character.only = T)

rgee::ee_Initialize(drive = T)

guara.ee <- st_bbox(guara) %>% 
  st_as_sfc() %>% 
  sf_as_ee() #Converts it to an Earth Engine Object

getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

mod.clean <- function(img) {
  # Extract the NDVI band
  ndvi_values <- img$select("EVI")
  # Extract the quality band
  ndvi_qa <- img$select("SummaryQA")
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "11")
  # Mask pixels with value zero.
  ndvi_values$updateMask(quality_mask)$divide(ee$Image$constant(10000)) #0.0001 is the MODIS Scale Factor
}

modis.evi <- 
  ee$ImageCollection("MODIS/006/MOD13Q1")$
  filter(ee$Filter$date('2001-01-01', '2022-12-31'))$
  map(mod.clean)

# Now we will create a hexagonal grid over the study area

hex <- st_make_grid(x = guara, 
                    cellsize = 0.005,
                    square = T) %>%
  st_sf() %>%
  rowid_to_column('hex_id')

hex <- hex[guara,]

plot(hex)


#Now we will use the grid created above to extract the mean EVI values within each cell for the years 2000-2020. The finer the spatial resolution, the longer it will take, this next chunk should take 30 minutes.
#This will take about 30 minutes
if(readline(prompt = "This will take 30 minutes. Hit enter to proceed or type 'no' to download the data from G-Drive. ") == "no"){
  googledrive::drive_download(file = googledrive::as_id("https://drive.google.com/file/d/1lwOE59c_sL3LsVIA98yGL5OK0DOmiSnz/view?usp=sharing"),
                              overwrite = T)
  evi.df <- read.csv("guara_evi.csv")
  evi.df <- evi.df[,3:ncol(evi.df)]
  colnames(evi.df) <- c('hex_id', stringr::str_replace_all(substr(colnames(evi.df[, 2:ncol(evi.df)]), 2, 11), "_", "-")) #Convert dates to unambiguous format
} else {
  #This will take about 30 minutes
  paste0(system.time(expr = cc.evi <- ee_extract(x = modis.evi, y = hex["hex_id"],
                                                 sf = FALSE, scale = 250,
                                                 fun = ee$Reducer$mean(), 
                                                 via = "drive", quiet = T))/60,
         " Minutes Elapsed. ")
  evi.df <- as.data.frame(cc.evi)
  colnames(evi.df) <- c('hex_id', stringr::str_replace_all(substr(colnames(evi.df[, 2:ncol(evi.df)]), 2, 11), "_", "-"))
  write.csv(x = evi.df, file = "~/guara_evi.csv")
  }

df <- read_csv("guara_evi.csv")

# Now we are going to perform a time series analysis on the data within each grid cell. But first, we will work through the procedure one step at a time.

evi.hw.lst <- list() #Create an empty list, this will be used to house the time series projections for each cell. 
evi.dcmp.lst <- list() #Create an empty list, this will be used to house the time series decomposition for each cell.
evi.trend <- data.frame(hex_id = evi.df$hex_id, na.cnt = NA, na.cnt.2 = NA, trend = NA, p.val = NA, r2 = NA, std.er = NA, trnd.strngth = NA, seas.strngth = NA) #This data frame will hold the trend data
Dates <- data.frame(date = seq(as.Date('2001-01-01'), as.Date('2022-11-01'), "month"))
Dates$month <- month(Dates$date)
Dates$year <- year(Dates$date)
i <- 1
tsv <- data.frame(evi = t(evi.df[i, 2:ncol(evi.df)])) #converting the data to a transposed data frame
colnames(tsv) <- c("evi")
head(tsv) #let's take a look
