pacman::p_load(char = c('forecast', 'stars', 'st', 'lubridate',
                        'imputeTS', 'leaflet', 'classInt', 'geojsonio', 'ggpubr'),
               install = T, update = F, character.only = T)
require(tidyverse)
require(rgee)
require(sf)
rgee::ee_Initialize(drive = T)

dir <- getwd()

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

df <- read_csv("tabular_data/rgee_file_577a5edb0ac1.csv")

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



# As you can see, there are multiple dates in the same month, we want to create a monthly time series so will take the mean value for each month.

na.cnt <- length(tsv[is.na(tsv)]) #We want to get an idea of the number of entries with no EVI value
evi.trend$na.cnt[i] <- na.cnt
td <- tsv %>% 
  mutate(month = month(as.Date(rownames(tsv))), year = year(as.Date(rownames(tsv)))) %>% 
  group_by(year, month) %>%
  summarise(mean_evi = mean(evi, na.rm = T), .groups = "keep") %>%
  as.data.frame()
head(td)


#That looks better! Unfortunately though, there are a number of dates which don’t have any evi value at all, let’s figure out which ones these are.
td$date <- as.Date(paste0(td$year, "-", td$month, "-01"))
dx <- Dates[!(Dates$date %in% td$date),]
dx

#Q2. Why might NA values occur, are there more during a certain time of year, why do you think that is?
##Now let’s append those NA records to the data frame so we can impute them, this is necessary to perform any other time-series analysis.

dx$mean_evi <- NA
tdx <- rbind(td, dx) %>% 
  arrange(date)
na.cnt <- length(tdx[is.na(tdx)])
evi.trend$na.cnt.2[i] <- na.cnt #add count of na values to dataframe
rm(td, dx) #remove data we're no longer using, this is a good rule of thumb, especially when working with larger datasets.
tdx <- ts(data = tdx$mean_evi, start = c(2001, 1), end = c(2019, 11), frequency = 12) #convert data to time series.
plot(tdx)

# Looks spotty… Let’s fill in those values using time series imputation, we will be applying an auto arima function using the Kalman smoothing.

tdx <- if(na.cnt > 0){imputeTS::na_kalman(tdx, model = "auto.arima", smooth = T)} else {
  tdx
}
plot(tdx)

# Q3. Look at the documentation for the forecast::auto.arima function, which model summary statistic does the function use in selecting the best fit ARIMA model.
## Now we’re ready for time series analysis! First, we decompose the time series.

tdx.dcp <- stl(tdx, s.window = 'periodic')
plot(tdx.dcp)


# Q4. Describe what you see in the plot above. For example, is there a clear seasonal pattern? What is the general trend of the series? Do you notice any pattern in the remainders? Is there a seasonal pattern we might not be accounting for?
## Now we remove seasonality from the time series before fitting a linear model.

tdx.ns <- data.frame(time = c(1:length(tdx)), trend = tdx - tdx.dcp$time.series[,1])
Tt <- trendcycle(tdx.dcp)
St <- seasonal(tdx.dcp)
Rt <- remainder(tdx.dcp)
trend.summ <- summary(lm(formula = trend ~ time, data = tdx.ns)) #tslm
plot(tdx.ns)
abline(a = trend.summ$coefficients[1,1], b = trend.summ$coefficients[2,1], col='red')


# Q5. Why would we want to remove seasonality before fitting a model to this data?
## Now we will add trend strength calculations and model summary statistics to our trend dataframe.

evi.trend$trend[i] <- trend.summ$coefficients[2,1]
evi.trend$trnd.strngth[i] <- round(max(0,1 - (var(Rt)/var(Tt + Rt))), 1) #Trend Strength Calculation <https://towardsdatascience.com/rainfall-time-series-analysis-and-forecasting-87a29316494e>
evi.trend$seas.strngth[i] <- round(max(0,1 - (var(Rt)/var(St + Rt))), 1) #Seasonal Strength Calculation
evi.trend$p.val[i] <- trend.summ$coefficients[2,4]
evi.trend$r2[i] <- trend.summ$r.squared
evi.trend$std.er[i] <- trend.summ$sigma
evi.trend[i,]

# Q6. Based on the plot above and the model summary statistics, describe the 
# trend. For example, is the trend significant? How much do EVI values change per
# month? Are they increasing or decreasing?
## And now, for fun, let’s forecast EVI values using a Holt-Winter’s model.

plot(evi.hw <- forecast::hw(y = tdx, h = 12, damped = T))

#  And for the grand finale, let’s run all of the above processes for all 2,312 
# cells in our hex grid! This could take 50+ minutes, if you want to skip ahead
# and download the data from Google Drive (recommended), type “no” and then Enter   
#in the prompt that appears in your console below.

if(readline(prompt = "This could take 50+ mins. Hit enter to proceed or type 'no' to download the data from G-Drive. ") == "no"){
  googledrive::drive_download(file = googledrive::as_id("https://drive.google.com/file/d/1nEIBmVZj4FHFdlAxuU5krxFACHfvLe3z/view?usp=sharing"), overwrite = T)
  evi.trend <- read.csv("evi_trend.csv")
} else {
  dir.create(paste0(dir,"/decomp_plots"))
  dir.create(paste0(dir,"/hw_plots"))
  for(i in 1:nrow(evi.df)){
    tsv <- data.frame(evi = t(evi.df[i, 2:ncol(evi.df)])) 
    colnames(tsv) <- c("evi")
    na.cnt <- length(tsv[is.na(tsv)])
    evi.trend$na.cnt[i] <- na.cnt
    if(na.cnt < 263){
      td <- tsv %>% 
        mutate(month = month(as.Date(rownames(tsv))), year = year(as.Date(rownames(tsv)))) %>% 
        group_by(year, month) %>%
        summarise(mean_evi = mean(evi, na.rm = T), .groups = "keep") %>%
        as.data.frame()
      td$date <- as.Date(paste0(td$year, "-", td$month, "-01"))
      dx <- Dates[!(Dates$date %in% td$date),]
      dx$mean_evi <- NA
      tdx <- rbind(td, dx) %>% 
        arrange(date)
      na.cnt <- length(tdx[is.na(tdx)])
      evi.trend$na.cnt.2[i] <- na.cnt
      rm(td, dx)
      tdx <- ts(data = tdx$mean_evi, start = c(2001, 1), end = c(2019, 11), frequency = 12)
      tdx <- if(na.cnt > 0){imputeTS::na_kalman(tdx, model = "auto.arima", smooth = T)} else {
        tdx
      }
      tdx.dcp <- stl(tdx, s.window = 'periodic')
      evi.dcmp.lst[[i]] <- tdx.dcp
      png(filename = paste0(dir,"/decomp_plots/hw_", i,".png"), width = 1200, height = 650) #This will save our decomposition plots
      plot(tdx.dcp)
      dev.off()
      tdx.ns <- data.frame(time = c(1:length(tdx)), trend = tdx - tdx.dcp$time.series[,1])
      Tt <- trendcycle(tdx.dcp)
      St <- seasonal(tdx.dcp)
      Rt <- remainder(tdx.dcp)
      trend.summ <- summary(lm(formula = trend ~ time, data = tdx.ns)) #tslm
      evi.trend$trend[i] <- trend.summ$coefficients[2,1]
      evi.trend$trnd.strngth[i] <- round(max(0,1 - (var(Rt)/var(Tt + Rt))), 1) #Trend Strength Calculation <https://towardsdatascience.com/rainfall-time-series-analysis-and-forecasting-87a29316494e>
      evi.trend$seas.strngth[i] <- round(max(0,1 - (var(Rt)/var(St + Rt))), 1) #Seasonal Strength Calculation
      evi.trend$p.val[i] <- trend.summ$coefficients[2,4]
      evi.trend$r2[i] <- trend.summ$r.squared
      evi.trend$std.er[i] <- trend.summ$sigma
      evi.hw <- forecast::hw(y = tdx, h = 12, damped = T)
      evi.hw.lst[[i]] <- evi.hw
      png(filename = paste0(dir,"/hw_plots/hw_", i,".png"), width = 1200, height = 650) #This will save our projection plots
      plot(evi.hw)
      dev.off()
      rm(evi.hw, trend.summ, tdx.ns, tdx.dcp, Tt, St, Rt, tdx, na.cnt)
      gc()
    } else {
      evi.ts[[i]] <- NA
    }
  }
}

head(evi.trend) #Let's take a peak

# plot a density plot showing the spread of trend values in Guaratinguetá

ggpubr::ggdensity(evi.trend, x = "trend", 
          fill = "#0073C2FF", 
          color = "#0073C2FF",
          add = "mean", 
          rug = TRUE)


#  Q7. What appears to be the overall trend in EVI values in the county, is it 
# getting more or less green over time? What might be the implications of this?
## Now we will join the trend data to the hex grid we created previously.

hex_trend <- hex %>%
  left_join(evi.trend, by = 'hex_id', keep = F) %>%
  replace(is.na(.), 0)
hex_trend <- st_transform(hex_trend, st_crs(4326))

# create a Leaflet map

trend_brks <- classIntervals(hex_trend$trend, n=11, style = "fisher")
colorscheme <- RColorBrewer::brewer.pal(n = 11, 'RdYlGn')
palette_sds <- colorBin(colorscheme, domain = hex_trend$trend, bins=trend_brks$brks, na.color = "#ffffff", pretty = T)

pop <- paste0("<b> Hex ID: </b>",hex_trend$hex_id,"<br><b>NA Count: </b>",hex_trend$na.cnt+hex_trend$na.cnt.2,"<br><b>Trend: </b>",format(round(hex_trend$trend, 4), scientific = FALSE),"<br><b> P-Value: </b>",round(hex_trend$p.val, 4),"<br><b>R2: </b>",round(hex_trend$r2, 4),"<br><b>Std Err: </b>",round(hex_trend$std.er, 4),"<br><b>Trend Strength: </b>",round(hex_trend$trnd.strngth, 2),"<br><b>Seasonal Strength: </b>",round(hex_trend$seas.strngth, 4),"<br>")
#Here we're creating a popup for our interactive map.


map <- hex_trend %>%
  leaflet() %>%
  setView(-22.793028310964505, -45.18305342940922, 9) %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo Map") %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB", 
                   options = providerTileOptions(opacity = 0.7)) %>%
  addPolygons(
    fillColor = ~palette_sds(hex_trend$trend),
    fillOpacity = hex_trend$trnd.strngth,
    opacity = 0.5,
    weight = 0.1,
    color='white', 
    group = "Hexbins", 
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE),
    popup = pop,
    popupOptions = popupOptions(
      maxHeight = 250, 
      maxWidth = 250)) %>%
  addLegend(
    title = "Trend: lm(EVI ~ Month)",
    pal = palette_sds,
    values = hex_trend$trend,
    opacity = 0.7,
    labFormat = labelFormat(
      digits = 5)) %>%
  addLayersControl(
    baseGroups = c("Topo Map", "Imagery"),
    overlayGroups = c("Hexbins"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position='bottomleft')

map

hex_trend

# group by neighborhood
census_gp <- census_gta %>% 
  group_by(name_neighborhood) %>% 
  summarise()

# Filter my neighborhood
clube <- census_gp %>% 
   filter(name_neighborhood == "Clube Dos 500")

# Create bbox to neighborhood
box_clube <- 
  sf::st_bbox(clube) %>% sf::st_as_sfc()

require(tmap)
tmap_mode("view")
map1 <- tm_shape(hex_trend)+
  tm_polygons("trend",
              style = 'fisher',
              border.col = NA)+
  tm_shape(census_gp)+
  tm_borders(col = "grey")+
  tm_shape(box_clube)+
  tm_borders(col = "black",lwd =  2)+
  tm_view(bbox=sf::st_bbox(hex_trend),
          set.zoom.limits = c(10,16))

map1

require(tmap)
tmap_mode("view")
map <- tm_shape(hex_trend)+
  tm_polygons("trend",
              style = 'fisher',
              border.col = NA,legend.show = FALSE)+
  
  tm_shape(census_gp)+
  tm_borders(col = "grey")+
  tm_text("name_neighborhood",
          fontfamily = "sans",
          fontface = "bold",
          size = 1.5)+
  tm_shape(clube)+
  tm_borders(col="grey1")+
  tm_view(bbox=sf::st_bbox(hex_trend),
          set.zoom.limits = c(10,16))
  
map

tmap_arrange(map1,map,ncol = 2,sync = F)
  
