
library(rgee)
library(sf)

reticulate::py_module_available()

# initialize
ee_Initialize()


# Set vars 
years <- 2012:2022
guara <- st_read("vector_data/guara/guara.shp")

# Run ee_extract using a batch size ---------------------------------------
rain_data <- list()

for (year in as.character(years)) {
  cat('processing data for the year:', year,"\n")
  init_date <- sprintf('%s-01-01',year)
  last_date <- sprintf('%s-12-31',year)
  terraclimate <- ee$ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')$    # SELECT IMAGE COLLECTION
    filterDate(init_date, last_date)$
    map(function(x) x$select("avg_rad"))           # SELECT BANDS
  pp_data <- ee_extract(x = terraclimate,
                        y = guara,
                        scale = 5000)  # fun = ee$Reducer$mean()
  
  rain_data[[year]] <- pp_data
}

# Wrangling data ----------------------------------------------------------
## Flatten list of years

flat <- rain_data %>%
  reduce(bind_cols)


# data validated by subset loreto_sf to single district. index from rain_data in the same order as loreto_sf. Correct to use bind_cols
guara_with_rain <- guara %>%
  bind_cols(flat)
