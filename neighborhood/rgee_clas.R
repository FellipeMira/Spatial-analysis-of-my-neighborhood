library(rgee)
library(tidyverse)
library(rgee)

#rgee::ee_install_upgrade()

#reticulate::py_install('earthengine-api==0.1.317', envname='vrgee')

ee_Initialize(drive = T)

guara.ee <- st_bbox(guara) %>% 
  st_as_sfc() %>% 
  sf_as_ee() #Converts it to an Earth Engine Object


land8 <- 
  ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filter(ee$Filter$date('2014-01-01', '2022-12-31'))$
  filterBounds(guara.ee)



addNDVI <- function(image) {
  
  return(image$addBands(image$normalizedDifference(c("SR_B5", "SR_B4"))))
}

ndviCollection <- land8$map(addNDVI)
lc8_gta<- ndviCollection
class(lc8_gta)

lc8_gta <- lc8_gta$select(c('ST_B10'))$toBands()  
ee_print(lc8_gta)

colorizedVis <- list(
  min=10,
  max=30,
  palette=c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

b10 <- lc8_gta$select('LC08_218076_20140201_ST_B10')
Map$addLayer(b10, colorizedVis, '2021 03 15')

