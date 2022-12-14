library(rgee)
library(tidyverse)
library(rgee)
library(sf)

#rgee::ee_install_upgrade()

#reticulate::py_install('earthengine-api==0.1.317', envname='vrgee')
  
ee_Initialize(drive = T)

guara.ee <- 
  st_bbox(guara) %>% 
  st_as_sfc() %>% 
  sf_as_ee() #Converts it to an Earth Engine Object

#Landsat 8 col
land8 <- 
  ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")

#JRC col
gsw <- ee$Image("JRC/GSW1_2/GlobalSurfaceWater")

# cloud cover
cc <- 60L

#l8 evi
AddEVI <- function (img) {
  evi <- img$expression(
    expression = '2.5 * ((nir - red) / (nir + 2.4 * red + 1))', opt_map = list(
      nir = img$select("B5")$multiply(0.0001),
      red = img$select("B4")$multiply(0.0001)))
  return (img$addBands(evi))
}

# Cloud mask
maskClouds <- function(img) {
  clear <- img$select('pixel_qa')$bitwiseAnd(2)$neq(0)
  clear <- img$updateMask(clear)
  return(clear)
}

# Water Mask
water_mask <- gsw$select('seasonality')$lt(11)$unmask(1)$clip(guara.ee)

#Clip por AOI
cliplim <- function (img) {return (img$clip(guara.ee))}

coleccion <- land8$
  filterBounds(guara.ee)$
  filterDate('2014-01-01', '2022-12-31')$
  filterMetadata('CLOUD_COVER','less_than', cc)$
  map(maskClouds)$
  map(cliplim)


#Aplicar función para EVI
col1 <- coleccion$map(AddEVI)


#Aplicar función para percentil y función de máscara de agua
col1 <- col1$reduce(ee$Reducer$percentile(list(90)))$mask(water_mask)

# Band names ver.
bandNames <- col1$bandNames()

bandNames$getInfo()




