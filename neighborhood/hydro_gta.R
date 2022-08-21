devtools::install_github("hydroversebr/hydrobr", build_vignettes = TRUE)

library(hydrobr)
library(ggplot2)
library(sf)
library(dplyr)
guara <- geobr::lookup_muni(name_muni = "Guaratinguetá")$code_muni %>% 
  geobr::read_municipality()


geobr::read_meso_region()

inv_flu_acre <- inventory(
  stationType = 'plu',
  as_sf = T,
  aoi = guara
)


ggplot()+
  geom_sf(data=guara,
          fill = "white")+
  geom_sf(data = census_gta,
          size = 0.1,
          fill='white')+
  geom_sf(data = inv_flu_acre)+
  theme_light()

data_flu_acre <- stationsData(
  # [1:10,] specifics that you are interest only in the first 10 rows (stations)
  inventoryResult = inv_flu_acre,
  deleteNAstations = F
)
