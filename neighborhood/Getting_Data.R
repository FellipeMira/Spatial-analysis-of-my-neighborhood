library(tidyverse)
library(geobr)
library(tmap)


# Finding the IBGE code for the municipality of Guaratingueta
guara <- 
  lookup_muni(name_muni = "GuaratinguetÃ¡")$code_muni %>% 
  read_municipality()

# Spatial data of census tracts of the GuaratinguetÃ¡ Population Census
census_gta <- read_census_tract(code_tract = 3518404)

# verifying data through plotting
tmap_mode(mode = "view")

tm_shape(guara)+
  tm_borders(col = "grey2")+
  tm_shape(census_gta)+
  tm_polygons()

# save vector data
st_write(guara,"vector_data/guara/guara.shp")
