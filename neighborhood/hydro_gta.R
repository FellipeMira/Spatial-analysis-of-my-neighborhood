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
  inventoryResult = inv_flu_acre,
  deleteNAstations = F)

data_flu_acre$`2245192`

tidy_data_acre <- organize(stationsDataResult = data_flu_acre)

flu_acre <- selectStations(
  # Pass on output from organize() function
  organizeResult = tidy_data_acre,
  # Decide by either yearly or monthly filter
  mode = "yearly",
  # Filter years with a maximum % of missing data
  maxMissing = 10,
  # Filter stations with a minimum of years available
  minYears = 15,
  # use civil year (month = 1) or define month for water year
  month = 1,
  # filter from initial to final years (or NULL to use entire time series)
  iniYear = 1980,
  finYear = 2020,
  # Use only consisted or raw data as well?
  consistedOnly = FALSE,
  # Plot figure? TRUE by default
  plot = TRUE
)

flu_acre

tidy_data_acre <- bind_rows(tidy_data_acre)

