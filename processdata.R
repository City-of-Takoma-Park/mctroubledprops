library(tidyverse)
library(mctroubledproperties)
library(leaflet)
library(sf)

mc_prop_data <- mctroubledproperties::read_fix_mc("7NX47WJxPoo7lCFSXmXQ2JPCR")

saveRDS(mc_prop_data, "./data/mc_prop_data.rds")
