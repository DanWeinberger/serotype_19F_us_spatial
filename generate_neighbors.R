## Imports a TIGRIS shape file at the census tract level for 2010
# and build a neighbor matrix

library(tigris)
library(sf)
library(tidyverse)
library(INLA)
options(tigris_use_cache = TRUE)  # cache shapefiles locally

#All counties in ABCs
#Exclude Erie, NY county because only has pediatric cases
#For CA exclude Contra Costa and Alameda because only have <18

abcs_counties <- readxl::read_excel('./Data/ABCsCountyFIPS.xlsx') %>%
  filter(!(county %in% c('ERIE', 'ALAMEDA','CONTRA COSTA')))

#Define states in ABCs
states <- c('CA','CO','CT','GA','MN','NY',  'MD', 'NM', 'OR' ,'TN')

##Import shape files for all tracts in ABCs
tracts_abc_states <- lapply(states, function (X)  {
   tracts(state = X, cb=T, year = 2010, class = "sf")
})

#make valid for INLA
tract_shp <- lapply(tracts_abc_states, function(X) st_make_valid(X)
)
                    
#filter down to counties present in ABCs
tract_shp_abcs <- lapply(tract_shp,function(X){
    shp_abc <- X %>%
    mutate(county_fips = paste0(STATE, COUNTY)) %>%
      dplyr::filter(county_fips %in% abcs_counties$FIPS)
    return(shp_abc)
})

#tract_list
all_tract <- bind_rows(tract_shp_abcs)

write_sf(all_tract, './Data/shapefiles/all_census_tracts.shp')

# Identify and remove islands
nb <- poly2nb(shp, queen = TRUE)
islands <- which(card(nb) == 0)

cat("Original polygons:", nrow(shp), "\n")
cat("Islands found:", length(islands), "\n")

if(length(islands) > 0) {
  cat("Island regions:", shp$l2_code[islands], "\n")
  
  # Store information about removed islands
  removed_islands <- data.frame(
    original_row = islands,
    l2_code = shp$l2_code[islands],
    reason_removed = "island_no_neighbors",
    stringsAsFactors = FALSE
  )
  
  # Remove islands
  shp_clean <- shp[-islands, ]
} else {
  shp_clean <- shp
  removed_islands <- data.frame(
    original_row = integer(0),
    l2_code = character(0),
    reason_removed = character(0),
    stringsAsFactors = FALSE
  )
}

# Create final INLA graph
nb_clean <- poly2nb(shp_clean, queen = TRUE)
nb2INLA("../Data/MDR.graph.commune", nb_clean)


