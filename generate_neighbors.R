## Imports a TIGRIS shape file at the census tract level for 2010
# and build a neighbor matrix

library(tigris)
library(sf)
library(tidyverse)
library(INLA)
library(spdep)
options(tigris_use_cache = TRUE)  # cache shapefiles locally

#All counties in ABCs
#Exclude Erie, NY county because only has pediatric cases
#For CA exclude Contra Costa and Alameda because only have <18
#Tennessee has 5 disjointed areas: counties 157, 113, (021,)
#CA islands: 2 census tracts are islands and filtered out
abcs_counties <- readxl::read_excel('./Data/ABCsCountyFIPS.xlsx') %>%
  filter(!(county %in% c('ERIE', 'ALAMEDA','CONTRA COSTA'))) %>%
  mutate(abc_region = if_else(STATE=="NY" & countyfips %in% c("037", "051", "055", "069", "073", "117", "123"), 'NY_1',
                              if_else(STATE=="NY" , 'NY_2', 
                                      if_else(STATE=="TN"  & countyfips %in% c("021", "037", "043", "147", "149", "165", "187", "189"), 'TN_1',
                                        if_else(STATE=="TN"  & countyfips %in% c("065"), 'TN_2',
                                                if_else(STATE=="TN"  & countyfips %in% c("113"), 'TN_3',
                                                        if_else(STATE=="TN"  & countyfips %in% c("157"), 'TN_4',
                                                                if_else(STATE=="TN"  & countyfips %in% c("001",'009','057','089','093','105','145','155','173'), 'TN_5',
                                                      STATE
  )))))))
  )
  
  
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
      dplyr::filter(county_fips %in% abcs_counties$FIPS & !(GEO_ID %in% c("1400000US06075017902","1400000US06075980401") ))
    return(shp_abc)
})

#Create a master shape file
all_tract <- bind_rows(tract_shp_abcs)

write_sf(all_tract, './Data/shapefiles/all_census_tracts.shp')


# Identify and remove islands
nb_all <- lapply(tract_shp_abcs, function(X) spdep::poly2nb(X, queen = TRUE))
names(nb_all) <- states

lapply(states, function(X)  nb2INLA(paste0("./Data/neighbor_files/",X,".MDR.graph.commune"), nb_all[[X]]))
#nb2INLA("../Data/MDR.graph.commune", nb_clean)


