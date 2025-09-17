library(tidyverse)
library(tidycensus)

# Built-in dataset: fips_codes
data("fips_codes")

cw <- fips_codes %>%
  mutate(county = gsub('County','',county),
         county = toupper(county),
         county_name = paste(county, state, sep=', '),
         fips=paste0(state_code,county_code),
         county_name = gsub(' ','',county_name)
  ) %>%
  dplyr::select( state,fips,county)%>%
  mutate(county=trimws(county, which = "right"))

#############################
cw_co <-  cw %>% filter(state=='CO') 

colorado_iis <- read_csv('./Data/colorado_immunization_county/combined_CO_Child_Rates.csv') %>%
    mutate(county=toupper(County)) %>%
    full_join(cw_co, by='county') %>%
  mutate(fips=as.numeric(fips),
        time = as.Date(sub(" -.*", "", Period),format = "%B %d, %Y") 
        )%>%
  rename(pct_pcv_iis = 'Up-To-Date Percent') %>%
  rename(vaccine=Vaccine) %>%
  dplyr::select(county,time,  vaccine,pct_pcv_iis, fips)%>%
  mutate(state = 'CO')
  
########################################

oregon_files <- list.files('./Data/oregon_immunization_county/staging', full.names = T)
oregon_iis_list <- lapply(oregon_files, function(X) readxl::read_excel(X))

cw_or <-  cw %>% filter(state=='OR') 

oregon_iis <- bind_rows(oregon_iis_list) %>%
  rename(Vaccine="...1",
         county=County) %>%
  pivot_longer(-c(Vaccine, county)) %>%
  filter(Vaccine=='4 doses PCV') %>%
  mutate(county= toupper(county),
         time = as.Date(paste(name, '01','01', sep='-'))
         )%>%
  full_join(cw_or, by='county') %>%
  rename(vaccine=Vaccine,
         pct_pcv_iis = value) %>%
  dplyr::select(county, time, vaccine, pct_pcv_iis, fips) %>%
  mutate(fips = as.numeric(fips),
         pct_pcv_iis =100 * pct_pcv_iis)%>%
  mutate(state = 'OR')


###############################

cw_mn <-  cw %>% filter(state=='MN') 

mn_iis <- read_csv('./Data/Minnesota immunizations.csv') %>%
  filter(vaccine=='PCV')%>%
  rename(county=location,
         pct_pcv_iis=percent) %>%
  mutate(county= toupper(county),
      time = as.Date(paste(year, '01','01', sep='-')),
      vaccine = '4 doses PCV'
    ) %>%
  dplyr::select(time, fips,vaccine, county, pct_pcv_iis,population) %>%
  mutate(state = 'MN')
  

#############
    
iis_combined <- bind_rows(mn_iis, colorado_iis, oregon_iis)


#Limited to base patient, has any encounters, has an immunization registry query
epic <- read_csv('./Data/abcs_epic_pcv_uptake_epic_pediatric_base_1_4.csv', skip=11) %>%
  rename(county_name = "County of Residence" ,
         pct_pcv_epic = "Percentage with Immunizations: PCV (%)",
         N_patients = "Number of Patients" 
         )%>%
  tidyr::fill( .,  county_name, .direction = "down") %>%
  mutate(state =sub(".*,", "", county_name)) %>%
  separate(county_name, into = c("county", "state"), sep = ",\\s*") %>%
  left_join(cw, by=c('state', 'county'))%>%
  mutate(fips = as.numeric(fips),
         pct_pcv_epic = gsub('%','', pct_pcv_epic),
         pct_pcv_epic = as.numeric(pct_pcv_epic)
         )


iis_combined_last <- iis_combined %>%
  filter(time == '2020-01-01')

compare <- epic %>%
  full_join(iis_combined_last, by=c('fips', 'state'))%>%
  mutate(time=factor(time))

#
ggplot(compare) +
  geom_point(aes(x=pct_pcv_epic, y=pct_pcv_iis, color=state, shape=time)) +
  theme_classic()+
  ggtitle('Comparison of IIS data with Epic Cosmos')+
  facet_wrap(~state)+
  ylim(40,90)+
xlim(40,90)+
  geom_abline(aes(intercept=0, slope=1), lty=2)
