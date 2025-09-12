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
  filter(state=='CO') %>%
  dplyr::select( fips,county)%>%
  mutate(county=trimws(county, which = "right"))

colorado_iis <- read_csv('./Data/colorado_immunization_county/combined_CO_Child_Rates.csv') %>%
    mutate(county=toupper(County)) %>%
    full_join(cw, by='county') %>%
  mutate(fips=as.numeric(fips)) %>%
  rename(pct_pcv_iis = 'Up-To-Date Percent') %>%
  dplyr::select(county, Period, Vaccine,pct_pcv_iis, fips)
  
epic <- read.csv('./Data/abcs_epic_pcv_uptake.csv') %>%
  filter(age=='age_1_2y' & grepl(',CO',county_name)) %>%
  rename(pct_pcv_epic = pct_pcv)

compare <- epic %>%
  full_join(colorado_iis, by='fips') %>%
  filter(Period=='January 1, 2019 - June 30, 2019')

#
ggplot(compare) +
  geom_point(aes(x=pct_pcv_epic, y=pct_pcv_iis)) +
  theme_classic()+
  ggtitle('Comparison of IIS data with Epic Cosmos')
