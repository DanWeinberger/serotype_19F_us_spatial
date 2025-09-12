#compare epic and IIS data

library(tidyverse)
library(readxl)

# helper: detect if vaccine appears before or after 12 months
# helper: extract doses before 12m
# helper for doses before 12m
# doses before 12m
# doses before 12m
extract_doses_lt12 <- function(strings, vaccine){
  pat <- paste0("GE(\\d+)", vaccine, "LT12m(?:on)?")
  out <- str_match(strings, pat)[,2]
  out <- as.integer(out)
  out <- ifelse(is.na(out) & str_detect(strings, paste0(vaccine,"LT12m(?:on)?")), 1, out)
  out[is.na(out)] <- 0
  out
}

# doses >=12m
extract_doses_ge12 <- function(strings, vaccine){
  out <- ifelse(str_detect(strings, paste0("No", vaccine, "GE12m")), 0, NA_integer_)
  pat <- paste0("GE(\\d+)", vaccine, "GE12m")
  val <- str_match(strings, pat)[,2]
  val <- as.integer(val)
  out <- ifelse(!is.na(val), val, out)
  out <- ifelse(is.na(out) & str_detect(strings, paste0(vaccine,"GE12m")), 1, out)
  out[is.na(out)] <- 0
  out
}


vaccines <- c("PCV7","PCV13","PCV15","PCV20")

iis <- readxl::read_excel('./Data/CT_VaxData_dummy.xlsx') %>%
  pivot_longer(cols=starts_with('received')) %>%
  rename(vax_string = name) 


# apply to dataframe
for (v in vaccines) {
  lt_var <- paste0(v,"_LT12")
  ge_var <- paste0(v,"_GE12")
  flag_var <- paste0(v,"_GE12_flag")
  
  iis[[lt_var]]   <- extract_doses_lt12(iis$vax_string, v)
  iis[[ge_var]]   <- extract_doses_ge12(iis$vax_string, v)
  iis[[flag_var]] <- ifelse(iis[[ge_var]] > 0, 1, 0)
}

# optional: overall flag for any vaccine at ≥12m
iis <- iis %>%
  mutate(Any_GE12 = if_else((PCV7_GE12 + PCV13_GE12 + PCV15_GE12 + PCV20_GE12) > 0, 1, 0))

any_primary = iis %>%
  filter(vax_string %in% c('ReceivedPCV7LT12mon', 'ReceivedGE1PCV13LT12mon', 'ReceivedGE1PCV15LT12mon','ReceivedGE1PCV20LT12mon')) %>%
  group_by(bth_yr, bth_quarter,FIPS_Code) %>%
  summarize(N_prim = sum(value))

boost = iis %>%
  filter(Any_GE12==1) %>%
  group_by(bth_yr, bth_quarter,FIPS_Code) %>%
  summarize(N_boost = sum(value))

combo <-   any_primary %>% 
  full_join(boost, by=c('bth_yr', 'bth_quarter','FIPS_Code'))

write.csv(combo,'./Data/iis_cleaned.csv')
