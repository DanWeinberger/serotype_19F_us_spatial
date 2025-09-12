#compare epic and IIS data

library(tidyverse)
library(readxl)

# helper: detect if vaccine appears before or after 12 months
# helper: extract doses before 12m
# helper for doses before 12m
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
  # explicit "NoPCVxGE12m" overrides everything
  out <- ifelse(str_detect(strings, paste0("No", vaccine, "GE12m")), 0, NA_integer_)
  
  pat <- paste0("GE(\\d+)", vaccine, "GE12m")
  val <- str_match(strings, pat)[,2]
  val <- as.integer(val)
  
  # fill in counts if not explicitly "No"
  out <- ifelse(!is.na(val), val, out)
  out <- ifelse(is.na(out) & str_detect(strings, paste0(vaccine,"GE12m")), 1, out)
  out[is.na(out)] <- 0
  out
}


vaccines <- c("PCV7","PCV13","PCV15","PCV20")

iis <- readxl::read_excel('./Data/CT_VaxData_dummy.xlsx') %>%
  pivot_longer(cols=starts_with('received')) %>%
  rename(vax_string = name) 


for (v in vaccines) {
  iis[[paste0(v,"_LT12")]] <- extract_doses_lt12(iis$vax_string, v)
  iis[[paste0(v,"_GE12")]] <- extract_doses_ge12(iis$vax_string, v)
}