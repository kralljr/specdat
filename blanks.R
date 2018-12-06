# Cleans and creates blanks data
# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Load constituents
load("data/cons.RData")

# load each blank data
# find parameters of interest
# yearly correct?

years <- seq(2000, 2017)
k <- 1
for(i in years) {
  print(i)
  # Name with year
  n1 <- paste0("data/blanks/blanks_all_", i, ".csv")
  # Load data
  x <- read.csv(n1)
  
  # Keep only cons
  # Don;t use Micrograms only? ********
  # Use only 24 hour
  # What to do about MDL for blank?
  x <- dplyr::filter(x, Parameter.Name %in% cons, Parameter.Name != "PM2.5 - Local Conditions", 
                     Blank.Type == "FIELD", 
                     Units.of.Measure == "Micrograms/cubic meter (LC)", Duration == "24 HOUR") %>%
    dplyr::select(., Parameter.Name, Sample.Measurement, Uncertainty)
  x <- x[complete.cases(x), ]
  
  x <- group_by(x, Parameter.Name) %>% 
    summarize(., mean = mean(Sample.Measurement, na.rm = T),
           count = n(),
           unc = sqrt(sum(Uncertainty^2, na.rm = T))) %>%
    mutate(., unc = unc / count, Year = i) %>%
    select(., -count)
  
  # Don't blank correct PM2.5
  
  if(k == 1) {
    xall <- x
  } else {
    xall <- full_join(xall, x)
  }
  
  k <- k + 1
}

save(xall, file = "data/blanks-all.RData")