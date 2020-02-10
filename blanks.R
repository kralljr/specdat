# Cleans and creates blanks data
# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Load constituents
load("data/cons.RData")

nab <- function(x) is.na(x) | x == ""

# load each blank data
# find parameters of interest

# all years
years <- seq(2000, 2019)
# Do not blank correct PM2.5
cons1 <- cons[-which(cons == "PM2.5 - Local Conditions")]

for(j in 1 : length(cons1)) {
  k <- 1
  for(i in years) {
    print(c(cons1[j], i))
    # Name with year
    n1 <- paste0("data/blanks/blanks_all_", i, ".csv")
    # Load data
    x <- read.csv(n1, stringsAsFactors = F)
    
    # Restrict blank data
    # Keep only cons
    # Don't use Micrograms only? ********
    # Use only 24 hour
    x <- dplyr::filter(x, Parameter.Name == cons1[j], 
                       Blank.Type == "FIELD", 
                       Units.of.Measure == "Micrograms/cubic meter (LC)", Duration == "24 HOUR")

    # Remove qualifiers, get ID
    # fix: doesn't work for later years
    #unite(x, quals, Qualifier.1 : Qualifier.10, sep = "") %>%
    #filter(x, quals == "NANANANANANANANANA") %>%
      # No qualifying flags for any 1-10
    x <-  filter(x, nab(Qualifier.1), nab(Qualifier.2),
                 nab(Qualifier.3),nab(Qualifier.4),
                 nab(Qualifier.5),nab(Qualifier.6),
                 nab(Qualifier.7),nab(Qualifier.8),
                 nab(Qualifier.9),nab(Qualifier.10)) %>%
      # remove qualifiers
      select(., -contains("Qualifier")) %>%
      mutate(., State.Code = str_pad(State.Code, 2, "left", pad = "0"), 
             County.Code = str_pad(County.Code, 3, "left", pad = "0"),
             Site.Num = str_pad(Site.Num, 4, "left", pad = "0"),
             id = paste0(State.Code, County.Code, Site.Num, ".", POC)) 
    
    if(k == 1) {
      xall <- x
    } else {
      xall <- full_join(xall, x)
    }
    
    k <- k + 1
  }
  
  # For all years, one constituent
  # Get median MDL by site
  # meds <- group_by(xall, id) %>% summarise(., m1 = median(MDL, na.rm = T))
  # 
  # # Merge in median MDL
  # xall <- full_join(xall, meds)
  
  # xall <- mutate(xall,
  #           # replace MDL with median MDL by site
  #           MDL = ifelse(is.na(MDL), m1, MDL),
  #           # Replace uncertainty if BDL
  #           Uncertainty = ifelse(Sample.Measurement < MDL, 5/6 * MDL, Uncertainty),
  #           # Replace below the detection limit with 1/2 MDL
  #           Sample.Measurement = ifelse(Sample.Measurement < MDL, 1/2 * MDL, Sample.Measurement)) %>%
    # Restrict to variables of interest
  xall <- dplyr::select(xall, Date.Local, id, Sample.Measurement, Uncertainty, MDL, Method.Name, Parameter.Name) %>%
    # Remove missing
  filter(., !is.na(Sample.Measurement))


  # Save constituent file for blanks
  cn <- gsub("\\.", "", gsub(" ", "", cons1[j]))
  write.csv(xall, file = paste0("data/blanks/", cn, "-blanks.csv"), row.names = F)
}
