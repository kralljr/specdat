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
# Add 0 to number


years <- seq(2000, 2017)
#years <- seq(2000, 2002)
k <- 1
cons1 <- cons[-which(cons == "PM2.5 - Local Conditions")]
cons1 <- cons1[1 : 2]
for(j in 1 : length(cons1)) {
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
    x <- dplyr::filter(x, Parameter.Name == cons1[j], 
                       Blank.Type == "FIELD", 
                       Units.of.Measure == "Micrograms/cubic meter (LC)", Duration == "24 HOUR") %>%
      unite(., quals, Qualifier.1 : Qualifier.10, sep = "") %>%
      # No qualifying flags for any 1-10
      filter(., quals == "NANANANANANANANANA") %>%
      mutate(., State.Code = str_pad(State.Code, 2, "left", pad = "0"), 
             County.Code = str_pad(County.Code, 3, "left", pad = "0"),
             Site.Num = str_pad(Site.Num, 4, "left", pad = "0"),
             id = paste0(State.Code, County.Code, Site.Num, ".", POC)) %>%
      dplyr::select(., Date.Local, id, Sample.Measurement, Uncertainty)
    x <- x[complete.cases(x), ]
    # 
    # x <- group_by(x, Parameter.Name) %>% 
    #   summarize(., mean = mean(Sample.Measurement, na.rm = T),
    #          count = n(),
    #          unc = sqrt(sum(Uncertainty^2, na.rm = T))) %>%
    #   mutate(., unc = unc / count, Year = i) %>%
    #   select(., -count)
    
    # Don't blank correct PM2.5
    
    if(k == 1) {
      xall <- x
    } else {
      xall <- full_join(xall, x)
    }
    
    k <- k + 1
  }
  cn <- gsub("\\.", "", gsub(" ", "", cons1[j]))
  write.csv(xall, file = paste0("data/blanks/", cn, "-blanks.csv"), row.names = F)
}

#save(xall, file = "data/blanks-all.RData")

ls <- list.files("data/blanks") 
ls <- ls[grep("-blanks", ls)]
x1 <- read.csv(file.path("data/blanks", ls[2]), 
    colClasses = c("Date", "character", "numeric", "numeric"))
x1 <- mutate(x1, state = substr(id, 1, 2))
t1 <- table(x1$id) %>% sort()
monsin <- unlist(monsin)
t1[names(t1) %in% monsin]
# Not enough to do by site?

# check monsin
