# File to blank correct metals and adjust EC/OC
# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)


# Load blanks
load("data/blanks-sum.RData")
load("data/ecoc-am.RData")
load("data/cons.RData")
load("data/mons.RData")
# fix cons of interest (removing TOR not measured for this time/site)
cons <- cons[c(1 : 22, 33 : 35, 37 : 38)]



###########################
# RESTRICT TO YEARS/MONITORS of interest

# Fix MDL
years <- seq(2000, 2005)

# for each constituent
for(j in 1 : length(cons)) {
  k <- 1
  for(i in years) {
    print(c(cons[j], i))
    # Name with year
    n1 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
    # Load data
    x <- fread(file = n1, sep=",")
    
    # restrict to parameter of interest, columns of interest
    x <- mutate(x, id = paste0(State.Code, County.Code, Site.Number, ".", POC))
    x <- filter(x, Parameter.Name == cons[j], id %in% mons$id)
    
    if(k == 1) {
      xall <- x
    } else {
      xall <- full_join(x, xall)
    }
  } # end loop over years
  
  # Fix character to numeric
  xall <- mutate(xall, Sample.Measurement = as.numeric(Sample.Measurement),
                 Detection.Limit = as.numeric(Detection.Limit),
                 Uncertainty = as.numeric(Uncertainty))
  
  # Now have all years for one constituent
  # Find median MDL
  meds <- group_by(xall, State.Code, County.Code, Site.Number, POC, Method.Name) %>%
    summarise(., dlmed = median(Detection.Limit, na.rm = T))
  # Find geomean (for missing): error, some nonpositive (0  values).  Do not use geomean because of 0s?
  geomean <- group_by(xall, State.Code, County.Code, Site.Number, POC, Method.Name) %>%
    summarise(., geomean = geoMean(Sample.Measurement + 10^(-10), na.rm = T))
  
  # Get average relative uncertainty
  relunc <- mutate(xall, relunc = Uncertainty / (Sample.Measurement + 10^(-10))) %>% 
    group_by(., State.Code, County.Code, Site.Number, POC, Method.Name) %>%
    summarise(., relunc = mean(relunc, na.rm = T))
  
  # Replace missing DL with median
  xall <- full_join(xall, meds) %>%
    full_join(., geomean) %>%
    full_join(., relunc) %>%
    mutate(., Detection.Limit = ifelse(is.na(Detection.Limit), dlmed, Detection.Limit),
           year = as.numeric(substr(Date.Local, 1, 4))) %>%
    select(., -dlmed)
  
  
  
  # Save results
  # get name again, with modified MDL
  # Save constituent file for blanks
  cn <- gsub("\\.", "", gsub(" ", "", cons[j]))
  n1 <- paste0("data/y2000-2005/Krall_speciation-", cn, "-chdate.csv")
  
  write.csv(xall, file = n1, row.names = F)
}



###########################
# Blank correction, replace BDL, OC/EC correction

blankbdl <- function(consU, bdl = F) {
  print(consU)
  k <- 1
  # Name with year
  cn <- gsub("\\.", "", gsub(" ", "", consU))
  
  n1 <- paste0("data/y2000-2005/Krall_speciation-", cn, "-chdate.csv")
  # Load data
  x <- fread(file = n1, sep=",")
  # fix id
  x <- mutate(x, id = paste0(State.Code, County.Code, Site.Number, ".", POC))
  x <- select(x, -year)
  # name for save
  n2 <- paste0("data/Krall_speciation-", cn, "-clean.csv")
  
  ###############
  # blank correct
  # merge blank data
  x <- mutate(x, 
              Sample.Measurement = as.numeric(Sample.Measurement),
              Uncertainty = as.numeric(Uncertainty))
  
  # no blank correct PM2.5?
  if(cn != "PM25-LocalConditions") {
    
    # only merge those with blanks
    blanks2 <- filter(blanks, Parameter.Name == consU) 
    blank1 <- left_join(x, blanks2)
    
    # Method summary for blanks
    bsum <- group_by(blanks2, Method.Name) %>% summarise(., bsum = median(blank, na.rm = T))
    blank1 <- full_join(blank1, bsum)
    
    # blank correct, DO NOT adjust uncertainty, rename parameter
    blank1 <- mutate(blank1, blank = ifelse(is.na(blank), bsum, blank),
                     Sample.Measurement = (Sample.Measurement) - blank, 
                     #Uncertainty = sqrt((Uncertainty)^2 + unc^2),
                     Parameter.Name = paste0("badj-", Parameter.Name))
    blank1 <- select(blank1, -blank)
    x <-  blank1
  }
  
  ###############
  # Replace BDL with 1/2 MDL, fix uncertainty
  lwhNA <- (length(which(!is.na(x$Sample.Measurement) & is.na(x$Detection.Limit))))
  if(lwhNA > 0) {
    cat("Constituent missing MDL:", consU)
  }
  if(bdl) {
    x <- mutate(x, Uncertainty = ifelse(Sample.Measurement < Detection.Limit,
                                        5/6 * Detection.Limit, Uncertainty),
                Sample.Measurement = ifelse(Sample.Measurement < Detection.Limit,
                                            1/2 * Detection.Limit, Sample.Measurement))
    
    
    ################
    # Missing metals, replace with geometric mean, unc = 4 * geomean
    # ** Should not be any missing for NH4, SO4, NO3, OC, EC **
    x <- mutate(x, Uncertainty = ifelse(is.na(Sample.Measurement), 4 * geomean,
                                        Uncertainty),
                Sample.Measurement = ifelse(is.na(Sample.Measurement), geomean,
                                            Sample.Measurement))
    
    
    ################
    # Missing uncertainty
    # Use average relative uncertainty, times sample.measurement
    x <- mutate(x, Uncertainty = ifelse(is.na(Uncertainty), 
                                        relunc * Sample.Measurement, Uncertainty))
  }
  x
  
}


ecocadjust <- function(x) {
  ###############
  # OC/EC correct
  ecoc1 <- mutate(x, Month = as.numeric(substr(Date.Local, 6, 7)))
  necoc <- c("OC CSN Unadjusted PM2.5 LC TOT", "EC CSN PM2.5 LC TOT")
  # restrict to EC/OC TOT
  ecoc1 <- filter(ecoc1, Parameter.Name %in% c(necoc, paste0("badj-", necoc)))
  # Remove detection
  ecoc1 <- select(ecoc1, -Detection.Limit)
  
  # If any TOT measurements
  if(nrow(ecoc1) > 0) {
    # join data with correction values
    ecoc1 <- left_join(ecoc1, ecoc)
    # make sure by submethod and by month
    
    # Get original variable names if necessary
    ecoc1 <- mutate(ecoc1, Type = ifelse(substr(Parameter.Name, 1, 4) == "badj", "badj", ""),
                    Parameter.Name = ifelse(substr(Parameter.Name, 1, 4) == "badj", 
                                            substring(Parameter.Name, 6), Parameter.Name))
    # Remove uncertainty 
    meas <- select(ecoc1, -Uncertainty, -geomean, -relunc, -bsum, -nc) %>%
      spread(., Parameter.Name, Sample.Measurement)
    
    # transform based on formulas from chapter 8
    meas <- mutate(meas, adjEC = 1.3 * `EC CSN PM2.5 LC TOT`, 
                   adjOC = (`OC CSN Unadjusted PM2.5 LC TOT` - 0.3 * `EC CSN PM2.5 LC TOT` - A) / M) %>%
      select(., -`OC CSN Unadjusted PM2.5 LC TOT`, -`EC CSN PM2.5 LC TOT`) %>%
      # group together EC/OC into one column
      gather(., Parameter.Name1, Sample.Measurement, adjEC : adjOC) %>%
      mutate(., Parameter.Name1 = paste0(Type, "-", Parameter.Name1)) %>%
      select(., -A, -M, -Type, -Month)
    # rename again
    meas <- mutate(meas, Parameter.Name = ifelse(Parameter.Name1 %in% c("badj-adjEC", "-adjEC"),
                                                 "EC CSN PM2.5 LC TOT",
                                                 ifelse(Parameter.Name1 %in% c("badj-adjOC", "-adjOC"),
                                                        "OC CSN Unadjusted PM2.5 LC TOT", NA)))
    
    # Uncertainty
    unc <- select(ecoc1, -Sample.Measurement, -geomean, -relunc, -bsum, -nc) %>%
      spread(., Parameter.Name, Uncertainty)
    
    # transform based on formulas from chapter 8
    unc <- mutate(unc, adjEC = 1.3 * `EC CSN PM2.5 LC TOT`, 
                  adjOC = sqrt((`OC CSN Unadjusted PM2.5 LC TOT`^2 - 0.3^2 * `EC CSN PM2.5 LC TOT`^2)) / M) %>%
      select(., -`OC CSN Unadjusted PM2.5 LC TOT`, -`EC CSN PM2.5 LC TOT`) %>%
      # group together EC/OC into one column
      gather(., Parameter.Name1, Uncertainty, adjEC : adjOC) %>%
      mutate(., Parameter.Name1 = paste0(Type, "-", Parameter.Name1)) %>%
      select(., -A, -M, -Type, -Month)
    # rename again
    unc <- mutate(unc, Parameter.Name = ifelse(Parameter.Name1 %in% c("badj-adjEC", "-adjEC"),
                                               "EC CSN PM2.5 LC TOT",
                                               ifelse(Parameter.Name1 %in% c("badj-adjOC", "-adjOC"),
                                                      "OC CSN Unadjusted PM2.5 LC TOT", NA)))
    
    
    # add in corrected EC/OC
    meas1 <- full_join(meas, unc) %>%
      select(., -Parameter.Name) %>%
      rename(., Parameter.Name = Parameter.Name1)
    
    # add in other constituents/pm
    xtest <- filter(x, !(Parameter.Name %in% c(necoc, paste0("badj-", necoc)))) 
    xtest <- full_join(xtest, meas1)
    x <- xtest
  }
  
  
  x
}



adjustCSN <- function(bdl = F) {
  
  # Blank/MDL./Unc for each constituent
  #for(i in 26 : 27) {
  for(i in 1 : length(cons)) {
    x <- blankbdl(cons[i], bdl)
    
    #combine all constituents
    if(i == 1) {
      xall <- x
    } else {
      xall <- full_join(xall, x)
    }
  }
  #adjust ECOC
  xall <- ecocadjust(xall)
  
  # remove unnecessary
  xall <- select(xall, -geomean, -relunc, -nc, -bsum)
  xall <- mutate(xall, Sample.Measurement = ifelse(Sample.Measurement < 0, 0, Sample.Measurement))
  
  xall
}


csndat <- adjustCSN()
write.csv(csndat, file = "data/csndat2000-2005.csv", row.names = F)

csndat <- adjustCSN(bdl = T)
write.csv(csndat, file = "data/csndat2000-2005-bdl.csv", row.names = F)

# # equation 8.10 in chapter 8
# #ec
# 1.3 * ec
# #oc
# (oc- 0.3 * ec - a) / m