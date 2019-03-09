# File to blank correct metals and adjust EC/OC
# Set working directory to home folder

# Load libraries
library(tidyverse)


# Load blanks
load("data/blanks-all.RData")
load("data/ecoc-am.RData")
load("data/cons.RData")


###########################
# Fix MDL
years <- seq(2000, 2017)
k <- 1
for(j in 1 : length(cons)) {
  for(i in years) {
    print(c(cons[j], i))
    # Name with year
    n1 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
    # Load data
    x <- fread(file = n1, sep=",")
    
    # restrict to parameter of interest, columns of interest
    x <- filter(x, Parameter.Name == cons[j])
    
    if(i == 1) {
      xall <- x
    } else {
      xall <- full_join(x, xall)
    }
  }
  
  # Now have all years for one constituent
  # Find median MDL
  meds <- group_by(xall, State.Code, County.Code, Site.Number, POC) %>%
    summarise(., m1 = median(Detection.Limit, na.rm = T))
  # Find geomean (for missing)
  geomean <- group_by(xall, State.Code, County.Code, Site.Number, POC) %>%
    summarise(., geomean = geoMean(Sample.Measurement, na.rm = T))
  
  # Get average relative uncertainty
  relunc <- mutate(xall, relunc = Uncertainty / Sample.Measurement) %>% 
    group_by(., State.Code, County.Code, Site.Number, POC) %>%
    summarize(., relunc = mean(relunc, na.rm = T))
  
  # Replace missing DL with median
  xall <- full_join(xall, meds) %>%
    full_join(., geomean) %>%
    full_join(., relunc) %>%
    mutate(., Detection.Limit = ifelse(is.na(Detection.Limit), meds, Detection.Limit),
           year = as.numeric(substr(Date.Local, 1, 4))) %>%
    select(., -meds)
  

  
  # Save results
  for(i in years) {
    # get name again, with modified MDL
    n1 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
    
    x <- filter(xall, year == i) %>% select(., -year)
    write.csv(x, file = n1, row.names = F)
  }
}



###########################
# Blank correction, replace BDL, OC/EC correction
years <- seq(2000, 2017)
k <- 1
for(i in years) {
  print(i)
  # Name with year
  n1 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
  # Load data
  x <- fread(file = n1, sep=",")

  # name for save
  n2 <- paste0("data/Krall_speciation-", i, "-clean.csv")
  
  ###############
  # blank correct
  # merge blank data
  x <- mutate(x, Year = as.numeric(substr(Date.Local, 1, 4)),
              Sample.Measurement = as.numeric(Sample.Measurement),
              Uncertainty = as.numeric(Uncertainty))
  # only merge those with blanks
  blank <- inner_join(xall, x)
  # blank correct, adjust uncertainty, rename parameter
  blank <- mutate(blank, Sample.Measurement = (Sample.Measurement) - mean, 
                  Uncertainty = sqrt((Uncertainty)^2 + unc^2),
                  Parameter.Name = paste0("badj-", Parameter.Name))
  blank <- select(blank, -unc, -mean, -Year)
  
  # remove non-blank corrected metals
  x1 <- anti_join(x, xall) %>% select(., -Year)
  # merge blank corrected with non-blank corrected
  x <- full_join(blank, x1)
  
  ###############
  # Replace BDL with 1/2 MDL, fix uncertainty
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
  
  ###############
  # OC/EC correct
  ecoc1 <- mutate(x, Month = as.numeric(substr(Date.Local, 6, 7)))
  necoc <- c("OC CSN Unadjusted PM2.5 LC TOT", "EC CSN PM2.5 LC TOT")
  # restrict to EC/OC TOT
  ecoc1 <- filter(ecoc1, Parameter.Name %in% c(necoc, paste0("badj-", necoc)))
  # What to do with detection?
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
    meas <- select(ecoc1, -Uncertainty) %>%
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
  unc <- select(ecoc1, -Sample.Measurement) %>%
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
  
  
  # Save new dataset
  write.csv(x, file = n2, row.names = F)
}



# # equation 8.10 in chapter 8
# #ec
# 1.3 * ec
# #oc
# (oc- 0.3 * ec - a) / m