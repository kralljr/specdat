# File to blank correct metals and adjust EC/OC
# Set working directory to home folder

# Load libraries
library(tidyverse)


# Load blanks
load("data/blanks-all.RData")
load("data/ecoc-am.RData")



###########################
# years of data
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
  # uncdertainty info (drop sample measurement)
  unc <- select(ecoc1, Parameter.Name, State.Code, County.Code, Site.Number, POC,
                Date.Local, Uncertainty, Method.Name)
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