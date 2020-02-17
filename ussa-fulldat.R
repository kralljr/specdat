# File to take Speciation data e-mailed from EPA
# Creates:
# - monitor data
# - gets constituents of interest
# - cleans year data to ensure only correct years in each file (Date of last change issue)
# - create chdate


# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)


###########################
# Get column headers for subsetting and year data
master <- "data/Krall_speciation.csv"
x <- read.csv(master, nrows = 100)
header <- colnames(x)

###########################
# Save monitor information
cc1 <- apply(x, 2, class)
cc <- cc1
keeps <- c("State.Code", "County.Code", "Site.Number", "POC",
           "Latitude", "Longitude", "State.Name", "County.Name")
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL

moninfo <- fread(file = master, sep = ",", colClasses = cc )
# Reduce to non-duplicated rows
# rows: 46713689
rows <- nrow(moninfo)
moninfo <- moninfo[-which(duplicated(moninfo)), ]
save(moninfo, file = "data/moninfo.RData")




###########################
# Year data
###########################





###########################
# Load one year, get parameter names of interest
n1 <- "data/Krall_speciation-2000.csv"
x <- read.csv(n1, nrows = 100, header = F)
colnames(x) <- header
cc1 <- apply(x, 2, class)
cc <- cc1
cn <- colnames(x)

# Remove to be dropped
drops <- c("ParameterCode", "Latitude", "Longitude", "Datum",
           "Date.GMT", "Time.GMT", "Method.Code",
           "State.Name", "County.Name", "Date.of.Last.Change")
cc[drops] <- rep("NULL", length = length(drops))
names(cc) <- NULL

x <- fread(file = n1, sep=",", colClasses = cc)
header1 <- header[-which(header %in% drops)]
colnames(x) <- header1

# List of parameters to keep
cons <- c("PM2.5 - Local Conditions", "Arsenic PM2.5 LC", "Aluminum PM2.5 LC" , 
          "Bromine PM2.5 LC", "Calcium PM2.5 LC" , "Chromium PM2.5 LC", "Copper PM2.5 LC",                       
          "Chlorine PM2.5 LC", "Iron PM2.5 LC" , "Lead PM2.5 LC", "Manganese PM2.5 LC",                    
          "Nickel PM2.5 LC", "Magnesium PM2.5 LC", "Phosphorus PM2.5 LC", "Selenium PM2.5 LC",                     
           "Titanium PM2.5 LC",  "Vanadium PM2.5 LC",  "Silicon PM2.5 LC",  "Zinc PM2.5 LC",                         
          "Strontium PM2.5 LC", "Potassium PM2.5 LC", "Total Nitrate PM2.5 LC",                
          "OC PM2.5 LC TOR",  "EC PM2.5 LC TOR",   "OC1 PM2.5 LC",  "OC2 PM2.5 LC",  "OC3 PM2.5 LC",                          
           "OC4 PM2.5 LC",   "OP PM2.5 LC TOR",   "EC1 PM2.5 LC",   "EC2 PM2.5 LC",  "EC3 PM2.5 LC",                          
          "Sulfate PM2.5 LC" , "Ammonium Ion PM2.5 LC"  ,"Sodium Ion Pm2.5 LC",
          "OCX Carbon PM2.5 LC",  "OC CSN Unadjusted PM2.5 LC TOT",  "EC CSN PM2.5 LC TOT",
          "Barium PM2.5 LC", "Sodium PM2.5 LC", "Sulfur PM2.5 LC")
        
save(cons, file = "data/cons.RData")
# List of parameters to drop:  42 - 3(sodium, barium, sulfur)
# "Rubidium PM2.5 LC", 
# "Zirconium PM2.5 LC"                    
# [34] "Chloride PM2.5 LC"  
# [1] "Sample Flow Rate- CV"                  
# [2] "Sample Volume"                         
# [3] "Ambient Min Temperature"               
# [4] "Ambient Max Temperature"               
# [5] "Average Ambient Temperature"           
# [6] "Sample Min Baro Pressure"              
# [7] "Sample Max Baro Pressure"              
# [8] "Average Ambient Pressure"    
# "Acceptable PM2.5 AQI & Speciation Mass"
# [48] "Antimony PM2.5 LC"                     
# [50] "Cadmium PM2.5 LC"                      
# [51] "Cobalt PM2.5 LC"                       
# [52] "Cerium PM2.5 LC"                       
# [53] "Cesium PM2.5 LC"                       
# [54] "Europium PM2.5 LC"                     
# [55] "Gallium PM2.5 LC"                      
# [56] "Hafnium PM2.5 LC"                      
# [57] "Indium PM2.5 LC"                       
# [58] "Iridium PM2.5 LC" 
# [59] "Molybdenum PM2.5 LC"                   
# [60] "Mercury PM2.5 LC"                      
# [61] "Gold PM2.5 LC"                         
# [62] "Lanthanum PM2.5 LC"                    
# [63] "Niobium PM2.5 LC"                      
# [64] "Tin PM2.5 LC"                          
# [65] "Samarium PM2.5 LC"                     
# [66] "Scandium PM2.5 LC"
# [67] "Silver PM2.5 LC"                       
# [68] "Tantalum PM2.5 LC"                     
# [69] "Terbium PM2.5 LC"                      
# [70] "Yttrium PM2.5 LC"                      
# [71] "Tungsten PM2.5 LC"   
# "Potassium Ion PM2.5 LC"
# "Carbonate Carbon CSN PM2.5 LC TOT"     
# [79] "Volatile Nitrate PM2.5 LC"             
# [80] "Non-volatile Nitrate PM2.5 LC"   




###########################
# Fix year datasets (dates within year)
###########################

years <- seq(2000, 2019)
#years <- c(2018, 2019)
nrow1 <- vector()
k <- 1
for(i in years) {
  print(i)
  # Name with year
  n1 <- paste0("data/Krall_speciation-", i, ".csv")
  # Load data
  if(i <= 2017) {
    x <- fread(file = n1, sep=",", colClasses = cc)
  } else {
    x <- fread(file = n1, sep=",", colClasses = cc[cc!= "NULL"])
  }
  # Apply appropriate header
  colnames(x) <- header1
  
  # Limit to only correct years
  x <- filter(x, substr(Date.Local, 1, 4) == paste(i))
  
  # Check number of rows
  nrow1[k] <- nrow(x)
  
  # Remove qualifiers
  # Qualifier = 2– Data does not meet a particular criteria,
  #but has been determined to be valid
  # MDL is from 2018-2019
  x <- filter(x, Qualifiers == "" | Qualifiers == 2 |
                is.na(Qualifiers) | Qualifiers == "MD - Value less than MDL.", 
              Qualifiers == "MD", Time.Local == "00:00")
  

  n2 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
  write.csv(x, file = n2, row.names = F)
  
  k <- k + 1
}
# Check rows are equal
#all.equal(sum(nrow1[1 : 18]), rows)








###########################
# Create methods dataset, fix yearly data (which columns)
# years of data
years <- seq(2000, 2019)
#years <- c(2018, 2019)
dates <- matrix(nrow = length(years), ncol = 2)
k <- 1
for(i in years) {
  print(i)
  # Name with year
  n1 <- paste0("data/Krall_speciation-", i, "-chdate.csv")
  # Load data
  x <- fread(file = n1, sep=",")
  # Apply appropriate header
  colnames(x) <- header1
  
  # verify dates
  dates[k, ] <- c(min(x$Date.Local, na.rm = T), max(x$Date.Local, na.rm = T))
  
  # fix method for EC/OC correction
  sub1 <- gsub(" ", "", "R & P Model 2025")
  sub2 <- gsub(" ", "", "R & P Model 2000")
  x <- mutate(x, SubMethod = Method.Name,
              SubMethod = gsub("R & P Model 2025", sub1, SubMethod),
              SubMethod = gsub("R & P Model 2000", sub2, SubMethod),
              SubMethod = gsub("R&P MDL2300", "R&PModel2300", SubMethod),
              SubMethod = sapply(strsplit(SubMethod, " "), function(x) x[[1]]))
  x <- dplyr::filter(x, Parameter.Name %in% cons)
  
  # Get method information
  method <- select(x, Parameter.Name, Unit.of.Measure, Method.Type, Method.Name, SubMethod) 
  method <- method[-which(duplicated(method)), ]
  method <- mutate(method, year = i)
  
  # save method information
  if(k == 1) {
    methodsall <- method
  } else {
    methodsall <- full_join(methodsall, method)
  }
  
  
  
  # fix data
  x <- dplyr::select(x, -Method.Type, -Unit.of.Measure, -Qualifiers, -Time.Local)
  write.csv(x, file = n1, row.names = F)
  
  
  k <- k + 1
}
print(dates)

save(methodsall, file = "data/methods-summ.RData")
