# Garbage file to check data
# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)


# Load constituents
load("data/cons.RData")

###########################
# CHECKING FOR DATA CONSISTENCY
###########################



###########################
# Get column headers for subsetting 
x <- read.csv("data/Krall_speciation.csv", nrows = 100)
header <- colnames(x)
cc1 <- apply(x, 2, class)


###########################
# CHECK QUALIFIERS
cc <- cc1
keeps <- c("Qualifiers", "Sample.Measurement")
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
qual <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )

# Reduce to non-missing rows for qual
qual <- filter(qual, !is.na(Qualifiers) & Qualifiers != "")
qual <- qual[complete.cases(qual), ]



###########################
# CHECK DATE OF LAST CHANGE
cc <- cc1
keeps <- c("Sample.Measurement", "Date.of.Last.Change")
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
date1 <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )
           
# Reduce to non-missing rows 
date1 <- rename(date1, Date = `Date of Last Change`)
date1 <- dplyr::filter(date1, !is.na(Date) & Date != "")
           


###########################
# Check if ID variable is unique
cc <- cc1
keeps <- c("State.Code", "County.Code", "Site.Number", "POC", "Date.Local") 
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
id <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )

# Check dimensions of id
dim(id)
id <- id[!duplicated(id), ]
dim(id)


###########################
# Check if time variable is non-zero
cc <- cc1
keeps <- c("Parameter.Name", "Time.Local") 
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
time <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )
time1 <- time[(time$`Parameter Name` %in% cons) & time$`Time Local` != "00:00", ]
table(time1$`Time Local`, time1$`Parameter Name`)




