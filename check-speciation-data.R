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
date1 <- mutate(date1, Date = as.Date(Date))

# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2010-03-15" "2013-03-14" "2015-07-16" "2014-11-17" "2015-11-30" "2018-10-17" 

###########################
# Check if ID variable is unique
cc <- cc1
keeps <- c("State.Code", "County.Code", "Site.Number", "POC", "Date.Local", "Parameter.Name") 
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
id <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )

id1 <- filter(id, `Parameter Name` == cons[22])

# Check dimensions of id
dim(id1)
id1 <- id1[!duplicated(id1), ]
dim(id1)


###########################
# Check if time variable is non-zero
cc <- cc1
keeps <- c("Parameter.Name", "Time.Local") 
cc[-which(header %in% keeps)] <- "NULL"
names(cc) <- NULL
time <- fread(file="data/Krall_speciation.csv", sep=",", colClasses = cc )
time1 <- time[(time$`Parameter Name` %in% cons) & time$`Time Local` != "00:00", ]
table(time1$`Time Local`, time1$`Parameter Name`)

#   00:08    09:42    09:30    08:40    16:15    11:40    10:35    08:55    11:47    15:42    15:45 
# 142      143      146      147      147      148      149      150      155      155      158 
# 08:45    13:45    13:50    06:00    08:42    14:50    10:55    09:15    12:20    10:15    11:10 
# 161      170      170      171      172      173      175      180      182      186      187 
# 05:00    14:25    09:35    13:15    00:15    11:05    18:00    09:05    10:40    03:00    17:00 
# 188      189      191      193      197      198      204      207      213      216      242 
# 12:45    13:30    14:20    11:30    15:30    02:00    16:30    14:15    12:15    00:06    12:30 
# 252      266      274      294      314      320      324      346      348      350      353 
# 14:45    23:59    11:15    15:15    07:00    10:45    14:30    11:45    10:30    16:00    15:00 
# 361      393      394      449      454      468      503      522      617      725      869 
# 08:00    00:04    14:00    09:00    13:00    10:00    11:00    00:03    01:00    23:00    12:00 
# 888      924     1125     1311     1379     1648     2232     2303     2390     2486     3940 
# 00:05    00:02    00:01    00:00 
# 4515    15776   166437 46475343 


