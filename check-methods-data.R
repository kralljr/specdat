# Set working directory to home folder

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Load methods
load("data/methods-summ.RData")

### CHECK METHODS

# Check methods for each constituent
t1 <- table(methodsall$Parameter.Name, methodsall$Method.Name)
# Restrict to non-OC/EC
gr <- c(grep("EC", rownames(t1)), grep("OC", rownames(t1)), grep("OP", rownames(t1)), 
        which(rownames(t1) %in% c("PM2.5 - Local Conditions", "Ammonium Ion PM2.5 LC", 
                                  "Sodium Ion Pm2.5 LC", "Total Nitrate PM2.5 LC", "Sulfate PM2.5 LC")))
t2 <- t1[-gr, ]
t2 <- t2[, which(colSums(t2) > 0)]
t(t2)

# Methods for ions
gr <- c(which(rownames(t1) %in% c("Ammonium Ion PM2.5 LC", 
                                  "Sodium Ion Pm2.5 LC", "Total Nitrate PM2.5 LC", "Sulfate PM2.5 LC")))
t2 <- t1[gr, ]
t2 <- t2[, which(colSums(t2) > 0)]
t(t2)

# Methods for PM2.5 (all gravimetric)
gr <- c(which(rownames(t1) %in% c("PM2.5 - Local Conditions")))
t2 <- t1[gr, ]
t2 <- t2[which(t2 > 0)]
t(t2)

## FRM vs. FEM
m1 <- filter(methodsall, Parameter.Name == "PM2.5 - Local Conditions")
table(m1$Method.Type)

# Methods of interest for OC/EC tot (2000-2005 approach)
gr <- which(rownames(t1) %in% c("OC CSN Unadjusted PM2.5 LC TOT", "EC CSN PM2.5 LC TOT"))
t2 <- t(t1[gr, ])
t2[which(rowSums(t2) > 0), ]


# Check units (all mug/m3)
t1 <- table(methodsall$Parameter.Name, methodsall$Unit.of.Measure)
t1 

# Double check SubMethod
nmeth <- c("R&P MDL2300", "Model 2025", "R&P MDL 2300", 
           "R & P Partisol 2000", "R & P Model 2300")

gr <- 0
for(i in 1 : length(nmeth)) {
  gr <- c(gr, grep(nmeth[i], methodsall$Method.Name))
}
gr <- gr[-1]

temp <- methodsall[gr, ]

# All R/R&P are non EC/OC
t2 <- table(methodsall$SubMethod, methodsall$Parameter.Name)
t2 <- t2[c("R", "R&P"), ]
t2[, colSums(t2) > 0]

t1 <- table(temp$Parameter.Name, temp$Method.Name)
t1 <- t1[which(rownames(t1) %in% c("OC CSN Unadjusted PM2.5 LC TOT", "EC CSN PM2.5 LC TOT") ), ]
t1[, colSums(t1) > 0 ]
