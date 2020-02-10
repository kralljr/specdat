# File to summarize blanks
library(knitr)
library(tidyverse)
library(EnvStats)
library(RColorBrewer)


#load constituents and monitors
load("data/cons.RData")
load("data/mons.RData")

# all files in the blanks folder
fp1 <- list.files("data/blanks")
#get all constituent blanks 
fp1 <- fp1[grep("-blanks.csv", fp1)]

# for each constituent
for(i in 1 : length(fp1)) {
  # read in data
  print(fp1[i])
  x <- read.csv(file.path( "data/blanks", fp1[i]), stringsAsFactors = F)
  
  # get ID, date, year
  x <- mutate(x, id = (as.character(id)), nc = nchar(id), 
              id = ifelse(nc == 10, paste0("0", id), id), 
              year = as.numeric(substr(Date.Local, 1, 4)),
              Date.Local = as.Date(Date.Local)) %>%
    # restrict to years
    filter(., year >= 2010) %>% 
    # remove id length
    select(., -nc)
  # Group by id, method (for each constituent)
  sum1 <- group_by(x, id, Method.Name) %>% 
    # get median blank across id, method (and count)
    summarise(blank= median(Sample.Measurement, na.rm = T), nc = n()) %>%
    # convert to numeric
    mutate(., blank = as.numeric(blank))
  
  # Add in parameter/constituent name 
  sum1 <- mutate(sum1, Parameter.Name = unique(x$Parameter.Name))

  # combine across constituents
  if(i == 1) {
    sumall <- sum1
  } else {
    sumall <- full_join(sumall, sum1)
  }
  
  
}


blanks <- sumall

# # add in submethod
# sub1 <- gsub(" ", "", "R & P Model 2025")
# sub2 <- gsub(" ", "", "R & P Model 2000")
# x <- mutate(x, SubMethod = Method.Name,
#             SubMethod = gsub("R & P Model 2025", sub1, SubMethod),
#             SubMethod = gsub("R & P Model 2000", sub2, SubMethod),
#             SubMethod = gsub("R&P MDL2300", "R&PModel2300", SubMethod),
#             SubMethod = sapply(strsplit(SubMethod, " "), function(x) x[[1]]))


# at least through March 2019
save(blanks, file = "data/blanks-sum-2010-2019.RData")