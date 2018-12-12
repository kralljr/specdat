# File to fix EC/OC values from technical manual

# load library
library(readxl)
library(tidyverse)

# Read in excel file with values
# Format to match with data
ecoc <- read_excel("data/ecoc-vals-ch2.xlsx")
ecoc <- ecoc[complete.cases(ecoc), ]
ecoc <- gather(ecoc, SubMethod, correct, -Type) %>%
  mutate(., Month = substring(Type, 2), 
         Month = match(Month, month.abb),
         Type = substr(Type, 1, 1))
ms <- filter(ecoc, Type == "M") %>% rename(., M = correct) %>% select(., -Month, -Type)
ecoc <- filter(ecoc, Type == "A") %>% rename(., A = correct) %>% select(., -Type)
ecoc <- full_join(ecoc, ms)
# Fix inconsistencies in method
ecoc$SubMethod[which(ecoc$SubMethod == "R&P-2025")] <- "R&PModel2025"
ecoc$SubMethod[which(ecoc$SubMethod == "R&P-2300")] <- "R&PModel2300"
ecoc$SubMethod[which(ecoc$SubMethod == "MetOne")] <- "Met"
ecoc$SubMethod[which(ecoc$SubMethod == "Anderson")] <- "Andersen"

save(ecoc, file = "data/ecoc-am.RData")