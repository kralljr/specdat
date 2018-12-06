# File to create monitor subset data
# Load data from 2013 paper
load("/Users/jenna/Dropbox/PM25cons_mort/FinalR_815/Prednaivedata_17aug11.RData")

# Get monitor IDs
monsin <- unlist(monsin)
monsin <- substr(monsin, 1, 9)


# Subset yearly data for 2000-2005
years <- seq(2000, 2005)
k <- 1
for(i in years) {
  print(i)
  # Name with year
  n1 <- paste0("data/Krall_speciation-", i, "-clean.csv")
  
  # Load data
  x <- fread(file = n1, sep=",")
  # Create id and filter
  x <- mutate(x, id = paste0(State.Code, County.Code, Site.Number)) %>%
    filter(., id %in% monsin)
  
  if(k == 1) {
    datall <- x
  } else {
    datall <- full_join(datall, x)
  }
  
  k <- k + 1
}
save(datall, file = "data/datall-monsubset.RData")



# separate into list by monitor 
datall1 <- select(datall, id, Date.Local, Parameter.Name, Sample.Measurement, Uncertainty)
# Keep only desired constituents
params <- unique(datall1$Parameter.Name)
drop <- params[c(grep("OC", params), grep("EC", params), grep("OP", params))]
drop <- drop[-grep("-adj", drop)]
datall1 <- filter(datall1, !(Parameter.Name %in% drop))

# measurement/uncertainty
meas <- select(datall1, -Uncertainty) %>%
  group_by(., id, Date.Local, Parameter.Name) %>%
  # Need to average by monitor?  Some EC with two measurements for same day?
  summarize(., meas = mean(Sample.Measurement)) %>%
  spread(., Parameter.Name, meas) %>%
  ungroup()
unc <- select(datall1, -Sample.Measurement) %>%
  group_by(., id, Date.Local, Parameter.Name) %>%
  # Need to average by monitor?  Some EC with two measurements for same day?
  summarize(., meas = mean(Uncertainty)) %>%
  spread(., Parameter.Name, meas) %>%
  ungroup()

# Unique IDs
unid <- unique(datall1$id)
# Separate data for each id
for(i in 1 : length(unid)) {
  # create separate measurement, uncertainty files
  meas1 <- filter(meas, id == unid[i]) %>% select(., -id)
  unc1 <- filter(unc, id == unid[i]) %>% select(., -id)
  

  meas1 <- meas1[complete.cases(meas1), ]
  unc1 <- unc1[complete.cases(unc1), ]
  print(c(unid[i], nrow(meas1), nrow(unc1)))
  # for those with mroe than 50 complete observations, save data
  if(nrow(meas1) > 50) {
    write.csv(meas1, file = paste0("data/monid/conc-", unid[i], ".csv"), row.names = F)
    write.csv(unc1, file = paste0("data/monid/unc-", unid[i], ".csv"), row.names = F)
    
  }
}