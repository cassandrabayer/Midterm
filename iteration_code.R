# Note: to run, get API key for computer
# change code to next 1000 listings (they are randomized previously)
# change all rents_to_???? to the next 1000


setwd("/Users/nancystetson/Google Drive/Midterm/Midterm Presentation")

library(lubridate)
library(tidyverse)
library(gmapsdistance)

rents_index <- read_csv('rents_indexed.csv')

set.api.key('AIzaSyABWtZiob-wCCGzItapg0cMEs5a--KcWZ0')

thursday <- as.numeric(ymd_hms("2016-10-20 09:00:00 PDT"))
# lubridate broken, but internet tells me it is 1475769600 (doesn't seem right actually)


rents_to_5000 <- rents_index[4001:5000,]

ptm <- proc.time()
opt <- gmapsdistance(origin = rents_to_5000$location, # 100 locations
                     destination = "37.784765+-122.407737",  #Downtown SF
                     mode = "driving",
                     traffic_model = "optimistic", # Can also be pessimistic
                     arrival = thursday) 


pes <- gmapsdistance(origin = rents_to_5000$location, # 100 locations
                     destination = "37.784765+-122.407737",  #Downtown SF
                     mode = "driving",
                     traffic_model = "pessimistic", # Can also be pessimistic
                     arrival = thursday) 
proc.time() - ptm


optimistic <- as.data.frame(opt$Time)
pessimistic <- as.data.frame(pes$Time)

optimistic$opt_time <- optimistic$`Time.37.784765+-122.407737`
optimistic$pes_time <- pessimistic$`Time.37.784765+-122.407737`

time_to <- optimistic
time_to <- time_to %>% 
  select(-`Time.37.784765+-122.407737`)

time_to  <- time_to %>% 
  mutate(opt_minutes = opt_time /60) %>%
  mutate(pes_minutes = pes_time / 60)

rents_to_5000 <- left_join(time_to, rents_to_5000, by = c("or" = "location"))

write_csv(rents_to_5000, "rents_to_5000.csv")


# read in rent data
# add line for new data
rents1 <- read_csv("rents_to_1000.csv")
rents2 <- read.csv("rents_to_2000.csv")
rents3 <- read_csv('rents_to_3000.csv')
rents4 <- read_csv('rents_to_4000.csv')
rents5 <- rents_to_5000


# bind together
rents <- rbind(rents1, rents2)
rents <- rbind(rents, rents3)
rents <- rbind(rents, rents4)
rents <- rbind(rents, rents5)



# Some data input and cleaning -------------
rents$price <- as.integer(rents$rent)
rents$pes_minutes <- as.integer(rents$pes_minutes)
rents <- rents %>% mutate( commute = as.integer((pes_minutes + opt_minutes)/2))


# filter if rents are NA
rents <- rents %>% filter(!(is.na(price)))

#filter if optimisic time > 200
rents <- rents %>% filter(commute <200)
rents <- rents %>% filter(price < 10000)
#rents <- rents %>% filter(sqft > 3)

write_csv(rents, 'rents_5000long.csv')
