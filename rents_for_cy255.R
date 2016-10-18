library(tidyverse)

rents1 <- read_csv("rents_to_1000.csv")
# Some data input and cleaning -------------
rents2 <- read.csv("rents_to_2000.csv")
rents3 <- read_csv('rents_to_3000.csv')
rents <- rbind(rents1, rents2)
rents <- rbind(rents, rents3)
rents$price <- as.integer(rents$rent)
rents$pes_minutes <- as.integer(rents$pes_minutes)
rents <- rents %>% mutate( commute = as.integer((pes_minutes + opt_minutes)/2))


# filter if rents are NA
rents <- rents %>% filter(!(is.na(price)))

#filter if optimisic time > 200
rents <- rents %>% filter(commute <200)
rents <- rents %>% filter(price < 100000)
rents <- rents %>% filter(sqft > 3)


write_csv(rents, 'rents_3000long.csv')
