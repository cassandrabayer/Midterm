



setwd('/Users/nancystetson/Google Drive/Midterm/Midterm Presentation')

library(tidyverse)
library(lubridate)
library(wesanderson)

# Load libraries ------------
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(rgeos)
library(sp)
library(shiny)


# Some data input and cleaning -------------
rents <- read.csv("rents_to_2000.csv")
rents$price <- as.integer(rents$rent)
rents$pes_minutes <- as.integer(rents$pes_minutes)
rents <- rents %>% mutate( commute = as.integer((pes_minutes + opt_minutes)/2))


# filter if rents are NA
rents <- rents %>% filter(!(is.na(price)))

#filter if optimisic time > 200
rents <- rents %>% filter(commute <200)
rents <- rents %>% filter(price < 100000)
rents <- rents %>% filter(sqft > 3)

palette <- rev(brewer.pal(11, 'RdYlBu'))
palette2 <- rev(brewer.pal(7, 'YlGnBu'))
