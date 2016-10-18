setwd('/Users/nancystetson/Google Drive/Midterm')

library('tidyverse')
library(lubridate)
library(wesanderson)


#commute <- read_csv('commute_times_us_zipcode_2011.csv')

#zcta <- foreign::read.dbf('Shapefiles/ZillowNeighborhoods-CA/ZillowNeighborhoods-CA.dbf')

#rent_commute <- left_join(rent, commute, by.x = 'RegionName', by.y = )

afford <- read_csv("Affordability_Wide_2016Q2_Public.csv")




afford <- afford %>% 
              select(-HistoricAverage_1985thru1999) %>%
              gather(year, value,  5:154)

afford$Index <- gsub(" ", "", afford$Index)

afford <- afford %>% spread(Index, value)

afford$quarter <- substr(afford$year, start = 6, stop=7)
afford$year1 <- substr(afford$year, start = 1, stop=4)

afford <- afford %>%
  mutate(quarter1 = ifelse(quarter=="03", "-03-01", 
                           ifelse(quarter=="06", "-06-01", 
                                  ifelse(quarter=="09", "-09-01", "-12-01")))) %>%
  mutate(date = as_date(paste(year1,quarter1,sep="")))

write_csv(afford, "afford.csv")

#sf <- afford %>% filter(RegionName == 'San Francisco, CA')
#ny <- afford %>% filter(RegionName == 'New York, NY')
#chi <- afford %>% filter(RegionName == 'Chicago, IL')


#names(sf)

ggplot(sf, aes(date, RentAffordability)) + geom_path() +
  geom_path(data= ny, aes(date, RentAffordability), color = "indianred") +
  geom_path(data = chi, aes(date, RentAffordability), color = "cadetblue") +
  xlab("Year") + ylab("Income for Rent") +
  ggtitle("Percent of Median Income Spent on Median Rent") +
  theme_minimal()

########################################################################
# Note: The following graphs have been formatted in midterm_slides.Rmd

# Try a different way

cities3 <- afford %>% filter(RegionName=='New York, NY' | 
                             RegionName == 'San Francisco, CA' |
                             RegionName == 'Chicago, IL' |
                             RegionName == 'Atlanta, GA')




ggplot(cities3, aes(date, RentAffordability, color=RegionName)) +
  geom_path(size=1) +
  #geom_path(data= filter(cities3,RegionName=='San Francisco, CA'),
             # aes(date, RentAffordability)) +
  xlab("Year") + ylab("Income for Rent") +
  ggtitle("Percent of Median Income Spent on Median Rent") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5, .05)) +
  scale_color_manual(values = c( 'lightblue2', 'lightblue3','lightblue4', 'firebrick'), 
                     guide = guide_legend(title = NULL)) +
  theme_minimal() 



# But what if you moved to the suburbs?
cities_near <- afford %>% filter(
                                RegionName=='Santa Rosa, CA' | 
                               RegionName == 'Vallejo, CA' |
                               #RegionName == 'Sacramento, CA'|
                              RegionName == 'San Jose, CA' |
                                #RegionName == 'Stockton, CA'
                              RegionName == 'San Francisco, CA' 
                              )



ggplot(cities_near, aes(date, RentAffordability, color=RegionName)) +
  geom_path() +
  geom_path(data= filter(cities_near,RegionName=='San Francisco, CA'),
    aes(date, RentAffordability), color='firebrick', size = 1) +
  geom_path(data= filter(cities_near,RegionName=='Vallejo, CA'),
            aes(date, RentAffordability), color='darkorange3', size = 1) +
  
  scale_color_manual(values = c( 'firebrick', 'goldenrod','goldenrod1', 'darkorange3'), 
                     guide = guide_legend(title = NULL)) +
  
  xlab("Year") + ylab("Income for Rent") +
  ggtitle("Percent of Median Income Spent on Median Rent") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5, .05)) +
 
  theme_minimal() 
