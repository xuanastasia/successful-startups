library(tidyverse)
library(readxl)
library(jsonlite)
library(cluster)
library(DescTools)
library(GGally)
library(gridExtra)
library(sf)
library(lubridate)
library(dplyr)

#How Many New Businesses Fail ?
#How Many New Businesses Secsees ?
#Reasons for Failing
#How to Avoid Failing

getwd()

setwd("C:/Users/huiyx/Documents/NUS XUAN/NUS Y2S2/projects/code")

startup_data <- read_csv("../data/big_startup_secsees_dataset.csv")
View(startup_data)
status <- startup_data %>% select(status)
unique(status)#operating #acquired #closed #ipo
#success -> ipo and acquired 
#failure -> closed

failed <- startup_data %>% 
  filter(status == "closed")

count(failed) #6238
count(success) #7096

success <- startup_data %>% 
  filter(status == "ipo" | status == "acquired")

#inspecting by region
region <- success %>% select(region)
sucess_reg <- success %>% 
  select(region) %>%
  group_by(region) %>% ungroup() %>%
  mutate(number = count(region))

s_by_region <- group_by(success, region)%>%
  summarise(count = n(), .groups="drop")

#plotting on map to view
mapdata <- map_data("world")
View(mapdata)
mapdata <- left_join(mapdata, s_by_region, by="region") 
View(mapdata)


s_mapdata <- mapdata %>%
  filter(!is.na(mapdata$count))
  
View(s_mapdata)

s_map <- ggplot(s_mapdata, aes(x = long, y = lat, group = group))+
    geom_polygon(aes(fill=count), color="black")


#inspecting by country
#success
s_by_country <- group_by(success, country_code)%>%
  summarise(success_count = n(), .groups="drop")

#failure
f_by_country <- group_by(failed, country_code)%>%
  summarise(failed_count = n(), .groups="drop")

#join both 
by_country <- full_join(s_by_country, f_by_country, by = "country_code")

by_country <- by_country %>%
  mutate(success_count = replace(success_count, is.na(success_count), 0) ) %>%
  mutate(failed_count = replace(failed_count, is.na(failed_count), 0) )

#calculate percentage of success based on country 
by_country <- by_country %>%
  mutate(success_count, failed_count, 
         success_rate = success_count/(success_count+failed_count) * 100)

#consider success_rate > 50% to be successful 
success_country <- by_country %>%
  filter(success_rate > 50) %>%
  arrange(desc(success_rate))
View(success_country)

#plot graph of country correlation to success
country_graph <- ggplot(success_country) +
  geom_col(mapping=aes(x=country_code, y=success_rate),fill = "red", col = "red") + 
  labs(title="The Success Rate of Start-ups in Each Country", 
       y="Success Rate (%)",x="Country")

#countries include: Belgium, Bermuda, Canada, Switzerland
#China, Germany, Finland, Croatia, Ireland, Israel, Japan,
#Saint Martin, Singapore, Trinidad & Tobago, Uruguay, USA

#Top 5 Countries with high success rate for start-ups (100% here)
#Bermuda, Croatia, Saint Martin, Trinidad & Tobago, Uruguay
#However, we notice that this is an unfair comparison as these 5 countries
#just happened to have very limited dataset (0 failure case, 1-2 success)
#hence its more reliable to look at the other countries 

#USA has the largest amount of start-up, followed by Canada, Israel, then China
#It is fair to consider all countries in the list with more cases
#in a sense the "top 5  100% success rate countries" might not even be considered
#as start-up nations due to the limited amount of start-ups they have in the dataset


