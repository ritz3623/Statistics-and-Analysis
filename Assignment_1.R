#This script contains the summary statistic and analysis on Taxi Service Dataset.
#Loading required libraries
library(dplyr)
library(chron)

#Step 1 : Acquire the Dataset 
tsd <- read.csv("TS_Dataset.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Step 2 : Refine the Dataset
#Correction of spelling mistakes
tsd <- tsd %>% rename(ID = ï..ID)

#Selecting the important variable for statistic
tsd <- tsd %>% select(1:3, dtArrive, dtBegin, Sum, ServiceID, 17:20, DistanceTotal, RouteLength)

#Filter out the unwanted obeservations
tsd <- tsd %>% filter(CarID != "NULL", dtArrive != "NULL", dtBegin != "NULL", AddrFromGpsY != "NULL", AddrToGpsX != "NULL", RouteLength != "NULL")

#Convert dtArrive and dtBegin into HH:mm:ss format
tsd$dtArrive <- gsub(".\\d$", "", tsd$dtArrive)
tsd$dtBegin  <- gsub(".\\d$", "", tsd$dtBegin)
#The oders in dataset are captured from 12:00 to 12:59 (1 hour)
tsd$dtArrive <- paste0("12:", tsd$dtArrive)
tsd$dtBegin <- paste0("12:", tsd$dtBegin)

#calculate difference in seconds between dtBegin and dtArrive and store the result in separate variable as "time_to_reach"  
tsd$dtArrive <- chron(times. = tsd$dtArrive)
tsd$dtBegin <- chron(times. = tsd$dtBegin)
tsd$time_to_reach <- tsd$dtBegin - tsd$dtArrive 
ch <- times(tsd$time_to_reach)
tsd$time_to_reach <- 60 * 60 * hours(ch) + minutes(ch) *60 + seconds(ch)
tsd$time_to_reach <- round(tsd$time_to_reach, digits = 0)

#Step 3 : Explore the dataset
#Q1 Unique number of clients/users?
unique_user_cnt <- n_distinct(tsd$Client)

#Q2: Top 10 clients/users?
#2.1 Highest revenue Generator
top_10_user_by_revenue <- tsd %>% group_by(Client) %>% summarise(Revenue = sum(Sum)) %>% arrange(desc(Revenue)) %>% head(10)
#2.2 Regular service consumers
top_10_user_by_regularity <- tsd %>% group_by(Client) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>% head(10)  

#Q3: How are starting points distributed? Is there a specific region in which taxi company is seeing very high demand?
plot(x = tsd$AddrFromGpsX, y = tsd$AddrFromGpsY, type = "p", main = "Pick up point Distribution", xlab = "longitude", ylab = "latitude")
top_region <- tsd %>% group_by(AddrFromGpsX, AddrFromGpsY)  %>% summarise(popularity = n()) %>% arrange(desc(popularity)) %>% head(1)

#Q4 What is the typical cart size/ cost per ride?
cost_per_ride <- mean(tsd$Sum)

#Q5: Find out the regular customer?
regular_customer <- tsd %>% group_by(Client) %>% summarise(rides_cnt = n()) %>% arrange(desc(rides_cnt)) %>% head(10)  

#Q6: Which distance regular customers travels? (large distance or short distance)
tsd_without_zero_dist <- tsd %>% filter(DistanceTotal != 0)
regular_customer_type <- tsd_without_zero_dist %>% group_by(Client) %>% summarise_each(funs(mean, median), DistanceTotal) 
regular_customer_type <- dplyr::left_join(regular_customer, regular_customer_type, by = "Client")

#Q7: Find most crowded pick up points?
Top_pickups_points <- tsd %>% group_by(AddrFromGpsX, AddrFromGpsY) %>% summarise(customer_cnt = n()) %>% arrange(desc(customer_cnt)) %>% head(5)

#Q8: What is the average cost paid by any customer?
avg_ride_cost_customer <- tsd %>% group_by(Client) %>% summarise(typical_cost = mean(Sum)) %>% arrange(desc(typical_cost)) %>% head(10)

#Q9: Are regular customers tends to choose specific car if yes then what average distance they travels using that car?
regular_customer_details <- dplyr::left_join(regular_customer, tsd, by = "Client") %>% select(Client, CarID, DistanceTotal)
regular_customer_car <- regular_customer_details %>% 
  group_by(Client, CarID) %>% 
  summarise(freq_car_used = n()) %>% 
  arrange(desc(freq_car_used)) %>% 
  top_n(1) %>% 
  filter(freq_car_used > 2) %>% 
  dplyr::left_join(., regular_customer_details, by = c("Client", "CarID")) %>% 
  select(Client, CarID, DistanceTotal) %>%
  group_by(Client, CarID) %>% 
  summarise(avg_dist = mean(DistanceTotal)) %>% 
  arrange(desc(avg_dist))

#Q10: Which type of car is more popular among the customers?
car_popularity <- tsd %>% group_by(CarID) %>% summarise(Total_Revenue = sum(Sum)) %>% arrange(desc(Total_Revenue)) %>% head(10)

#Q11: What is the average time car driver takes to reach at suggested pickup points after customer placed the order?
avg_time_by_driver <- tsd %>% group_by(CarID) %>% summarise(avg_time = mean(time_to_reach)) %>% arrange(avg_time) %>% head(10)

#Q12: From which pick up points company gets customer who travels large distance?
large_dist_points <- tsd %>% group_by(AddrFromGpsX, AddrFromGpsY) %>% summarise(large_dist = max(DistanceTotal))
large_dist_points <- large_dist_points[order(large_dist_points$large_dist, decreasing = TRUE),]
head(large_dist_points, 10)