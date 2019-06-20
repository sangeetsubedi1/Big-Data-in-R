# installing the required packages
install.packages("tidyverse")
install.packages("nycflights13")

# Load the installed package into R sessions
library(nycflights13)
library(tidyverse)

# installing the required packages
install.packages("tidyverse")
install.packages("nycflights13")

# Load the installed package into R sessions
library(nycflights13)
library(tidyverse)

## Link to the Book: https://r4ds.had.co.nz/transform.html

# Below are the solution to the excercise, please be sure to 
# run them as per line individually as per your need. 
#EXERCISE 5.2.4
#1.
arr_delay_2_or_more_hours<- filter(flights, arr_delay >= 120)
flew_to_houston<- filter(flights, dest == "IAH" | dest == "HOU")
operator_UA_AE_DL <- filter(flights, carrier =="UA"| carrier == "DL" | carrier == "AA")
departed_in_summer <- filter(flights, month == c(7:9))
arrived_more_than_2hrs_late_left_on_time <- filter(flights, arr_delay>=120 & dep_delay <= 0)
delayed_atleast_1hr <- filter(flights, dep_delay >= 60 & arr_delay <= (dep_delay - 30))
departed_between_00to06 <- filter(flights, dep_time >= 0000 & dep_time <= 0600)

#2 dplyr filtering helper --> between(). It is only for numeric values. 
# it will determine the values between x and y, where x is the lower numeric bound and y is upper.
#and return either TRUE or FALSE
between_00_06 <- filter(flights, between(flights$dep_time,0000,0600))

#3 there are 8255 missing dep_time. That means the flights never took off
#the other missing values associated with it are arr_time, arr_delay, dep_delay and air_time.  
missing_val_dep_time <- filter (flights, is.na(flights$dep_time))

#EXERCISE 5.5.2
#MUTATE
#1
deptime_time_min<-transmute(flights,dep_time, deptime = (dep_time %/%100)*60 + dep_time %% 100)
schtime_time_min<-transmute(flights,sched_dep_time, schtime = (sched_dep_time %/%100)*60 + sched_dep_time %% 100)
#2
flight_sml<- select(flights, air_time, arr_time, dep_time)
compared_arr_dep<-mutate(flight_sml, 
       arr_time_min = (arr_time %/% 100) * 60 + (arr_time %% 100),
       dep_time_min = (dep_time %/% 100) * 60 + (dep_time %%100),
       trip_time_minutes = arr_time_min - dep_time_min)


#3 




#4 
head(arrange(flights, desc(row_number(flights$dep_delay))),10)
most_delayed <- head(arrange(flights, desc(min_rank(flights$dep_delay))),10)

# EXERCISE 5.6.7

#4 
flights %>% group_by(year, month, day) %>% summarise(
  canceled_no = sum(is.na(air_time) | air_time == 0), 
  avg_arr_delay = mean(arr_delay, na.rm = TRUE),
  avg_dep_delay = mean(dep_delay, na.rm = TRUE)
)%>%
  select(year, month, day, canceled_no, avg_arr_delay, avg_dep_delay) %>%
  filter(avg_arr_delay > 0) %>%
  ggplot()+
  geom_point(aes(x = avg_arr_delay, y = canceled_no, color = "red"))+
  geom_smooth(aes(x = avg_arr_delay, y = canceled_no, color = "red"), se =FALSE,method = "gam") +
  geom_point(aes(x = avg_dep_delay, y = canceled_no,color =  "blue"))  +
  geom_smooth(aes(x = avg_dep_delay, y = canceled_no,color =  "blue"), se =FALSE, method = "gam") 


#5
not_cancelled <- filter(flights, flights$air_time > 0)


not_cancelled %>% group_by(carrier) %>% summarise(count = n(),
                                                  median_arr_delay = median(arr_delay), 
                                                  avg_arr_delay = mean(arr_delay)
                                                  )%>%
  filter(count > 1000)



#Exercise 5.7.1

#2 worst tailnum
not_cancelled %>% group_by(tailnum) %>%summarise(count = n(),
                                                 max_arr_delay = max(arr_delay),
                                                 is_on_time_freq = mean(arr_delay<= 0, na.rm = TRUE)
                                                 )%>%
            filter(count < 30) %>% arrange (desc(is_on_time_freq))






