library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(janitor)
library(broom)

Apl_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202004-divvy-tripdata.csv")
May_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202005-divvy-tripdata.csv")
Jun_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202006-divvy-tripdata.csv")
Jul_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202007-divvy-tripdata.csv")
Aug_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202008-divvy-tripdata.csv")
Sep_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202009-divvy-tripdata.csv")
Oct_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202010-divvy-tripdata.csv")
Nov_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202011-divvy-tripdata.csv")
Dec_2020 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202012-divvy-tripdata.csv")
Jan_2021 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202101-divvy-tripdata.csv")
Feb_2021 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202102-divvy-tripdata.csv")
Mar_2021 = read_csv("/Volumes/Macintosh HD 2/Documents/Coursera/Goggle Data Analytics/Portfolio/Tripdata Csv/202103-divvy-tripdata.csv")

compare_df_cols(Apl_2020,May_2020,Jun_2020,Jul_2020,Aug_2020,Sep_2020,Oct_2020,Nov_2020,Dec_2020,Jan_2021,Feb_2021,Mar_2021, return = "mismatch")

trip_total<- rbind(Apl_2020,May_2020,Jun_2020,Jul_2020,Aug_2020,Sep_2020,Oct_2020,Nov_2020,Dec_2020,Jan_2021,Feb_2021,Mar_2021)
trip_total_c<- na.omit(trip_total)

unique(trip_total_c$rideable_type)
unique(trip_total_c$member_casual)

trip_total_c$trip_duration<-as.numeric(difftime(trip_total_c$ended_at,trip_total_c$started_at, units ='mins'))

trip_total_c$weekday<-weekdays(trip_total_c$started_at,abbreviate = FALSE)

trip_total_c$month<-format(trip_total_c$started_at,"%m")

trip_total_c<-trip_total_c %>% 
  distinct(ride_id,.keep_all = TRUE)

trip_total_c<-trip_total_c %>% 
  filter(trip_duration>0)


trip_total_c %>% 
  group_by(member_casual) %>% 
  summarise(n=n()) %>% 
  mutate(percent = n*100/sum(n))

avg_duration <-trip_total_c %>% 
  group_by(member_casual) %>% 
  summarise(average_trip_time=mean(trip_duration))

ggplot(avg_duration, aes(member_casual, y = average_trip_time, fill = member_casual))+
  geom_col() + 
  labs(title="Average trip duration Casual vs Member")+scale_fill_brewer("Paired")

avg_duration_per_weekday <- trip_total_c%>%
  group_by(member_casual, weekday)%>%
  summarise(average_trip_time = mean(trip_duration))

avg_duration_per_weekday$weekday <- factor(avg_duration_per_weekday$weekday, levels= c( "Monday", 
                                                                                        "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
avg_duration_per_weekday[order(avg_duration_per_weekday$weekday),]

ggplot(avg_duration_per_weekday, aes(weekday, average_trip_time, fill=member_casual))+
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual(values = c("casual"="light blue", "member"="pink"))
  labs(title="Average duration of trip per weekday")




