#Polen  YILMAZAY - 2016555070
library(tidyverse)
library(ggplot2)
library(lubridate)

data<-read_csv2("D:/Kitaplar/Data Science/assignment2_data.csv")
colnames(data)[1] <- 'Date'

set.seed(2016555070)

index <- sample(1:nrow(data),100)
mydata <- data[index, ]

mydata

#PART 1

#Question 1

color_mean <- mydata %>%
  group_by(Color) %>%
  summarize(mean_by_color=sum(Sales)/sum(Units))

color_mean


#Question 2

region_mean <- mydata %>%
  group_by(Region) %>%
  summarize(mean_by_region=sum(Sales)/sum(Units))

region_mean


#Question 3

max_values_date <- mydata %>%
  group_by(Date)%>%
  summarize(max_date=max(Sales))

head(max_values_date[order(-max_values_date$max_date),],5)

max_values_region <- mydata %>%
  group_by(Region)%>%
  summarize(max_region=max(Sales))

max_values_region


max_values_color <- mydata %>%
  group_by(Color)%>%
  summarize(max_colore=max(Sales))

max_values_color


#Question 4
 
datetime<-dmy(mydata$Date)
datetime

newdata <- mutate(mydata,
                        "year"=year(datetime),
                        "month"=month(datetime,label=TRUE),
                        "day"=day(datetime))

newdata

newdata2<-newdata%>%
  group_by(month)%>%
  summarize(mean_sales=mean(Sales))


head(newdata2[order(-newdata2$mean_sales),],1)



#Question 5

year<-select(newdata,year,Sales)%>%
  group_by(year) %>%
  summarize(year_sales=mean(Sales))


ggplot(data=year)+
  geom_point(mapping = aes(x=year_sales,y=year))
