#Polen  YILMAZAY - 2016555070
library(tidyverse)

stringr::words

#Qeustion 1
data <- stringr::words

set.seed(2016555070)

mydata <- sample(data,size=100,replace = FALSE)

mydata


#Question 2

str_subset(mydata, "^a.+.e$")



#Question 3

numberofvowels<-tibble(word=mydata,
                       nvowels=str_count(mydata,"[aeiou]"))%>%
  arrange(desc(nvowels))


count(numberofvowels%>%filter(nvowels>3))



#Question 4

newdata <- tibble( 
  word = mydata,
  i = seq_along(word)
)

length<-str_length(mydata)
newdata<- mutate(newdata,length)

head(newdata[order(-newdata$length),],5)

#Question 5

mydata[str_detect(mydata,"age|any|day|exp|her|pro|the")]

