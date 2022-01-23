library(dplyr)
library(tidyverse)
data("starwars")

#First Question :
count(unique(starwars %>% select(homeworld)))


#Second Question : ??
mostmovie<-max(lengths(starwars$films))
filter(starwars,lengths(films)== mostmovie)


#Third Question :
massandheight<-starwars %>%
  group_by(species) %>%
  summarize(avg_mass = mean(mass, na.rm=TRUE),avg_height=mean(height,na.rm = TRUE))

massandheight

#Istege olarak ayri islemlere ve degiskenlere de tabi tutulabilir.
'
mass<-starwars %>%
  group_by(species) %>%
  summarize(avg_mass = mean(mass, na.rm=TRUE))
            
height<-starwars %>%
  group_by(species) %>%
  summarize(avg_height=mean(height,na.rm = TRUE))
height
'


#Fourth Quesiton :
starwars<-add_row(starwars,name='Polen Yýlmazay',height=155,mass=40,
                  hair_color='brown',skin_color='wheat',
                  eye_color='brown',birth_year=97.0,sex='female',
                  gender='feminine',homeworld='Tatooine',species='Human',
                  films=list(c('A New Hope','Revenge of the Sith')))

newdata<-starwars
newdata


#Fifth Question : 
starwars_2<-select(starwars,name,mass,height,
                 species,hair_color,skin_color,
                 eye_color,sex,gender)

starwars_2

summarize(starwars_2,
          BMI=mass/((height/100)^2))



#Sixth Question : 
starwars_2<-group_by(starwars_2,
                     BMI=mass/(height*height/10000))

starwars_3<-starwars_2%>%
  mutate(categories=cut(BMI,breaks = c(0,18.50,24.99,29.99,Inf),labels = c("underweight","healthy","overweight","obese")))%>%
  select(species,BMI,categories)%>%
  group_by(species,categories)%>%
  count(categories)

starwars_3


#Bu sekilde de hepsi ayri bir atrribute içine atanipta species sayimi yaptirabiliriz.Sadece bu biraz daha kod kalabalikligina ve ekstra yer tutumuna neden oluyor.
'underweight<-filter(starwars_2,BMI<18.5)
underweight
healthy<-filter(starwars_2,BMI>18.5 & BMI<24.99)
healthy
overweight<-filter(starwars_2,BMI>25.0 & BMI<29.99)
overweight
obese<-filter(starwars_2,BMI>30)

underweight %>%
   group_by(species)%>%
   count(species)

healthy %>%
   group_by(species)%>%
   count(species)

overweight %>%
   group_by(species)%>%
   count(species)

obese %>%
   group_by(species)%>%
   count(species)'


#Seventh Question :
ggplot(data=starwars_2,mapping=aes(x=gender,y=sex,color=BMI)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)
  #geom_point(mapping=aes(x=gender,y=sex,color=BMI))










