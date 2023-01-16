library(ggplot2)

library(corrplot)

library(ggthemes)

library(dplyr)

library(caTools)

**DATA**

bike <- read.csv('bikeshare.csv')

print(head(bike))

![1](https://user-images.githubusercontent.com/97744709/212746134-19160522-3052-48bb-aca2-738adc58dbe8.jpg)

**EXPLORATORY DATA ANALYSIS**

**#Scatter plot of count vs temp**

pl <- ggplot(bike, aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.3)

print(pl)

![2](https://user-images.githubusercontent.com/97744709/212746594-149f5308-0286-442a-9d3a-a8d899aa558c.jpg)

**#Scatter plot of count vs datetime**

bike$datetime <- as.POSIXct(bike$datetime)

pl <- ggplot(bike, aes(x=datetime,y=count)) + geom_point(aes(color=temp), alpha=0.4)

pl <- pl + scale_color_continuous(low="red",high="blue")

print(pl)

![3](https://user-images.githubusercontent.com/97744709/212747141-9738e69e-33c6-4753-899a-340799dbf8dd.jpg)

_#There is a seasonality to the data, for winter and summer. Also bike rental counts are increasing in general._

**#Correlation between temp and count**

print(cor(bike[,c('temp','count')]))

![4](https://user-images.githubusercontent.com/97744709/212747913-fa8c3065-98c2-4368-945a-1e8996dee7a6.jpg)

**#Exploring the season data**

pl1 <- ggplot(bike, aes(x=factor(season),y=count)) + geom_boxplot(aes(color=factor(season))) + theme_bw()

print(pl1)

![5](https://user-images.githubusercontent.com/97744709/212748554-a8be4d27-d247-4a2c-b55e-4ac673bb8fdc.jpg)

_#There are more rentals in winter than in spring_

**#FEATURE ENGINEERING**

**#Creating an 'hour' column that takes the hour from the datetime column**

bike$hour <- format(bike$datetime, "%H")

bike$hour <- sapply(bike$hour, as.numeric)

print(head(bike))

![6](https://user-images.githubusercontent.com/97744709/212749066-f7cc5180-5670-4a34-bfac-34fb338f4403.jpg)

**#Creating a scatterplot of count versus hour, with color scale based on temp. Only using bike data where workingday==1**

pl2 <- ggplot(filter(bike, workingday==1), aes(x=hour,y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp))

pl2 <- pl2 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange'))

print(pl2)

![7](https://user-images.githubusercontent.com/97744709/212750393-f13a63da-f7e1-4be0-91f3-a9f9ee307dc5.jpg)

**#Creating a same plot for non working days**

pl3 <- ggplot(filter(bike, workingday==0), aes(x=hour,y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp))

pl3 <- pl3 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange'))

print(pl3)

![8](https://user-images.githubusercontent.com/97744709/212750727-38fb4159-8499-4115-8877-48e4d7c3ac79.jpg)

_#Working days have peak activity during the morning (~8am) and right after work gets out (~5pm), with some lunchtime activity. While the non-work days have a steady rise and fall for the afternoon_

**#BUILDING THE MODEL**

**#Building a model that predicts count based solely on the temp feature**

temp.model <- lm(count~temp,bike)

print(summary(temp.model))

![9](https://user-images.githubusercontent.com/97744709/212751262-453d1fcd-dcbb-49fa-b966-499219033a95.jpg)

**#Predicting bike rentals if the temperature was 25 degrees Celsius**

temp.test <- data.frame(temp=c(25))

temp.25.prediction <- predict(temp.model,temp.test)

print(temp.25.prediction)

![10](https://user-images.githubusercontent.com/97744709/212751604-65ca86da-15e2-486b-818e-c03c8d2e0cf7.jpg)


model <- lm(count ~ .-datetime-atemp-casual-registered,bike)

# print(summary(model))
