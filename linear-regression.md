library(ggplot2)

library(corrplot)

library(ggthemes)

library(dplyr)

library(caTools)

bike <- read.csv('bikeshare.csv')

print(head(bike))



pl <- ggplot(bike, aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.3)

bike$datetime <- as.POSIXct(bike$datetime)

pl <- ggplot(bike, aes(x=datetime,y=count)) + geom_point(aes(color=temp), alpha=0.4)

pl <- pl + scale_color_continuous(low="red",high="blue")
  
# print(pl)

# print(cor(bike[,c('temp','count')]))

pl1 <- ggplot(bike, aes(x=factor(season),y=count)) + geom_boxplot(aes(color=factor(season))) + theme_bw()

# print(pl1)

bike$hour <- format(bike$datetime, "%H")
bike$hour <- sapply(bike$hour, as.numeric)

# print(head(bike))

# print(head(working_day_1))

pl2 <- ggplot(filter(bike, workingday==1), aes(x=hour,y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp))

pl2 <- pl2 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange'))

# print(pl2)

pl3 <- ggplot(filter(bike, workingday==0), aes(x=hour,y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp))

pl3 <- pl3 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange'))

# print(pl3)

temp.model <- lm(count~temp,bike)

# print(summary(temp.model))

temp.test <- data.frame(temp=c(25))

temp.25.prediction <- predict(temp.model,temp.test)

# print(temp.25.prediction)

model <- lm(count ~ .-datetime-atemp-casual-registered,bike)

# print(summary(model))
