library(ISLR)

library(caTools)

library(class)

library(ggplot2)

**DATA**

print(head(iris))

![1](https://user-images.githubusercontent.com/97744709/212979493-1918b58d-34fd-4725-9e33-c767dd99f475.jpg)

print(str(iris))

![2](https://user-images.githubusercontent.com/97744709/212979714-9e93bc36-ad9e-4253-a8af-bd9afebbd578.jpg)

**STANDARDIZE DATA**

**#Using scale() to standardize the feature columns of the iris dataset**

standardized.iris <- scale(iris[1:4])

**#Checking if the scaling worked by checking the variance of one of the new columns**

print(var(standardized.iris[,1]))

![3](https://user-images.githubusercontent.com/97744709/212980327-6f88e707-c14c-402c-9d6d-cd206cd2ff0d.jpg)

**#Joining the standardized data with the target 'Species' column**

final.data <- cbind(standardized.iris, iris[5])

print(final.data)

![4](https://user-images.githubusercontent.com/97744709/212981560-356024db-aa24-4015-aae7-678a4a3f2130.jpg)

**TRAIN AND TEST SPLITS**

set.seed(101)

sample <- sample.split(final.data$Species, SplitRatio = 0.7)

train.data <- subset(final.data, sample == TRUE)

test.data <- subset(final.data, sample == FALSE)

**BUILDING A KNN MODEL**

predicted.species <-
  knn(train.data[1:4], test.data[1:4], train.data$Species, k = 1)

print(predicted.species)

![5](https://user-images.githubusercontent.com/97744709/212982097-5abbb4c4-cb50-428f-84fc-9471bffdfbfc.jpg)

**#Misclassification rate**

misclass.error <- mean(predicted.species != test.data$Species)

print(misclass.error)

![6](https://user-images.githubusercontent.com/97744709/212982357-8a5b8124-a494-4566-9253-c044a7a61682.jpg)

**CHOOSING A KNN VALUE**

**#Creating a plot of the error (misclassification) rate for k values ranging from 1 to 10**

predicted.species <- NULL

error.rate <- NULL

    for (i in 1:10) {
      set.seed(101)
      predicted.species <- knn(train.data[1:4], test.data[1:4], train.data$Species, k = i)
      error.rate[i] <- mean(predicted.species != test.data$Species)
    }

k.values <- 1:10

error.df <- data.frame(error.rate, k.values)

pl <-
  ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point() + geom_line(lty =
                                                                                   'dotted',
                                                                                 color = 'red',
                                                                                 size = 1)

print(pl)

![7](https://user-images.githubusercontent.com/97744709/212982931-b965e497-3be6-4898-a4aa-799248a30770.jpg)

_#The error drops to its lowest for k values between 2-6._
