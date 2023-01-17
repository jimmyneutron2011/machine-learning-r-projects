_#We will be exploring the use of tree methods to classify schools as Private or Public based off their features_

library(ISLR)

library(ggplot2)

library(caTools)

**DATA**

df <- College

print(head(df))

![1](https://user-images.githubusercontent.com/97744709/212985834-c855d924-32d4-4fcf-b4bb-fa5e54ed7368.jpg)

**EXPLORATORY DATA ANALYSIS**

**#Scatterplot of Grad.Rate versus Room.Board, colored by the Private column**

pl <- ggplot(df, aes(x=Room.Board, y=Grad.Rate)) + geom_point(aes(color=Private))

print(pl)

![2](https://user-images.githubusercontent.com/97744709/212986286-5f805d4d-61fc-4b2b-93aa-4d6cd286e03c.jpg)

**#Histogram of full time undergrad students, colored by Private**

pl1 <- ggplot(df, aes(x=F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50, position=position_stack(reverse=TRUE))

print(pl1)

![3](https://user-images.githubusercontent.com/97744709/212986945-a2b41e32-23f3-4c37-bf43-7738b7288421.jpg)

**#Histogram of Grad.Rate colored by Private**

pl2 <- ggplot(df, aes(x=Grad.Rate)) + geom_histogram(aes(fill=Private),color='black', bins=50, position=position_stack(reverse=TRUE))

print(pl2)

![4](https://user-images.githubusercontent.com/97744709/212987571-2d852801-b2ed-4650-b6bc-10800ca2060f.jpg)

_#We find that a college has a graduation rate above 100%. This is an error._

**#College whose Graduation Rate is above 100%**

print(subset(df, Grad.Rate > 100))

![5](https://user-images.githubusercontent.com/97744709/212988977-efbb6a94-3255-4819-b592-dfd1bf2baae8.jpg)

**#Changing Cazenovia College's grad rate to 100%**

df['Cazenovia College','Grad.Rate'] <- 100

print(df['Cazenovia College',])

![6](https://user-images.githubusercontent.com/97744709/212989428-974061f5-5af6-4753-93ed-3574cb188f01.jpg)

**#Histogram of the corrected Grad.Rate colored by Private**

pl2 <- ggplot(df, aes(x=Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50, position = position_stack(reverse=TRUE))

print(pl2)

![7](https://user-images.githubusercontent.com/97744709/212990372-37decb10-1836-497e-89b0-1633ef9fa16c.jpg)

**TRAIN TEST SPLIT**

set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.7)

train <- subset(df, sample==TRUE)

test <- subset(df, sample==FALSE)

**DECISION TREE**

**#Using the rpart library to build a decision tree to predict whether or not a school is Private**

library(rpart)

tree <- rpart(Private~., method='class', data=train)

**#Using predict() to predict the Private label on the test data**

tree.predictions <- predict(tree,test)

print(head(tree.predictions))

![8](https://user-images.githubusercontent.com/97744709/212991429-5371d5e4-34cc-493a-b26e-8ad7acea4fc2.jpg)

_#We have two columns with the probabilities_

**#Turning these two columns into one column to match the original Yes/No Label for a Private column**

tree.predictions <- as.data.frame(tree.predictions)

    joiner <- function(x){
      if (x>=0.5){
        return("Yes")
      }else{
        return("No")
      }
    }

tree.predictions$Private <- sapply(tree.predictions$Yes, joiner)

print(head(tree.predictions))

![9](https://user-images.githubusercontent.com/97744709/212992448-dacd9e21-59fa-4a1b-8b9a-d28cdab803e5.jpg)

**#Using table() to create a confusion matrix of our tree model**

print(table(tree.predictions$Private, test$Private))

![10](https://user-images.githubusercontent.com/97744709/212992788-763047d3-ea39-4062-a9a4-6af70cb42adb.jpg)

**#Using the rpart.plot library and the prp() function to plot out our tree model**

library(rpart.plot)

print(prp(tree))

![11](https://user-images.githubusercontent.com/97744709/212993191-43631c0a-97a2-426b-bb87-1692b9efc09d.jpg)

**RANDOM FOREST**

library(randomForest)

**#Using randomForest() to build out a model to predict Private class**

rf.model <- randomForest(Private~., data=train, importance=TRUE)

**#Model's confusion matrix on its own training set**

print(rf.model$confusion)

![12](https://user-images.githubusercontent.com/97744709/212993833-1dcb2526-d314-4cb5-97ca-218384eec720.jpg)

**#Feature importance**

print(rf.model$importance)

![13](https://user-images.githubusercontent.com/97744709/212994144-5abcfd5e-a8b5-4f03-b590-e463b39c43f0.jpg)

**#Using random forest model to predict on our test set**

rf.predictions <- predict(rf.model,test)

print(table(rf.predictions,test$Private))

![14](https://user-images.githubusercontent.com/97744709/212994523-fbf2889f-b222-4f56-8a63-8d9cc8056859.jpg)
