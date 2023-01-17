_#We will use LendingClub.com's lending data from 2007-2010 and try to classify and predict whether or not the borrower paid back their loan in full_

library(ggplot2)

library(caTools)

library(e1071)

**DATA**

loans <- read.csv('loan_data.csv')

loans <- as.data.frame(loans)

print(head(loans))

![1](https://user-images.githubusercontent.com/97744709/213004971-1eb92275-fe02-4105-a178-758f96dcb1cf.jpg)

print(summary(loans))

![2](https://user-images.githubusercontent.com/97744709/213005491-2e92287c-f4c3-4944-bf64-8913e3b3e087.jpg)

print(str(loans))

![3](https://user-images.githubusercontent.com/97744709/213005822-6ba69ec2-f22b-4000-80d9-be5c847d5d20.jpg)

**#Converting the following columns to categorical data using factor() - inq.last.6mths, delinq.2yrs, pub.rec, not.fully.paid, credit.policy**

loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)

loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)

loans$pub.rec <- as.factor(loans$pub.rec)

loans$not.fully.paid <- as.factor(loans$not.fully.paid)

loans$credit.policy <- as.factor(loans$credit.policy)

**EXPLORATORY DATA ANALYSIS**

**#Histogram of fico scores colored by not.fully.paid**

pl <-
  ggplot(data = loans, aes(x = fico)) + geom_histogram(
    aes(fill = not.fully.paid),
    color = 'black',
    bins = 40,
    position = position_stack(reverse = TRUE)
  )

print(pl)

![4](https://user-images.githubusercontent.com/97744709/213007083-7cc55d4a-3ab4-4c1f-85c1-c1b7d183e4dc.jpg)

**#Barplot of purpose counts, colored by not.fully.paid**

pl1 <-
  ggplot(data = loans, aes(x = factor(purpose))) + geom_bar(aes(fill = not.fully.paid),
                                                            color = 'black',
                                                            position = 'dodge')

print(pl1)

![5](https://user-images.githubusercontent.com/97744709/213007709-04f3b29b-4a1a-4db1-8b3b-138c06e72618.jpg)

**#Scatterplot of fico score versus int.rate**

pl2 <-
  ggplot(data = loans, aes(x = int.rate, y = fico)) + geom_point()

print(pl2)

![6](https://user-images.githubusercontent.com/97744709/213009469-2fea7435-3f2a-4f8f-85ac-3bccb3517242.jpg)

pl3 <-
  ggplot(data = loans, aes(x = int.rate, y = fico)) + geom_point(aes(color =
                                                                       not.fully.paid), alpha =
                                                                   0.5)

print(pl3)

![7](https://user-images.githubusercontent.com/97744709/213011138-ddc02b72-5667-4ce0-9738-54e2d5287647.jpg)

**BUILDING THE MODEL**

**#Splitting data into training and test sets**

set.seed(101)

sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train <- subset(loans, sample == TRUE)

test <- subset(loans, sample == FALSE)

**#Using the svm() function to train a model on training set**

model <- svm(not.fully.paid ~ ., train)

print(summary(model))

![8](https://user-images.githubusercontent.com/97744709/213011840-209586cd-6862-4168-9520-db9836c614eb.jpg)

**#Using predict() to predict new values from the test set using our model**

pred.values <- predict(model, test[1:13])

print(table(pred.values,test$not.fully.paid))

![9](https://user-images.githubusercontent.com/97744709/213012135-b05c1f72-cc97-4f52-be07-a09a59cbd9ff.jpg)

**TUNING THE MODEL**

**#Using the tune() function to test out different cost and gamma values**

tuned.results <-
  tune(
    svm,
    train.x = not.fully.paid ~ .,
    data = train,
    kernel = 'radial',
    ranges = list(cost = c(100, 200), gamma = c(0.1))
  )

print(summary(tuned.results))

![10](https://user-images.githubusercontent.com/97744709/213013571-4a8fe440-e91c-4914-8350-4a8a0f4ef91b.jpg)

tuned.model <- svm(not.fully.paid ~ .,
                   data = train,
                   cost = 100,
                   gamma = 0.1)

tuned.predictions <- predict(tuned.model, test[1:13])

print(table(tuned.predictions, test$not.fully.paid))

![11](https://user-images.githubusercontent.com/97744709/213015443-53447555-6762-4870-95e4-23d0ec3aa31a.jpg)
