_#We'll use UCI's wine dataset to to see how well we can cluster the wine into groups_

library(dplyr)

library(ggplot2)

library(ggthemes)

**DATA**

df1 <- read.csv('winequality-red.csv', sep = ';')

print(head(df1))

![1](https://user-images.githubusercontent.com/97744709/213018359-a4220c22-9692-42c6-89a7-f1ce8a5cded2.jpg)

df2 <- read.csv('winequality-white.csv', sep = ';')

print(head(df2))

![2](https://user-images.githubusercontent.com/97744709/213018609-78f8c691-1cfb-4e72-9a79-6e8176641b0a.jpg)

**#Adding a label column to both df1 and df2 indicating a label 'red' or 'white'**

df1$label <- c('red')

df2$label <- c('white')

**#Combining df1 and df2 into a single data frame called wine**

wine <- bind_rows(df1, df2)

print(str(wine))

![3](https://user-images.githubusercontent.com/97744709/213019095-780ce370-2c09-443b-a469-cb95c1bc0262.jpg)

**EXPLORATORY DATA ANALYSIS**

**#Histogram of residual sugar, colored by label**

pl <-
  ggplot(wine, aes(x = residual.sugar)) + geom_histogram(
    aes(fill = label),
    color =
      'black',
    bins = 50,
    position = position_stack(reverse = T)
  ) + scale_fill_manual(values = c('red', 'white'))

print(pl)

![4](https://user-images.githubusercontent.com/97744709/213019756-d3fd5673-e1ec-4299-bde0-b7445aee3267.jpg)

**#Histogram of citric.acid, colored by label**

pl1 <-
  ggplot(wine, aes(x = citric.acid)) + geom_histogram(
    aes(fill = label),
    color = 'black',
    bins = 50,
    position = position_stack(reverse = T)
  ) + scale_fill_manual(values = c('red', 'white'))

print(pl1)

![5](https://user-images.githubusercontent.com/97744709/213020177-9c865d72-36ec-4002-acdd-91659e48de72.jpg)

**#Histogram of alcohol, colored by label**

pl2 <-
  ggplot(wine, aes(x = alcohol)) + geom_histogram(
    aes(fill = label),
    color = 'black',
    bins = 50,
    position = position_stack(reverse = T)
  ) + scale_fill_manual(values = c('red', 'white'))

print(pl2)

![6](https://user-images.githubusercontent.com/97744709/213020499-783709f2-b42c-4d3c-8aaf-8ae57c1e8550.jpg)

**#Scatterplot of residual.sugar versus citric.acid, colored by label**

pl3 <-
  ggplot(wine,
         aes(x = citric.acid,
             y = residual.sugar)) + geom_point(aes(color = label), alpha =
                                                 0.2) + scale_color_manual(values = c('red', 'white')) + theme_dark()

print(pl3)

![7](https://user-images.githubusercontent.com/97744709/213021110-19d74637-698d-4d39-ad20-b8f363c70f82.jpg)

**#Scatterplot of volatile.acidity versus residual.sugar, colored by label**

pl4 <-
  ggplot(wine, aes(x = volatile.acidity, y = residual.sugar)) + geom_point(aes(color =
                                                                                 label), alpha = 0.2) + scale_color_manual(values = c('red', 'white')) + theme_dark()

print(pl4)

![8](https://user-images.githubusercontent.com/97744709/213021476-d8260757-4e6d-4413-9450-6df2b80b2b63.jpg)

**#Grabbing the wine data without the label**

clus.data <- select(wine, -label)

print(head(clus.data))

![9](https://user-images.githubusercontent.com/97744709/213021797-9323bdb4-4a04-4fc0-bb27-fc624a52a641.jpg)

**BUILDING THE CLUSTERS**

**#Calling the kmeans function on clus.data**

wine.cluster <- kmeans(clus.data, 2)

print(wine.cluster$centers)

![10](https://user-images.githubusercontent.com/97744709/213022216-2102fc0c-184e-4d69-8e0d-713106245296.jpg)

**EVALUATING THE CLUSTERS**

**#Using the table() function to compare cluster results to the real results**

print(table(wine$label, wine.cluster$cluster))

![11](https://user-images.githubusercontent.com/97744709/213022536-a956bad4-b90d-4aeb-867f-ebc330982f23.jpg)

_#Red is easier to cluster together. There seems to be a lot of noise with white wines, this could also be due to "Rose" wines being categorized as white wine, while still retaining the qualities of a red wine. Overall this makes sense since wine is essentially just fermented grape juice and the chemical measurements we were provided may not correlate well with whether or not the wine is red or white. K-Means can only give us the clusters, it can't directly tell what the labels should be, or even how many clusters we should have, we are just lucky to know we expected two types of wine._
