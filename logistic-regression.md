**_#We will be working with the UCI adult dataset to predict if people in the data set belong in a certain class by salary, either making <=50k or >50k per year._**

library(dplyr)

library(Amelia)

library(ggplot2)

library(ggthemes)

library(caTools)

**DATA**

adult <- read.csv('adult_sal.csv')

print(head(adult))

![1](https://user-images.githubusercontent.com/97744709/212877105-d5b85190-97a8-4fc9-b72d-0f7d3983f58a.jpg)

**#Dropping the repeated index**

adult <- select(adult,-X)

print(head(adult))

![2](https://user-images.githubusercontent.com/97744709/212877500-93a56104-4baf-4403-8e50-7c7371b47d87.jpg)

print(str(adult))

![3](https://user-images.githubusercontent.com/97744709/212877909-fc73fb03-a2e8-44f8-b386-881ee94227f7.jpg)

print(summary(adult))

![4](https://user-images.githubusercontent.com/97744709/212878221-0a5eca14-1e5e-4d0d-aecc-e49451ab48f4.jpg)

**DATA CLEANING**

**#REDUCING THE NUMBER OF FACTORS OF:**

**#type_employer column**

print(table(adult$type_employer))

![5](https://user-images.githubusercontent.com/97744709/212878966-d65ca887-29e3-4950-87ef-b309b4fcbb16.jpg)

    group_emp <- function(job) {
  
    job <- as.character(job)
  
    if (job == 'Never-worked' | job == 'Without-pay') {
    
      return('Unemployed')
  
    } else if (job == 'Local-gov' | job == 'State-gov') {
    
      return('SL-gov')
  
    } else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc') {
    
      return('self-emp')
  
    } else{
    
      return(job)
  
    }

    }

adult$type_employer <- sapply(adult$type_employer, group_emp)

print(table(adult$type_employer))

![6](https://user-images.githubusercontent.com/97744709/212879314-a94f0346-3f61-4360-be4f-449f70fe53f3.jpg)

**#marital column**

print(table(adult$marital))

![7](https://user-images.githubusercontent.com/97744709/212882642-65a0bef0-947b-47a7-80b1-7875d4d6c245.jpg)

    group_marital <- function(status) {
  
    status <- as.character(status)
  
    if (status == 'Divorced' |
      
        status == 'Separated' | status == 'Widowed') {
    
      return('Not-Married')
  
    } else if (status == 'Married-AF-spouse' |
             
              status == 'Married-civ-spouse' |
             
              status == 'Married-spouse-absent') {
    
      return('Married')
  
    } else{
    
      return('Never-Married')
  
    }

    }

adult$marital <- sapply(adult$marital, group_marital)

print(table(adult$marital))

![8](https://user-images.githubusercontent.com/97744709/212883500-d09d2a53-d2c0-4726-a4a6-d8e502bbc15f.jpg)

**#country column**

print(table(adult$country))

![9](https://user-images.githubusercontent.com/97744709/212883863-5dfd9227-94c1-481e-a0e7-6e6ea08015b3.jpg)

#Grouping these countries by continent

Asia <- c(
  'China',
  'Hong',
  'India',
  'Iran',
  'Cambodia',
  'Japan',
  'Laos' ,
  'Philippines' ,
  'Vietnam' ,
  'Taiwan',
  'Thailand'
)

North.America <- c('Canada', 'United-States', 'Puerto-Rico')

Europe <-
  c(
    'England' ,
    'France',
    'Germany' ,
    'Greece',
    'Holand-Netherlands',
    'Hungary',
    'Ireland',
    'Italy',
    'Poland',
    'Portugal',
    'Scotland',
    'Yugoslavia'
  )

Latin.and.South.America <-
  c(
    'Columbia',
    'Cuba',
    'Dominican-Republic',
    'Ecuador',
    'El-Salvador',
    'Guatemala',
    'Haiti',
    'Honduras',
    'Mexico',
    'Nicaragua',
    'Outlying-US(Guam-USVI-etc)',
    'Peru',
    'Jamaica',
    'Trinadad&Tobago'
  )
  
Other <- c('South')

    group_country <- function(nation) {
  
    nation <- as.character(nation)
  
    if (nation %in% Asia) {
    
      return('Asia')
  
    } else if (nation %in% North.America) {
    
      return('North.America')
  
    } else if (nation %in% Europe) {
    
      return('Europe')
  
    } else if (nation %in% Latin.and.South.America) {
    
      return('Latin.and.South.America')
  
    } else{
    
      return('Other')
  
    }

    }


adult$country <- sapply(adult$country, group_country)

print(table(adult$country))

![10](https://user-images.githubusercontent.com/97744709/212885738-e9844b5d-cefd-4f7b-a463-e45acd899d46.jpg)

print(str(adult))

# print(table(adult$education))

group_edu <- function(edu) {
  edu <- as.character(edu)
  if (edu == 'Preschool' | edu == '1st-4th' | edu == '5th-6th') {
    return('Primary')
  } else if (edu == '7th-8th' |
             edu == '9th' | edu == '10th' | edu == 'HS-grad') {
    return('Secondary')
  } else if (edu == '11th' | edu == '12th') {
    return('Junior.College')
  } else if (edu == 'Assoc-acdm' | edu == 'Assoc-voc') {
    return('Associate')
  } else if (edu == 'Bachelors' |
             edu == 'Prof-school' | edu == 'Some-college') {
    return('Bachelors')
  } else{
    return(edu)
  }
}

adult$education <- sapply(adult$education, group_edu)

# print(table(adult$education))

group_occu <- function(occu) {
  occu <- as.character(occu)
  if (occu == 'Craft-repair' |
      occu == 'Handlers-cleaners' |
      occu == 'Machine-op-inspct' |
      occu == 'Other-service' |
      occu == 'Priv-house-serv' | occu == 'Transport-moving') {
    return('Services')
  } else if (occu == 'Adm-clerical' |
             occu == 'Exec-managerial' | occu == 'Sales' |
             occu == 'Tech-support') {
    return('Office worker')
  } else{
    return(occu)
  }
}

adult$occupation <- sapply(adult$occupation, group_occu)

# print(table(adult$occupation))

# print(str(adult))

adult[adult == '?'] <- NA

adult$type_employer <- as.factor(adult$type_employer)
adult$marital <- as.factor(adult$marital)
adult$country <- as.factor(adult$country)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$income <- as.factor(adult$income)
adult$education <- as.factor(adult$education)

# mmap <- missmap(adult, y.at=c(1), y.labels=c(''), col=c('yellow','black'))

# print(mmap)

adult <- na.omit(adult)

mmap <-
  missmap(
    adult,
    y.at = c(1),
    y.labels = c(''),
    col = c('yellow', 'black')
  )

# print(mmap)

# print(str(adult))

pl <-
  ggplot(adult, aes(x = age)) + geom_histogram(
    aes(fill = income),
    color = 'black',
    binwidth = 1,
    position = position_stack(reverse = TRUE)
  ) + theme_bw()

# print(pl)

pl1 <-
  ggplot(adult, aes(x = hr_per_week)) + geom_histogram() + theme_bw()

# print(pl1)

adult <- rename(adult, region = country)

# print(head(adult))

pl2 <-
  ggplot(adult, aes(x = region)) + geom_bar(aes(fill = income)) + theme_bw()

# print(pl2)

set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.7)

train <- subset(adult, sample == TRUE)
test <- subset(adult, sample == FALSE)

model <- glm(income ~ ., family=binomial(link='logit'), data=train)

# print(summary(model))

# new.step.model <- step(model)

# print(new.step.model)

# print(summary(new.step.model))

test$predicted.income <- predict(model, newdata=test, type='response')

#confusion matrix
# print(table(test$income, test$predicted.income > 0.5))

#accuracy
acc <- (6373 + 1415) / (6373 + 547 + 880 + 1415)

# print(acc)

#recall
rec <- 6373 / (6373 + 547)

# print(rec)

#precision
prec <- 6373 / (6373 + 880)

# print(prec)
