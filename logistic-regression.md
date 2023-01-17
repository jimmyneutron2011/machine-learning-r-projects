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

**#Grouping these countries by continent**

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

**#education**

print(table(adult$education))

![11](https://user-images.githubusercontent.com/97744709/212909288-91918f18-0e1a-49b5-9074-3c5e51154677.jpg)

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

print(table(adult$education))

![12](https://user-images.githubusercontent.com/97744709/212911370-cd529d89-8693-45d8-bcd8-80eb660538af.jpg)

**#occupation**

print(table(adult$occupation))

![13](https://user-images.githubusercontent.com/97744709/212912077-1edc8e6e-0add-48f5-bbea-7bfc96818601.jpg)

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

print(table(adult$occupation))

![14](https://user-images.githubusercontent.com/97744709/212913054-2947fa60-51a2-45eb-935f-deeefc5d00d3.jpg)

print(str(adult))

![15](https://user-images.githubusercontent.com/97744709/212913580-8bfa87b7-9837-4971-beae-a507c40d3817.jpg)

**#Making sure the columns have factor levels**

adult$type_employer <- as.factor(adult$type_employer)

adult$marital <- as.factor(adult$marital)

adult$country <- as.factor(adult$country)

adult$occupation <- as.factor(adult$occupation)

adult$relationship <- as.factor(adult$relationship)

adult$race <- as.factor(adult$race)

adult$sex <- as.factor(adult$sex)

adult$income <- as.factor(adult$income)

adult$education <- as.factor(adult$education)

**MISSING DATA**

**#Convert any cell with a '?' value to a NA value**

adult[adult == '?'] <- NA

**#Missmap function**

print(missmap(adult))

![16](https://user-images.githubusercontent.com/97744709/212917584-b6b53f31-7d4d-4935-a811-988cd7a31dc2.jpg)

_#The missmap function from the Amelia package is bascially a heatmap pointing out missing values (NA). This gives us a quick glance at how much data is missing._

**#Getting rid of y labels**

mmap <- missmap(adult, y.at=c(1), y.labels=c(''), col=c('yellow','black'))

print(mmap)

![17](https://user-images.githubusercontent.com/97744709/212918212-2917e9ce-9196-49ef-ba51-d54fbcab7926.jpg)

**#Using na.omit() to omit NA data from the adult data frame**

adult <- na.omit(adult)

mmap <-
  missmap(
    adult,
    y.at = c(1),
    y.labels = c(''),
    col = c('yellow', 'black')
  )

print(mmap)

![18](https://user-images.githubusercontent.com/97744709/212919085-e0334f9e-4d00-4d11-9ec5-168afbad6302.jpg)

**EXPLORATORY DATA ANALYSIS**

print(str(adult))

![19](https://user-images.githubusercontent.com/97744709/212920048-a659bf63-aa6f-45aa-96c9-0f00252faf04.jpg)

**#Histogram of ages, colored by income**

pl <-
  ggplot(adult, aes(x = age)) + geom_histogram(
    aes(fill = income),
    color = 'black',
    binwidth = 1,
    position = position_stack(reverse = TRUE)
  ) + theme_bw()

print(pl)

![20](https://user-images.githubusercontent.com/97744709/212920778-3df51ed3-d4ba-4a49-9c3e-197ab2160ae7.jpg)

**#Histogram of hours worked per week**

pl1 <-
  ggplot(adult, aes(x = hr_per_week)) + geom_histogram() + theme_bw()

print(pl1)

![21](https://user-images.githubusercontent.com/97744709/212921735-b547c3fe-859b-45b7-b9e2-bf04dc32ec68.jpg)

**#Renaming the country column to region column to better reflect the factor levels**

adult <- rename(adult, region = country)

**#Barplot of region with the fill color defined by income class**

pl2 <-
  ggplot(adult, aes(x = region)) + geom_bar(aes(fill = income)) + theme_bw()

print(pl2)

![22](https://user-images.githubusercontent.com/97744709/212922813-ead91fd6-0520-4b50-9102-161373116e29.jpg)

**BUILDING A MODEL**

_#Building a model to classify people into two groups: Above or Below 50k in Salary_

print(head(adult))

![23](https://user-images.githubusercontent.com/97744709/212923971-cfa35812-ed8d-45be-8f48-32afb8fa3b58.jpg)

**#Train Test split**

set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.7)

train <- subset(adult, sample == TRUE)
test <- subset(adult, sample == FALSE)

**#Training the model**

model <- glm(income ~ ., family=binomial(link='logit'), data=train)

print(summary(model))

![24](https://user-images.githubusercontent.com/97744709/212927123-0b406c59-da3b-4f23-9281-c127bbd29dda.jpg)

**#Using the step() function to create a new model**

new.step.model <- step(model)

print(new.step.model)

![25](https://user-images.githubusercontent.com/97744709/212928495-196ca257-c2f7-4150-a937-815542abe9b3.jpg)

_#The step() function iteratively tries to remove predictor variables from the model in an attempt to delete variables that do not significantly add to the fit. It uses Akaike information criterion (AIC) to do this._

print(summary(new.step.model))

![26](https://user-images.githubusercontent.com/97744709/212930956-7ca4c98f-3cf5-4387-a798-607baca754b7.jpg)
![27](https://user-images.githubusercontent.com/97744709/212930755-6ec6ebfd-6f7c-447a-836c-4ce37ee04470.jpg)

**#Creating a confusion matrix using the predict function with type='response' as an argument inside of that function**

test$predicted.income <- predict(model, newdata=test, type='response')

print(table(test$income, test$predicted.income > 0.5))

![28](https://user-images.githubusercontent.com/97744709/212932154-476568ad-9322-46ae-8f1f-22662c2c4370.jpg)

**#Calculating accuracy of our model**

acc <- (6373 + 1415) / (6373 + 547 + 880 + 1415)

print(acc)

![29](https://user-images.githubusercontent.com/97744709/212932948-6e001c00-2179-4c79-a9ce-73b80ceae2eb.jpg)

**#Calculating recall**

rec <- 6373 / (6373 + 547)

print(rec)

![30](https://user-images.githubusercontent.com/97744709/212933416-74583755-c7b0-446a-bf0d-9333d4a8c5ad.jpg)

**#Calculating precision**

prec <- 6373 / (6373 + 880)

print(prec)

![31](https://user-images.githubusercontent.com/97744709/212933866-dba7f241-7678-442a-856d-15443d3945d3.jpg)
