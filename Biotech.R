---
  title: "Team 2"
output: slidy_presentation
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Install packages
install.packages("DescTools")
install.packages("dplyr"")
install.packages("caret"")
install.packages("ggplot2")
install.packages("tree")
install.packages("randomForest")
install.packages("purrr")
install.packages("factoextra")
install.packages("readr")
install.packages("ggExtra")

## Calling necessary packages
library(DescTools)
library(dplyr)
library(caret)
library(ggplot2)
library(tree)
library(randomForest)
library(purrr)
library(factoextra)
library(readr)
library(ggExtra)

## Reading in the data file
memory.limit()
memory.limit(size=56000)

biotech = read.csv("Dataset.csv", na.strings = c("", " "), as.is =T)
str(biotech)
bio <- biotech

cols = c("Attrition","BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","OverTime","Over18")
bio[ ,cols] = lapply(bio[ ,cols],factor)
str(bio)

## Checking for missing values
PlotMiss(x= bio, main ="Variables with Missing (NA) values")

## Checking for and removing duplicates
biotech_select = bio
biotech_select[duplicated(biotech_select), ] 
biotech_select = biotech_select[!duplicated(biotech_select), ]
str(biotech_select)




## Subsetting the data for training/testing
set.seed(1)

select <- sample (1: nrow(biotech_select), .6*nrow(biotech_select))

train_biotech <- biotech_select[select, ]   # this is the training data we will use to develop the model
test_biotech <- biotech_select[-select, ]


## Logistic Regression Model
#Model Development
lr1 <- glm(Attrition ~ Department + BusinessTravel + EnvironmentSatisfaction + Age + DistanceFromHome + JobInvolvement + JobSatisfaction
                   + StockOptionLevel + RelationshipSatisfaction + YearsSinceLastPromotion+ WorkLifeBalance + RelationshipSatisfaction + OverTime + JobRole+ TotalWorkingYears +MonthlyIncome +MaritalStatus
           , data= train_biotech, family = binomial(link = "logit"))

summary(lr1)

#Model Validation
#Apply the logistic regression model lr_model1 on test_carmax data set and create predicted values of NPS Category (binary dependent variable) in testing data

# Create predicted values of NPS Category (binary dependent variable) in testing data: test_biotech
pr_lr_Testing <- predict(lr1, newdata= test_biotech, type= "response")

# Convert the predicted values of the dependent variable (test) to be re-coded as "1" if prob >0.5, or as "0" otherwise using ifelse() function
pr_lr_coded <- ifelse(pr_lr_Testing > 0.5, 1, 0) 

# Create the Confusion Matrix 
table(test_biotech$Attrition, pr_lr_coded) 


#What is the precision of the logistic regression model when applied to the dei_test dataset?
#Precision = Given the model predicted the individual Attrition = yes, how many times did Attrition actually equal yes?

7/(9+7)*100
#The precision of the logistic regression model when applied to the test_carmax data set is 50%


#What is the recall of the logistic regression model when applied to the test_carmax data set?
#Recall = Given Carmax earns NPS 10, how many times did the model predict it correctly? 

7/(90+7)*100
#The recall of the logistic regression model when applied to the test_carmax data set is 99.23%

## DATA VISUALIZATION
g <- ggplot(biotech, aes(Department))
g + geom_bar(aes(fill=Attrition), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

g <- ggplot(biotech, aes( Gender))
g + geom_bar(aes(fill=Attrition), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Attrition among gender") 

# Visualize categorical data:
pAttrition <- biotech_select %>% group_by(Attrition) %>% summarise(count = n()) %>% 
  mutate(Percent = (count / sum(count))*100) %>% 
  ggplot() + geom_bar(aes(y=Percent, x=Attrition, fill=Attrition), stat = "identity")
pDept <- biotech_select %>% ggplot(aes(Department, fill=Attrition)) + geom_bar()  + coord_flip()
pMarital <- biotech_select %>% ggplot(aes(MaritalStatus, fill=Attrition)) + geom_bar()
pGender <- biotech_select %>% ggplot(aes(Gender, fill=Attrition)) + geom_bar()
pEducationField <- biotech_select %>% ggplot(aes(EducationField, fill=Attrition)) + geom_bar()
pJobRole <- biotech_select %>% ggplot(aes(JobRole, fill=Attrition)) + geom_bar()
pAttrition
pDept
pMarital
pGender
pEducationField
pJobRole
grid.arrange(pAttrition, pDept, pMarital,pGender, ncol=2, nrow=2)

biotech_select %>% ggplot(aes(JobRole, fill=Attrition)) + geom_bar() + coord_flip() + coord_flip() +  labs(title="Attrition by Job Role", y="Employees")

biotech_select %>% ggplot(aes(EducationField)) + geom_bar(aes(fill=Attrition))  + coord_flip() + labs(title="Attrition by Educational Field", y="Employees")

g <- ggplot(biotech_select, aes(JobRole, MonthlyIncome))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       x="Education Field",
       y="Monthly Income")

ggplot(biotech_select, aes(x=YearsAtCompany, y=YearsSinceLastPromotion, color=Attrition, shape=Attrition)) +
  geom_point() + 
  geom_smooth(method=lm)

ggplot(biotech_select, aes(x=YearsAtCompany, y=YearsSinceLastPromotion, color=Attrition, shape=Attrition)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(biotech_select, aes(x=YearsAtCompany, y=YearsSinceLastPromotion, color=Attrition)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  facet_grid(cols =  vars(Attrition, scales = "free"))

 g <- ggplot(biotech_select, aes(x=YearsAtCompany, y=YearsSinceLastPromotion, color = OverTime)) +
  geom_point() + 
   geom_smooth(method = lm, se = FALSE)+
  labs(title = "Attrition",
       ,
       y = "Years Since Last Promotion", x = "Years At Company")
 
   g+  facet_grid(. ~ Attrition) 
   
   g <- ggplot(biotech_select, aes(x=YearsAtCompany, y=YearsSinceLastPromotion, color = OverTime)) +
     geom_point(size=2.5,alpha = 1,shape=16) + 
     geom_smooth(method = lm, se = FALSE)+
     scale_color_brewer(palette = "Accent")+
     theme_light()+
     theme(legend.position = "bottom")+
     labs(title = "Attrition",
          ,
          y = "Years Since Last Promotion", x = "Years At Company")
   
   g+  facet_grid(. ~ Attrition) 
   
 #Distance from home and work life balance  
   g2 <- ggplot(biotech_select, aes(OverTime, DistanceFromHome, color = Attrition))
   g2 + geom_boxplot(varwidth=T, fill="plum") + 
     labs(title="Box plot", 
          x="WorkLifeBalance",
          y="Distance From Home")
   g2 +  facet_grid(. ~ Attrition) 
   
#OptionStockLevel and YearsatThecompany
   
   g3 <- ggplot(biotech_select, aes(x=JobSatisfaction, y=MonthlyIncome, color = StockOptionLevel)) +
     geom_point(size=2.5,alpha = 1,shape=16) + 
     geom_smooth(method = lm, se = FALSE)+
     scale_color_brewer(palette = "Accent")+
     theme_light()+
     theme(legend.position = "bottom")+
     labs(title = "Attrition",
          ,
          y = "MonthlyIncome", x = "Job Satisfaction")
   
   g3 +  facet_grid(. ~ Attrition)
   
#Distribution between the Monthly Income and Age
   ggplot(biotech_select, aes(x = Age, y = MonthlyIncome, color = Attrition)) +
     geom_point() +
     facet_wrap(~ Department)+
     labs(title = "Attrition by Age and Monthly Income")
   
   
   ggplot(biotech_select, aes(x = Age, y = MonthlyIncome, color = Attrition)) +
     geom_point() +
     facet_wrap(~ JobRole)+
     labs(title = "Attrition by Jobtitle and Monthly Income")
#Distribution between the Years At Company and Age     
   ggplot(biotech_select, aes(x = Age, y = YearsAtCompany, color = Attrition)) +
     geom_point() +
     facet_wrap(~ Department)+
     labs(title = "Attrition by Age and Years At Company")  
   
#Distribution between the RelationshipSatisfaction and Age     
   ggplot(biotech_select, aes(x = PerformanceRating, y = StockOptionLevel, color = Attrition)) +
     geom_point() +
     facet_wrap(~ Department)+
     labs(title = "Attrition by Age and Work-Life Balance") 
   biotech_select2 <- biotech_select
   biotech_select2$JobInvolment <- as.factor(biotech_select2$JobInvolment)
   str(biotech_select2)
   
 #MonthlyIncome vs OverTime
   g <- ggplot(biotech_select, aes(x=YearsAtCompany, y=MonthlyIncome,color = Job.Involvement)) +
     geom_point(size=2.5,alpha = 1,shape=16) + 
     geom_smooth(method = lm, se = FALSE)+
         scale_color_brewer(palette = "Accent")+
     theme_light()+
     theme(legend.position = "bottom")+
     labs(title = "Attrition",
          ,
          y = "Monthly Income", x = "Years At Company")
   
   g+  facet_grid(. ~ Attrition) 
   

   
# binary encode ordinal variables 
  LessThan5k <- ifelse(biotech_select$MonthlyIncome < 5000, 1, 0)
   biotech_select$NewWorker <- ifelse(biotech_select$NumCompaniesWorked <=1, 1, 0)
   biotech_select$LowLevel <- ifelse(biotech_select$JobLevel == 1, 1, 0)
   biotech_select$NewHire <- ifelse(biotech_select$YearsAtCompany <4, 1, 0)
   biotech_select$WorkedOver30 <- ifelse(biotech_select$TotalWorkingYears >=30, 1, 0)
   biotech_select$Uninvolved <- ifelse(biotech_select$JobInvolvement <2, 1, 0)
   biotech_select$NewToRole <- ifelse(biotech_select$YearsInCurrentRole <=1, 1, 0)
   biotech_select$Unbalanced <- ifelse(biotech_select$WorkLifeBalance <2, 1, 0)
   biotech_select$SalaryHike <- ifelse(biotech_select$PercentSalaryHike  >20, 1, 0)
   biotech_select$HighlySatisfied <- ifelse(biotech_select$JobSatisfaction == 4, 1, 0)
   biotech_select$LongCommute <- ifelse(biotech_select$DistanceFromHome >= 15, 1, 0)
   biotech_select$AgeUnder40 <- ifelse(biotech_select$Age <40, 1, 0)
   biotech_select$DueForPromotion <- ifelse(!biotech_select$YearsSinceLastPromotion %in% c(1,5,6,7), 1, 0)
   biotech_select$TopPerformer <- ifelse(biotech_select$PerformanceRating == 4, 1, 0)
   biotech_select$NoStock <- ifelse(biotech_select$StockOptionLevel < 1, 1 , 0)
   biotech_select$NoTraining <- ifelse(biotech_select$TrainingTimesLastYear < 1, 1, 0)
   biotech_select$LogIncome <- log(biotech_select$MonthlyIncome)
   biotech_select$SqIncome <- biotech_select$MonthlyIncome^2
   biotech_select$SqRtIncome <- sqrt(biotech_select$MonthlyIncome)
   rdata <- subset(data, select = -c(Over18, Department, JobRole, MaritalStatus, EducationField, EmployeeCount, StandardHours))
   
  