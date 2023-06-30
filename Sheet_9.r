### Stats with R Exercise sheet 9

##################################################################################
# Week 11: Model Families and Logistic Regression
##################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 8. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:Mrinal Mahindran-
## Matriculation number:7028742
## Name: Anas Mohammed Ali
## Matriculation number:7028821
## Name:Meenu Anil
## Matriculation number:7028669

##################################################################################
##################################################################################

# The following line of code clears your workspace:
rm(list = ls())

library(rstudioapi)
# Set the path to source file location:
path <- "D:/files/saarland/Study/Assignments/statistics with r/sheet9.r"
setwd(dirname(getActiveDocumentContext()$path)) 

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.
## a) Build a simple logistic regression model that models the probability of survival 
##    (binary) based on sex (categorical) and  passengerClass (categorical) without 
##    an interaction and store it in mSurv. 
##    You have to use the glm() function and specify the family correctly.
TitanicSurvival
mSurv <- glm(survived~sex+passengerClass,data= TitanicSurvival, family="binomial")


## b) Look at the summary. What group does the intercept correspond to?
summary(mSurv)
#intercept in the summary of the mSurv model corresponds to the predicted logarithmic probability of survival for a female passenger in the first class.

## c) Were men more likely to survive than women? Is the effect significant?
#From the summary we can see that men were more likely to survive than women as the coefficient for sex variable is positive. Since the p value is less than 0.05 indicate that it is statistically significant



## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##    Calculate their expected survival on the logit scale (i.e. the scale of the model) 
##    either by hand or using predict() with a new data.frame
Rose <- data.frame(sex = "female", passengerClass = "1st")
Rose
Jack <- data.frame(sex = "male", passengerClass = "3rd")
Jack
Survive_Jack <- predict(mSurv, newdata = Jack,type = "response")
Survive_Jack
Survive_Rose <- predict(mSurv, newdata = Rose, type = "response")
Survive_Rose
logit_rose <- log(Survive_Rose / (1-Survive_Rose))
logit_jack <- log(Survive_Jack / (1-Survive_Jack))

## e) Transform your results from d to the probability scale, using the formula given on the slides. 
##    You can check your calculation by asserting the probabilities lie in the 0-1 range. 
##    For whom does the model predict the higher probability of survival?

# probability of survival for Rose
prob_rose <- exp(logit_rose) / (1 + exp(logit_rose))
prob_rose
# probability of survival for Jack
prob_jack <- exp(logit_jack) / (1 + exp(logit_jack))
prob_jack

#The model predicts the higher probability of survival for Rose than Jack with a value of 0.89.



##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption 
## and sleep (among others). The data set "coffee.csv" contains data from 10 students, 
## who reported on 10 randomly chosen days of the year: 
##  sleep:  how many hours of sleep they had in the previous night
##  mood:   how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
## In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they don't feel well
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat <-read.csv("coffee.csv")
coffeedat



## b) Plot the number of consumed cups of coffee in three individual scatterplots 
##    by sleep, mood, and temperature. 
##    You can use geom_jitter() to get a nicer plot

#Scatterplot for Sleep
ggplot(coffeedat, aes(x=sleep, y=coffee)) + 
  geom_jitter(width = 0.1) + 
  ggtitle("Number of Consumed Cups of Coffee by Sleep") +
  xlab("Sleep (hours)") + ylab("Coffee Consumed")

#Scatterplot for Mood
ggplot(coffeedat, aes(x=mood, y=coffee)) + 
  geom_jitter(width = 0.1) + 
  ggtitle("Number of Consumed Cups of Coffee by Mood") +
  xlab("Mood (1-10)") + ylab("Coffee Consumed")

#Scatterplot for Temperature
ggplot(coffeedat, aes(x=temperature, y=coffee)) + 
  geom_jitter(width = 0.1) + 
  ggtitle("Number of Consumed Cups of Coffee by Temperature") +
  xlab("Temperature (F)") + ylab("Coffee Consumed")





## c) Can you detect an obvious relationship in any of the plots?
## Due to high scattering of the points, it is difficult to detect any obvious relation among them.



## d) Fit a simple linear regression model with all three predictors and store it in linmod
linmod <- lm(coffee ~ sleep + mood + temperature, data = coffeedat)



## e) Fit a generalized linear model with the appropriate family 
##    (hint: coffee is a count variable) and store it in poimod
# Fit the Poisson regression model
poimod <- glm(coffee ~ sleep + mood + temperature, data = coffeedat, family = poisson())




## f) Look at the two summaries of the models and write what changed?
summary(linmod)
summary(poimod)
#The linmod model  gives the R-squared and adj R-squared values, which indicate the proportion of the variation in the response variable that is explained by the predictors. 
#On the other hand, the poimod model gives the deviance, which indicates how well the model fits the data.

#The linmod model  gives the p-values for the coefficients, which indicate the significance of the predictors in explaining the variation in the response variable. 
#On the other hand, the poimod model gives the z-scores, which indicate the significance of the predictors in explaining the variation in the response variable.



## g) In fact, we have repeated measures in our design, so refit the model 
##    including a random intercept for subject using glmer() with the correct 
##    family specification and store it in mixedpoi
mixedpoi <- glmer(coffee~ sleep + mood + temperature + (1|subj), data = coffeedat, family = poisson())



## h) Look at the summary and report what changed in comparison to both linmod and poimod.
#The mixedpoi model allows for modeling the within-subject variability in coffee consumption, by allowing for random effects for the subject variable in addition to the fixed effects of the predictors. 
#This allows the model to account for any non-independence of observations within subjects.



## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
mixedlin <- glmer(coffee ~ sleep + mood + temperature + (1|subj), data = coffeedat, family = gaussian())



## j) Compare the AIC for all four models. Which one has the best fit?
AIC(linmod)
AIC(poimod)
AIC(mixedpoi)
AIC(mixedlin)
#The lowest AIC value is 478.7281 therefore mixedpoi has the best fit

## k) And which model is conceptually the appropriate one? Explain why.
#In this case, a logistic regression model would be appropriate as it is commonly used to model binary outcome variables such as survival or non-survival. 
#The logistic regression model would allow us to estimate the probability of survival as a function of sex and class, and investigate whether these variables have a significant impact on survival.

## l) Finally, report on the effects of interest in light of our research hypotheses 
##    specified above for the model you chose in k)
#The logistic regression model would allow us to estimate the probability of survival as a function of sex and class, and investigate whether these variables have a significant impact on survival.