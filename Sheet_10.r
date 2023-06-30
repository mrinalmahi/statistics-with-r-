### Stats with R Exercise sheet 10

###############################################################################
# Week 12: Model Selection, Transformations, Power
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 22. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:Mrinal Mahindran-
## Matriculation number:7028742
## Name: Anas Mohammed Ali
## Matriculation number:7028821
## Name:Meenu Anil
## Matriculation number:7028669

###############################################################################
###############################################################################

# The following line of code clears your workspace:
rm(list = ls())


###############################################################################
### Exercise 1 Simplifying random effect structures
###############################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for 
##  effects of Frequency, the type of the previous Word and the native 
##  language of the participant:

m = lmer(RT ~ PrevType + Frequency + NativeLanguage + 
              (PrevType + Frequency | Subject) + (PrevType + NativeLanguage | Word), 
        data = lexdec, REML = F)
help('isSingular')


## a) Unfortunately, the maximal model given above gives a warning that indicates 
##    that the model is too complex for the data. In order to get a model that converges 
##    without warnings, try to use backwards selection on the random effects. 
##    First exclude the random effect that is least contributing to the model fit and so on 
##    (this may require multiple steps and a large number of fitted models!). 
##    Use model comparison to decide which effects can be excluded.
##    You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1


## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?


## c) Another approach is to simplify the random effect structure by excluding correlations. 
##    Try out whether this would have solved the problem.
m = lmer(RT ~ PrevType + Frequency + NativeLanguage +
           (PrevType | Subject) + (Frequency | Subject) + (PrevType | Word) + (NativeLanguage | Word),
         data = lexdec, REML = F)


###############################################################################
### Exercise 2 Simulations and power
###############################################################################

## In the following we provide you with code for simulations. 
## The goal of the exercise is for you to try out the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable 
## the results are -- this is necessary because we are sampling the data randomly, 
## so it could be that we sometimes get more or less "lucky" draws. 

n        <- 200 # number of observations to be simulated
predA    <- rnorm(n, 80, 20)
predB    <- rnorm(n, 30, 30)
interact <- 0.08*(predA*predB) 
error    <- rnorm(n, 0, 50)

resp     <- 42 + 0.2*predA - 5.3*predB + interact + error
d        <- data.frame(predA, predB, resp)

## a) Write down what values you would hope for the model to estimate in the ideal case:
#i) intercept = 42
#ii) predA = 0.2
#iii) predB = -5.3
#iv) predA:predB = 0.08

## b) Can the model recover the original model structure and estimate correct coefficients 
##    for the predictors?
#It is possible that the model can recover the original model structure and estimate correct coefficients for the predictors, but it depends on the sample size and the distribution of the error term. If the sample size is large and the error term is normally distributed with a small variance, it is more likely that the model will recover the correct coefficients. However, if the sample size is small or the error term has a large variance,
#it may be more difficult for the model to recover the correct coefficients. 


## c) What happens if you change the number of subjects? (specify the numbers you tried out!)


## d) What happens if you change the variance of the error term? (specify the numbers you tried out!)


## e) What happens if you change the effect sizes? (specify the numbers you tried out!)



## Next we include the above code into a loop to calculate the power of the experiment 
## number of simulated data sets
sim = 1000 # number of simulations
n   = 100  # number of participants in each simulation

## results matrix
results = matrix(nrow=sim, ncol=4)

colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08*(predA*predB)
  error    <- rnorm(n, 0, 50)
  resp     <- 42 + 0.2*predA - 5.3*predB + interact + error
  d        <- data.frame(predA, predB, resp)
  m1       <- lm(resp~predA*predB, data=d)
  
  ## store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}


## f) We use the above code and the results matrix to calculate power. Recall that the power is 
##    the probability of rejecting the Null hypothesis, given a specific effect size.
##    We can approximate this by calculating the proportion of simulated datasets, 
##    where the effect comes out significant, i.e. below 0.05. 
##    Calculate the power based on the simulations for all three effects of interest 
##    (i.e., predA, predB and the interaction) individually.
power_predA <- mean(results[,2]<0.05)
power_predB <- mean(results[,3]<0.05)
power_interact <- mean(results[,4]<0.05)



## g) How does power change when you decrease your alpha level to 0.01?
#When decreasing the alpha level from 0.05 to 0.01, the power of the experiment will decrease. This is because a lower alpha level means a higher threshold for rejecting the null hypothesis,
#making it harder to detect an effect as statistically significant.


## h) How does power change, when you decrease the number of participants in each simulated data 
##    set to 50? (alpha-level = 0.05)
#When decreasing the number of participants in each simulated data set to 50, the power of the experiment will decrease. This is because a smaller sample size means less data, 
#which in turn means a less stable and less accurate estimate of the coefficients.

