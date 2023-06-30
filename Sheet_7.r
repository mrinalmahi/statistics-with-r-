### Stats with R Exercise sheet 7

##############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 1. Write the code below the questions. 
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

# The following line of code clears your workspace.

rm(list = ls())


########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises unless specified differently!
########


##a) Load the dataset Salaries from package carData and store it in a variable called data. 
# Familiarize yourself with the content of the dataset: 
# https://r-data.pmagunia.com/dataset/r-dataset-package-car-salaries

library(ggplot2)
library(dplyr)
library(car)
data("Salaries")
data <- Salaries


## b) Run a simple regression, just including 'years since PhD' as predictor and salary as the dependent variable
##  Store it in lm1

lm1 <- lm( salary ~ yrs.since.phd , data = data)
summary(lm1)


## c) Report and explain the effect of 'years since PhD'
# From the above result It can be seen that we cannot correctly predict salary from years since phD because we cannot see a good regression line as the points are highly scattered
# The intercept of the regression line on the salary axis is 91718.7 and has a positive slope of 985.3.




## d) Make a scatterplot of salary by 'years since PhD', including the regression line
ggplot(data, aes(y=salary, x=yrs.since.phd)) +
  geom_point(color = "red") +
  geom_smooth(method ='lm', se =FALSE)


## e) Next, fit a model of salary including 'years since PhD' and discipline as predictors, store it in lm2
lm2 <- lm( salary ~ yrs.since.phd + discipline +0 , data = data)
lm2

## f) Report and explain the effects of 'years since PhD' and discipline.
# From the above result we can see that both the disciplines A and B have the same slope of 1119. Discipline A has intercept of 80158 on the salary axis and discipline B has intercept of 95943.


##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
data$sal_pred = fitted(lm2)

## g) Now, plot the original data (salary by 'years since PhD' with different colors for discipline), but use the 
## fitted values (sal_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.
ggplot(data, aes(y=salary, x=yrs.since.phd, , color = discipline)) +
  geom_point() +
  geom_smooth(data= data, aes(y=sal_pred, x=yrs.since.phd),method="lm",se=FALSE)



## h) Run a regression model that includes also the interaction 
# between 'years since PhD' and discipline and store it as lm3
lm3 <- lm( salary ~ discipline + discipline:yrs.since.phd + 0, data=data)
lm3


## i) Plot the results of the model! (This time no need to specify the pred data set)
ggplot(data, aes(y=salary, x=yrs.since.phd, , color = discipline)) +
  geom_point() +
  geom_smooth(data= data, aes(y=fitted(lm3), x=yrs.since.phd),method="lm", se= FALSE )


## j) Report the results of lm3 and interpret with the help of the graph in i)
# The regression line of Discipline A has an intercept of 84845.4 and a slope of 933.9 while discipline B has an intercept of 92375.4s and a slope of 1299.3

# Also from the graph we can see that the regression lines are not parallel while including interaction unlike the graph in g.



## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)
par(mfcol=c(2,3))
plot(lm3,which = seq(1, 6))



## l) Interpret what you see in k) and possibly suggest further steps


