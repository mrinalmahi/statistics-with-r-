### Stats with R Exercise sheet 8

##############################################################################
# Week 10: Linear Mixed Effects Models
##############################################################################

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

###############################################################################
###############################################################################

# The following line of code clears your workspace:
rm(list = ls())

library(languageR)
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


###############################################################################
### 1. Linear mixed model for chicken growth 
###############################################################################

## a) We will first look at the dataset ChickWeight, which is already 
##    loaded in base R. Check out the help page of the data set to understand 
##    how the data was collected and look at the summary.
help("ChickWeight")
ChickWeight


## b) Let's plot the data. 
##    1) Group the data by Diet and Time. Use a function summarySE() 
##       from Rmisc library to get the mean and se of weight. 
##       Assign resulting dataset to aggData.
library(Rmisc)
?summarySE()
aggData <-summarySE(ChickWeight,measurevar="weight",groupvars=c("Diet","Time"))
aggData


##    2) Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(x=Time, y=weight,color=Diet))+
  geom_errorbar(aes(ymin=weight-1.96*se, ymax=weight+1.96*se), width=.1)+
  geom_line() 
  

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data
test <-ggplot(ChickWeight, aes(x=Time, y=weight,color=Chick))+
  geom_line() 
test+ facet_grid(~Diet)


## d) What do you observe, looking at c?
# Looking at c we can see that the four different classes of diets are shown as line plots and on every plot we can see different classes of chick distributed with various colors
# That is we can see the growth of each and every chick distinctively over the period of time 


## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##    1) What fixed effect(s) do you enter into the model?
#Here the fixed effect is Diet


##    2) what random effect(s) should be included to account for the repeated measures structure of the data?

# The random effect that needs to be accounted for is Chick.


##    3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?
#Time should be added as random slope to get maximal model

## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod <- lmer(weight~ Time*Diet + 
                   (1+ Time|Chick), data=ChickWeight) 
chickmod


## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <- lmer(weight ~ Time+(1+Time|Chick), data=ChickWeight) 


## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chicknull,chickmod)



## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis
# Here the obtained P value is 4.656e-05. Here the result is a low p value indicating that the difference between the models
#is significant.

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])
#Here from the graph we can see that the intercept gives us the starting weight of each chick. The slope represent the rate at which each chick 
# grows over time. From the graph we can see that the growth of each chick depends on the diet given




#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set from Sheet 4 and suppose, we want to look 
##    at effects of the word type of the previously presented word (each subject saw a 
##    different randomized sequence) and effects of the complexity of the word itself, while 
##    taking into account the dependence between data points collected on the same word and from the same subject. 
##    Which of the following models has a maximal random effect structure given the experimental design?
##    Motivate your choice.

m1 = lmer(RT ~ PrevType + Complex + (PrevType        |Subject) + (         Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType        | Word), lexdec)
m3 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType + Complex + (         Complex|Subject) + (PrevType        | Word), lexdec)
m5 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (1               | Word), lexdec)



## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##    to their final math grade in school. The summer school course has 200 participants, coming from 8 different
##    partner Universities from all over Germany. These 200 participants were randomly split into 10 tutorial groups,
##    where each tutorial was held by a different tutor.
##    Given the design of your study, what random effects should you add to the following model:
##    NOTE: We accept only the answers with explanations!

## lmer(advancedalgebrascore ~ mathGrade, someData)



