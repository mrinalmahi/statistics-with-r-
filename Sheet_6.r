### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:Mrinal Mahindran-
## Matriculation number:7028742
## Name: Anas Mohammed Ali
## Matriculation number:7028821
## Name:Meenu Anil
## Matriculation number:7028669
###########################################################################################



#################################
### Exercise 1: One-way ANOVA
#################################

library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(tidyr)
library(gridExtra)

## This time we will be working with the "anorexia" data frame (package 'MASS') 

## This is a data set of a clinical study with 3 conditions: Two groups received an active treatment,
## while the control group did not receive treatment. The study population is anorexia patients
## and the recorded response is the weight before the study and the weight after the study for
## for each patient.


## a) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.
data("anorexia")
data<- anorexia
## b) In a first step, we will concentrate on the dependent variable Postwt and
##  Treat as the predictor variable (we will assume that the weight before treatment is comparable between groups). 
##  Please formulate a sensible research hypothesis.
## "Active treatment is a good treatment method of anorexia patients"
## c) Build a boxplot of Postwt depending on "Treat". Please use ggplot here and below!
ggplot(data=data, aes(x=Treat, y=Postwt)) +
  geom_boxplot()
## d) Looking at the boxplots, is there a difference between the weight between the
##  3 treatment groups?
## Yes, from the box plots, we can see a significant difference between the groups.
## The min, max and mean of Post weights in all the three groups are different.

## e) Now we are ready to perform 1-way ANOVA: please use the function aov() on 
## Postwt depending on Treat and assign the result to aov1way
aov1way <- aov(Postwt ~ Treat, data=anorexia)


## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why:

## f) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)
##We can use Q-Q Plot to check if a assumption is validated or not.
## i)  The response variable for each levels have a normal distribtuion: Not violated
## ii) All the different levels have different variance.               : Violated
## iii) All the datas are independent                                  : Not violated

## g) Normality of residuals (figure out the best way to check this assumption)
qqnorm(aov1way$residuals)
qqline(aov1way$residuals)

## h) What do you conclude from your results in g? (give a detailed justified answer to whether it is violated or not)
## we can see from the Q-Q Plot that the the points fails to align along the qq line at the ends.
## This shows that the given levels have different variance.
## i) Homogeneity of variance of residuals (figure out the best way to check this assumption)
# Barlett's test is used to to test if k samples are from populations with equal 
# variances. It has the following null hypothesis: all k population variances 
# are equal. We ran the test with alpha-value = 0.05.
bartlett.test(Postwt ~ Treat, data=data)

## j) What do you conclude from i? (give a detailed justified answer to whether it is violated or not)

## k) What are your options if you detect that the data violates the ANOVA assumptions? 

## l) Now we turn to the results. Look at the summary of aov1way
summary(aov1way)

## m) State your conclusion

## n) Use paired.t.test in order to test which levels of Treat are actually different. Use
## "bonferroni" as the method of p-value adjustment.

pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = "bonferroni")

## o) Bonferroni is known to be a conservative method: it preserves the nominal alpha level,
##  but lacks power to detect effects. An alternative is the "holm" method, which also
##  preserves the overall alpha level, but is less conservative. Try this method.
pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = "holm")

## p) State your conclusions.


##################################
### Exercise 2: 2-way ANOVAF
##################################

## Above, we have only looked at post treatment weights. If the sample is big and
## patients were randomly assigned to treatment groups, this is fine to measure the
## success of the treatment as we can assume that weight before the treatment is 
## similar between groups.

## a) Create a graph to see whether prewt is similar between Treat groups.
ggplot(data=data, aes(x=Treat, y=Prewt)) +
  geom_boxplot()

## b) What is your conclusion?

## Next, we will transform the data set, such that we have one variable combining
## both Prewt and Postwt values and an additional factor coding for Time. This will allow us
## to directly address the change in weight under different treatments in a factorial
## ANOVA.
## Please run the following command.

data_long = anorexia%>% pivot_longer(c(Postwt,Prewt), names_to = "Time", values_to = "Weight") %>%
  mutate(Time = factor(Time, levels=c("Prewt","Postwt")))
summary(data_long)

## c) Plot boxplots for the distribution of `Weight` for each of the `Time` 
## values for data_long. Build 3 plots (each containing 2 boxplots) side by side depending on the 
## `Treat` variable.
CBT <- ggplot(subset(data_long, Treat == 'CBT'), aes(x = Weight, y = Time)) +
  geom_boxplot() +
  ggtitle("CBT")

Cont <- ggplot(subset(data_long, Treat == 'Cont'), aes(x = Weight, y = Time)) +
  geom_boxplot() +
  ggtitle("Cont")
Ft <- ggplot(subset(data_long, Treat == 'FT'), aes(x = Weight, y = Time)) +
  geom_boxplot() +
  ggtitle("FT")

grid.arrange(grobs=list(CBT,Cont,Ft))

## d) Describe the pattern you observe in c)

## e) build a two-way ANOVA including Time and Treat as predictors and their interaction
##  and assign it to aov2way.
aov2way <- aov(Weight ~ Time*Treat, data_long)
aov2way

## f) Report your results in line with the research question.

## g) In order to evaluate the interaction, we will use pairwise tests again. The
## function, we are going to use here is TukeyHSD. Please call the function on the 
##  two-way anova
TukeyHSD(aov2way)

## h) The interaction between Time and Treat produces 15 (!) different comparisons,
##  but not all of them are meaningful to us. Please select three comparisons to report, 
##  which conceptually make most sense! Explain your choice!



#################################################
### Exercise 3: independence assumption
#################################################

## The two-way ANOVA above violates the independence assumption.
##  a) Explain why.
#While doing the two way annova test a dependency is created within the data as the active treatment is working for some of the patients therefore it violates the independence assumption

##  b) Can you think of a way to conduct an ANOVA on this dataset without violating
##  the independence assumption, but taking into account differences between groups 
##  prior to treatment?

