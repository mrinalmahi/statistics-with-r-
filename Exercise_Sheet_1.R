### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete, please do not leave out subquestions!

## Please write below your (and your teammates') name and matriculation number. 
## Name:Mrinal Mahindran-
## Matriculation number:7028742
## Name: Anas Mohammed Ali
## Matriculation number:7028821
## Name:Meenu Anil
## Matriculation number:7028669


## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.


getwd()

## b) Get help with this function.
help()

## c) Change your working directory to another directory.
setwd("D:/stati")







###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the range for which you want to plot the 
##    normal distribution (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.75. Assign this to the 
##    variable x.
   x <- seq(-5,5,0.75)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution. Use the defaults for mean and sd (standard normal distribution)
y<-dnorm(x,mean=0,sd=1,log=FALSE)

## c) Now use plot() to plot the normal distribution for z values of "x". Specify
## the type to be line using the suitable argument of plot()
plot(x,y,"l")

## d) This plot does not look like a smooth normal distribution. Change the vector
##  x to have smaller increments and plot again (you also need to update y)
x <- seq(-5,5,0.2)
y <-dnorm(x,mean=0,sd=1,log=FALSE)
plot(x,y,type="l")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dotted line, set the argument 'lty' to 3.
help(abline)
abline(v=mean(x),lty=3)

## f) Take a look at the trees dataset (You can see it by typing "trees"), which 
##    has height, diameter and volume.
##    Then select only the Height part and store it in a variable "treesHeight".
trees
treesHeight<-trees$Height
treesHeight
## g) Calculate the mean and standard deviation of treesHeight and plot a normal
##    distribution with these parameters (NB:you should not use the same x range 
##    as above!)
x <- seq(60,90,2)
mean(treesHeight)
sd(treesHeight)
y <-dnorm(x,mean=mean(treesHeight),sd=sd(treesHeight),log=FALSE)
plot(x,y,type="l")

## h) We observe two additional tree height values (62 and 86). What's the 
##    likelihood that these heights (or more extreme ones) respectively 
##    come from the normal distribution from g)?

pnorm(q = 62,mean=mean(treesHeight),sd=sd(treesHeight), lower.tail = T)
pnorm(q = 86,mean=mean(treesHeight),sd=sd(treesHeight), lower.tail = T)
## The likelihood of 62 and 86 are 0.01400391 and 0.941724.

## i) What do you conclude from the p-values? (informal)
##Answer:
## From the p-values, we can conclude that the maximum likelihood is for the height of 76
## and from there the likelihood decreases with a standard deviation of 6.37. 
## The likelihoods are almost zero when the heights reach the extremities.

## j) Use the random sampling function in R to generate 25 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 58 to 94 using xlim. 
##    Fix the number of breaks to 11 using breaks
sample(treesHeight,25)
hist(sample(treesHeight,25),xlim = c(58,94),breaks=11)

## k) What do you observe in j?
##Answer:
## From j, we can observe that, when sampled with 25 samples, the occurrences of some values from the 
## sample was high, having a frequency of 4.












###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)

## b) Specifically, we will deal with the dataset 'lexdec'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
lexdec
#Lexical decision latencies elicited from 21 subjects for 79 English concrete nouns, with variables linked to subject or word.

## c) Inspect 'lexdec'. Look at the head, tail, 
##    and summary. 
head(lexdec)
tail(lexdec)
summary(lexdec)

## d) What do head and tail show you?
# The head function shows the first 6 rows present in the lexdec dataset
#The tail function shows the last 6 rows present in the lexdec dataset

## e) Look at the first 15 rows of the data.frame
head(lexdec,15)

## f) The file contains multiple observations for each participant. Create a 
##   subset only including subject number M2 and assign it to M2
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
 M2<-lexdec[lexdec$Subject=="M2",]
 nrow(M2)
## there are 79 rows in the subset
## g) looking at the summary of M2, what can you find out about the demographic 
##    parameters of this participant?
 summary(M2)
 ## From the summary, we can find that all the participant are females in the selected subset.
 ## Also none of them have English as their language.

## h) Create a histogram (using hist()) of "RT" (logarithm of reaction time) 
##    for M2
 hist(M2$RT)

## i) Create a kernel density plot for this data using density()
 plot(density(M2$RT))

 ## j) What is the difference between the two?
 ## The kernel density plot gives much flexibility of the data and the values are continuous.
 ## The y-axis shows density from 0 to 2 approximately
 ## The histogram is more discrete and has less flexibility of the data.
 ## The y-axis shows frequency from 0 to 30 approximately

 ## k) Is this data likely from a normal distribution? How would you check ?
 ##    (describe in words, remember to comment out text)
 #No, this data is not from a normal distribution. we can check by the qq plot and
 # the fat pencil test as given below.
 qqnorm(M2$RT, pch = 1,frame=FALSE)

## l) Looking at the graph, do you think the data is skewed? In which direction?
 ## Yes, the data is positively skewed.

#############################################
### Exercise 4: Dataframes and boxplots
#############################################
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18	15	18	19	23	17	18	24	17	14	16	16	17	21	22	18	20	21	20	20	
# 16	17	17	18	20	26


 ## a) What measurement scale is this data? Is it discrete or continuous? Explain
 ##    in one sentence why.
 ## This is a classical example of discrete data. It is a ratio scale. The teacher was counting and 
 ## she cannot take a decimal value since it was a word count..

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))
 lib
## The variable has nominal scale.
 
## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pid', and your 
##    participants should be labeled from 1 to 26
 pid<- c(1:26)
 pid

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
 obs <-c(18,15,18,19,23,17,18,24,17,14,16,16,17,21,22,18,20,21,20,20,16,17,17,18,20,26)

## e) Create a dataframe including pid, obs and lib. Assign this to 'stories'. 
 stories<-data.frame(pid,obs,lib)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pid' and 'lib'?
 summary(stories)
 class(stories$pid)
 class(stories$lib)

#Class of pid is integer and the class of lib is character
## g) Change the class of 'pid' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
 stories$pid <-as.factor(stories$pid)
 
 stories$lib <-as.factor(stories$lib)
 
 ## we change the class of pid from integer to factor as we need to unorder the "pid" column
 ## and by this we can create an independency between the data.
 ## That is, these are participant ID's and we don't need an order in here. Therefore, factors.
 
 ## we change the class of lib from character to factor to bring more meaning as it creates
 ## levels to these characters. for instance, it is easier to type "Y" instead of "Yes".
 ## Also it can reduce the size of the code as it is stored as labelled integers when factored.
 

## h) Create a boxplot of obs for the two lib groups
 boxplot(stories$obs ~ stories$lib)

## i) Are there outliers in one of the lib groups?
 # Yes as seen from the boxplot there are outliers in both groups

## j) Which group shows the larger interquartile range? 
 # The N group shows the larger interquartile range

## k) Which one has the greater overall range?
 #The N group shows the larger overall range

## l) What is a whisker? Why is the upper whisker of group "Y" so short?
 #The whiskers are two lines outside the box, one goes from the minimum to the lower quartile and the other goes from the upper quartile to the maximum
 
 #The upper whisker of Y is shorter because the distribution is negatively skewed

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?
## In the boxplot the median of y is plotted
## The mean and the median is different because the data is not normally distributed and
## the data exhibits positive skewness.
 mean(stories[stories$lib == "Y", 'obs'])
 abline(h=mean(stories[stories$lib == "Y", 'obs']))
 