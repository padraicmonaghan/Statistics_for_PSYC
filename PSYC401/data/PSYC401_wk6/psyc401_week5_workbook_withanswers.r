# PSCY401 Workbook
# Week 5
# ANSWERS

#####################################################################
# Part 1: Revision

####################################
# Task 1: Checklist: What I can now do

# You should be able to answer yes to all the following. If you can't
# yet, go back to the previous workbooks and repeat your working until
# you can answer yes, being able to type in and run the commands without
# referring to your notes. 
# 1. I can open R-studio

# 2. I can install and open new libraries using install.packages() and library()

# 3. I can make an R script file

# 4. I can input a file into an object in R-studio using read.csv()

# 5. I can join two files together using inner_join()

# 6. I can select certain variables from an object using select()

# 7. I can select subsets of data using filter() (e.g., I can select
# participants in two conditions from a data set containing participants
# in four conditions)  

# 8. I can make new variables using mutate()

# 9. I can arrange data according to subsets using group_by()

# 10. I can change format of data from wide to long format using pivot_longer

# 11. I can change format of data from long to wide format using pivot_wider

# 12. I can produce summaries of means and standard deviations for
# subsets of data after applying group_by() using summarise() 

# 13. I can draw histograms of single variables, point plots of two
# ratio/interval/ordinal variables, bar plots of counts, and box plots
# of one categorical and one ratio/interval/ordinal variable using
# ggplot() 

# 14. I can run a Chi-squared test and Cramer's V test using chisq.test() and cramersV()

# 15. I can interpret the results of a Chi-squared test and Cramer's V
# test and write up a simple report of the results. 

# 16. I can save an R script file.

# ANSWER here are some examples of the commands/functions in use:
rm(list=ls())
library(tidyverse)

# from week4 practical
dat <- read.csv("PSYC401-shipley-scores-anonymous-17_22.csv"); 
dat$academic_year <- as.factor(dat$academic_year)
View(dat)
# from week2 practical
dat2 <- read.csv("../../week2/practical/data/ahicesd.csv")
pinfo <- read.csv("../../week2/practical/data/participantinfo.csv")

all_dat <- inner_join(x = dat2, y = pinfo, by = c('id', 'intervention'))
summarydata <- select(.data = all_dat, id, ahiTotal, cesdTotal, age, occasion)
dat_17 <- filter(.data = dat, academic_year == '201718')
all_dat2 <- group_by(.data = all_dat, intervention, occasion)
summarise(.data = all_dat2, mean(ahiTotal), sd(ahiTotal), n())
ggplot(dat, aes(x = Age)) + geom_histogram()
ggplot(dat, aes(x = Shipley_Voc_Score, y = Gent_1_score)) + geom_point()
ggplot(dat, aes(x = Shipley_Voc_Score, y = Gent_1_score)) + geom_point() + geom_smooth( method = lm)
all_dat$occasion <- as.factor(all_dat$occasion)
ggplot(all_dat, aes(x = occasion, y = ahiTotal)) + geom_boxplot()
dat_long <- pivot_longer(dat, names_to = "test", values_to = "score", cols = c("Gent_1_score", "Gent_2_score"))
#or:
dat_long <- pivot_longer(dat, names_to = "test", values_to = "score", cols = starts_with("Gent"))
dat_wide <- pivot_wider(dat_long, names_from = "test", values_from = "score")

library(lsr)
chisq.test(x = all_dat$occasion, y = all_dat$intervention)
cramersV(x = all_dat$occasion, y = all_dat$intervention)


#########################################################################
# Part 2: Running an independent t-test

#####################################
# Task 2: Load, prepare, and explore the data

# 17. Clear out R using rm(list=ls())

rm(list=ls())

# 18. Load again the data set on the Shipley and Gent vocabulary scores. 

# ANSWER:
dat <- read.csv("PSYC401-shipley-scores-anonymous-17_22.csv"); 
dat$academic_year <- as.factor(dat$academic_year)

# 19. Set the research question: do people who self-identify as male or female have 
# different scores on the Gent vocabulary test? The research hypothesis is:
# "People who identify as male or female have different vocabulary scores". 
# What is the null hypothesis?
  
# ANSWER: there is no difference between people who self-identify as male
# or female on vocabulary scores.

# 20. To test the research hypothesis, we will filter people who self-identify
# as male or female from the data set. To be inclusive, additional research
# questions would be part of your research project to analyse also
# people who self-identify as other gender. Run this command to extract a 
# subset of the data (note that the | stands for "or", and means Gender matches
# male or gender matches female):

dat2 <- filter(.data = dat, Gender == 'Male' | Gender == 'Female')

# 21. Draw a box plot of Gent vocabulary test 1 scores by gender. 
# For a box plot, note that we need data in "long format", where 
# each observation is on one line, and we have a column that indicates
# which condition (in this case Gender) the participant is in.
# Does it look like there might be a gender effect? What is the direction of
# the effect? 

# ANSWER: 
ggplot(dat2, aes(x = Gender, y = Gent_1_score)) + geom_boxplot()

# Maybe males are slightly higher, but looks like a lot of overlap?

# 22. Note that unless we had filtered the data, the box plot would contain
# 'NA' as well, which stands for missing data. 
# In a data set it's always a good idea to call missing data 'NA' rather than
# just leaving them blank because this could be interpreted as a zero or
# as an error of filling in data. 
# Missing values make things untidy, so it's good practice to focus only on the
# variables we need for the t-test and remove all other missing
# values. Use select() to get just the Gender and Gent_1_score
# variables, and put this in a new object called 'dat3'. 

# ANSWER: 
dat3 <- select(.data = dat2, Gender, Gent_1_score)

# 23. Next, in order to run a t-test we have to remove any rows of data 
# which contain a 'NA' - either in the Gender or the Gent_1_score variables. 
# We do this using drop_na(dat3), put the result in a new object called 'dat4' 
# Run this command: dat4 <- drop_na(dat3)

# ANSWER: 
dat4 <- drop_na(dat3)

# 24. Now, redraw the box plot from Step 21. Check there are just two groups.

# ANSWER: yes there are just two groups.
ggplot(dat4, aes(x = Gender, y = Gent_1_score)) + geom_boxplot()

# 25. Compute mean and SDs for people who self-identify as male or female
# on Gent vocabulary test 1 scores (hint: use group_by() and summarise()). 

# ANSWER: 
dat5 <- group_by(.dat = dat4, Gender)
summarise(.dat = dat5, mean(Gent_1_score), sd(Gent_1_score))
# if you know how to pipe...:
group_by(.dat = dat4, Gender) %>% summarise(mean(Gent_1_score), sd(Gent_1_score))


####################################
# Task 3: Run the independent t-test and measure effect size

# 26. Conduct an independent t-test using this command:

t.test(Gent_1_score ~ Gender, paired = FALSE, data = dat5 )

# - 'Gent_1_score ~ Gender' : the ~ can be interpreted as 'by', i.e.,
# compute Gent_1_score by Gender 
# - 'paired = FALSE' : this means we are doing an independent t-test (a
# paired t-test would have paired = TRUE) 

# 27. The results should look like this, do yours?

# Welch Two Sample t-test
# 
# data:  Gent_1_score by Gender
# t = -1.7356, df = 62.409, p-value = 0.08756
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -10.0862020   0.7105407
# sample estimates:
#   mean in group Female   mean in group Male 
# 57.57407             62.26190 
# 

# 28. The key part of the results to look at is the one that has t = -1.7356, df =
# 62.409, p-value = 0.08756. This is the result that you report:
# t(62.41) = -1.74, p = .088.

# The value is negative because the function includes Female before Male
# - and Female score is lower than Male score. What matters is how far
# away from zero the t-test is (either positively or negatively). 
# The df value is slightly odd because the t.test() function figures out
# degrees of freedom in a technical way which takes into account
# differences in variance in the data between the two groups. We can
# just use the value that the t.test() function gives us. 

# 29. Is this a significant difference? 

# ANSWER: no.

# 30. Now we need to compute the effect size, using Cohen's d. You need
# the library lsr (you might need to install.packages("lsr") first): 
library(lsr) 
install.packages("lsr")
# then use this command:
#dat5$Gender <- droplevels(dat5$Gender)
cohensD(Gent_1_score ~ Gender, method = "unequal", data = dat5 )

# - It's pretty much the same as the t-test() command except that we use
# 'method = 'unequal' instead of 'paired = FALSE'. For a paired t-test
# you would use 'method = 'paired' 

# WATCH OUT! If you get this error:
# Error in cohensD(Gent_1_score ~ Gender, method = "unequal", data = dat2) : 
# grouping factor must have exactly 2 levels

# then you need to run this following command before the cohensD command:
dat5$Gender <- droplevels(dat5$Gender)

# The reasons for needing this are a bit of a pain in the neck. It is because
# even though we have filtered the data to only include people who self-identify
# as male or female at Step 20, if R studio interprets the Gender variable to be 
# a nominal/categorical variable then it will encode that there are three levels
# of the nominal/categorical variable. If we remove some of the data within those
# three levels, that doesn't make any difference, R studio still encodes that
# there are these three categories of gender. The t.test() command is fine with
# this but the cohensD() function can't cope.


# 31. What is the effect size? Make a brief report of the results -
# reporting means and SDs, the t-test, p-value, and Cohen's d. Discuss
# your brief report in your group.

# ANSWER: d = 0.30
# We hypothesised that people who self-identify as male or female would vary in
# terms of vocabulary scores. Males (mean = 62.3, SD = 15.7) scored higher than 
# females (mean = 57.6, SD = 15.2) on the first time participants attempted the 
# Gent vocabulary test, however this difference was not significant,
# t(62.41) = -1.74, p = .088, Cohen's d = 0.30. 

# 32. Make sure all commands are in the source window, save them as a new 
# R script file.


###############################################################
# Task 4: Practise running another independent t-test

# 33. Next research question: do people who are native English speakers
# have different vocabulary scores than those who learned English as a
# second language? What is the research hypothesis and the null hypothesis?

# ANSWER: 
# Research Hypothesis: People who speak English as a native language have higher 
# vocabulary scores than those with English as a second language.
# Null Hypothesis: There is no difference in vocabulary scores between native 
# English and second language English speakers.

# 34. Repeat the Steps 22-30 in Tasks 2 and 3 except using english_status 
# in place of Gender throughout.

# ANSWER: 
dat <- read.csv("PSYC401-shipley-scores-anonymous-17_22.csv")
dat2 <- select(.data = dat, english_status, Gent_1_score)
dat3 <- drop_na(dat2)
ggplot(dat3, aes(x = english_status, y = Gent_1_score)) + geom_boxplot()
dat4 <- group_by(.dat = dat3, english_status)
summarise(.dat = dat4, mean(Gent_1_score), sd(Gent_1_score), n())
t.test(Gent_1_score ~ english_status, paired = FALSE, data = dat3 )
cohensD(Gent_1_score ~ english_status, method = "unequal", data = dat3 )

# 35. Write a brief report of the results, including means and SDs for
# native speakers and ESL speakers, t-test, p-value, and Cohen's d. 
# Discuss your report in your group.

# ANSWER: We hypothesised that whether English was a speaker's first or
# second language would have an effect on vocabulary scores. Native
# English speakers (mean = 65.4, SD = 10.3) scored significantly higher
# on the Gent vocabulary test than speakers of English as a second
# language (mean = 46.9, SD = 15.7), t(114.08) = -9.17, p < .001,
# Cohen's d = 1.39, a large effect size.

# 36. Save your R script file.

####################################################
# Part 3: Conducting a paired t-test

####################################
# Task 5: Conducting a paired t-test 

# 37. Clear out R-studio before we get started again:
rm(list=ls())

# 38. We are going to investigate again the data from this paper:
# Woodworth, R.J., O'Brien-Malone, A., Diamond, M.R. and Schuez, B.,
# 2018. Data from, "Web-based Positive Psychology Interventions: A
# Reexamination of Effectiveness". Journal of Open Psychology Data,
# 6(1). 

# Our research question is whether happiness scores are affected
# by the interventions. We will look at the pre-test (occasion 0) 
# and the first test after the intervention (occasion 1).

# 39. What is the research hypothesis and what is the null hypothesis?

# ANSWER: RH: happiness scores change (increase) from the first to the second
# occasion of testing. NH: happiness scores do not change across 
# occasions.

# 40. For a paired t-test we can only include data from people who have
# produced scores at both occasions of testing. So, we need a slightly
# different version of the data. Go to week 5 moodle, download these
# files: ('ahicesd.csv', and 'participantinfo2.csv'). Remind yourself
# what these data mean.  
      
# ANSWER:
dat <- read.csv("ahicesd.csv")
pinfo <- read.csv("participantinfo2.csv")
      
# 41. Once again, join the ahicesd.csv and participantinfo2.csv data in
# R-studio by aligning the names for the participant bumbers in these two data sets 
# (see week 2 workbook for reminders about this). 

# ANSWER:
all_dat <- inner_join(x = dat, y = pinfo, by = c('id', 'intervention'))

# 42. Let's select only the relevant variables. Use select() to select only 
# id, ahiTotal, and occasion variables, and save this as a new object 
# called 'summarydata' 

# ANSWER: 
summarydata <- select(.data = all_dat, id, ahiTotal, occasion)

# 43. Use filter to pull out only occasion == 0 or occasion == 1 scores
# (hint: use 'occasion == 0 | occasion == 1'), save this as a new object
# called 'summarydata2' 

# ANSWER:
summarydata2 <- filter(.data = summarydata, occasion == 0 | occasion == 1)

# 44. Here is where we would usually remove all the NA values, but there aren't
# any in this file (so we don't need drop_na()). 

# 45. Now, we need to make sure occasion is treated as a categorical
# variable, rather than a continuous variable, so we need to convert it
# to a factor:
summarydata2$occasion <-as.factor(summarydata2$occasion)

# 46. Now, draw a box plot of ahiTotal scores by occasion (why do we 
# use a box plot?)

# ANSWER: 
ggplot(summarydata2, aes(x = occasion, y = ahiTotal)) + geom_boxplot()
# we use a boxplot because we have one nominal/categorical variable and one
# interval/ratio/ordinal measure.

# 47. Compute mean and SD for each occasion

# ANSWER: 
dat5 <- group_by(.dat = summarydata2, occasion)
summarise(.dat = dat5, mean(ahiTotal), sd(ahiTotal))

#   occasion 'mean(ahiTotal)' 'sd(ahiTotal)'
#   <fct>               <dbl>          <dbl>
# 1 0                    69.3           12.3
# 2 1                    72.4           12.6

# 48. Run the paired t-test: it's the same as for the independent t-test
# except that we use 'paired = TRUE' in place of 'paired = FALSE':

t.test(ahiTotal ~ occasion, paired = TRUE, data = summarydata2)

# Is the result significant? 

# ANSWER: yes, significant.
# 	Paired t-test
# 
# data:  ahiTotal by occasion
# t = -4.9224, df = 146, p-value = 2.282e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -4.347519 -1.856563
# sample estimates:
# mean of the differences 
#               -3.102041
	      
# 49. Before we run the Cohen's d command for these data, we have to
# make sure we have a list of the participants in one condition,
# followed by the list of participants in the other condition. We can do
# this using the command arrange():
summarydata3 <- arrange(.data = summarydata2, occasion)

# 50. Now run Cohen's d: it's the same as for the independent t-test
# except that we use 'method = 'paired':
cohensD(ahiTotal ~ occasion, method = "paired", data = summarydata3)

# ANSWER: d = 0.4059904

# 51. Write up a brief report of the result and discuss in your group.

# ANSWER: We tested whether participants' happiness scores at first
# testing after the interventions were different than their scores prior
# to the interventions. We found that prior to the intervention scores
# were significantly lower (M = 69.3, SD = 12.3) than they were
# immediately after the interventions (M = 72.4, SD = 12.6), t(146) =
# = 4.92, p < .001, Cohen's d = 0.41. (it is a medium effect, and
# significant (partly because there are a lot of participants being tested)).

# 52. Save your R script file.


#####################################################
# Part 4: More practise running a paired t-test

# We are going to figure out whether people have different scores the
# first and second time they take the Gent vocabulary test.

# 53. Go back to the vocabulary scores data. Load the data into dat, and make
# another object dat2 that contains only the subject_ID, Gent_1_score and Gent_2_score.

dat <- read.csv("PSYC401-shipley-scores-anonymous-17_22.csv"); 
dat$academic_year <- as.factor(dat$academic_year)
dat2 <- select(.data = dat, subject_ID, Gent_1_score, Gent_2_score)

# 54. Some people did not do all the tests - look at participant 46 for instance.
# To do a t-test we need data where the person does both tests.
# We can filter out the scores where there are no NAs by repeating the drop_na we did at step 23, above.
# Call the new data object dat3.

dat3 <- drop_na(dat2)

# 55. Now to do a paired t-test we need to get the Gent_1_score and Gent_2_scores into the same column.
# We do this with pivot_longer, run this command:
# dat4 <- pivot_longer(dat3, names_to = "test", values_to = "score", cols = c("Gent_1_score", "Gent_2_score"))


dat4 <- pivot_longer(dat3, names_to = "test", values_to = "score", cols = c("Gent_1_score", "Gent_2_score"))

# 56. As an extra benefit of having the data in "long format", it also means 
# we can now also draw a box plot of the Gent vocabulary scores taken at the 
# first and second occasion. Draw the box plot.

ggplot(dat4, aes(x = test, y = score)) + geom_boxplot()

# 57. Get mean and SD of the scores for the tests:

dat5 <- group_by(.data = dat4, test)
summarise(.data=dat5, mean(score), sd(score), n())
# or if you know how to pipe:
group_by(.data = dat4, test) %>% summarise(mean(score), sd(score), n())

# test         `   mean(score)` `sd(score)` `n()`
# <chr>                  <dbl>       <dbl> <int>
# 1 Gent_1_score          58.2        15.2   191
# 2 Gent_2_score          60.2        16.3   191


# 58. Run the t-test:
t.test(score ~ test, paired = TRUE, data = dat4 )

# data:  score by test
# t = -2.7079, df = 190, p-value = 0.007389
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.3663816 -0.5289064
# sample estimates:
#   mean of the differences 
# -1.947644 


# 59. arrange the data as in step 49 so that the responses to the first
# test are listed first, then the responses to the second test.
dat6 <- arrange(.data = dat4, test)

cohensD(score ~ test, method = "paired", data = dat6 )

# 0.1959359



#####################################################
# Part 5: Extras

# 60. In the vocabulary scores data, is there a significant
# difference between males and females for your academic year group? 

# 61. Are there significant differences for the other vocabulary test
# measures between males and females, or between those with 
# English as first or second language?

# 62. For more practice running and interpreting t-tests, look at the
# Glasgow psychology materials:
# https://psyteachr.github.io/quant-fun-v2/t-tests.html
# (ignore anything that says R markdown, you should be able to get to the data
# and follow the instructions for manipulating it and running t-tests.)
