

############################################################################################################


# In Week 18, we aim to *further* develop skills in working with the linear model

# We do this to learn how to answer research questions like:

# 1. What person attributes predict success in understanding?
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?

# These kinds of research questions can be answered using methods like the linear model

# When we do these analyses, we will need to think about how we report the results:  
# -- we usually need to report information about the kind of model we specify;
# -- and we will need to report the nature of the associations estimated in our model;
# -- we usually need to decide, is the association between the outcome and any one
# predictor significant?
# -- does that association reflect a positive or negative relationship between the
# outcome and that predictor?
# -- are the associations we see in sample data relatively strong or weak?

# We will consolidate and extend learning on data visualization:
# -- Using scatterplots to examine the relationships we may observe or predict
# -- Generating predictions given model estimates of slope coefficients



############################################################################################################


# -- We will take things step-by-step --

# -- I will be explicit about when I will:
# -- introduce -- ask you to do something new;
# -- revise -- where you have started to do things and maybe can use some 
# practice to strengthen skills;
# -- consolidate -- where you have had the chance to practice things before;
# -- extend -- where you can do things that will stretch you -- where you might
# need to do some independent research



############################################################################################################
## Part 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to empty the R environment
rm(list=ls())                            


# -- Task 2 -- Run this code to load relevant libraries
library("ggeffects")
library("tidyverse")



############################################################################################################
############################################################################################################


# -- In this workbook, we use data from a second 2020 study of the response of adults from a UK national
# sample to written health information

# study-two-general-participants.csv



############################################################################################################
## Part 2: Load data #######################################################################################


# -- Task 3 -- Read in the first data file we will be using: 
# study-two-general-participants.csv
# -- hint: Task 3 -- Use the read_csv() function to read the data file into R
study.two <- read_csv("study-two-general-participants.csv")
# -- hint: Task 3 -- When you read the data file in, give the data object you 
# create a clear name e.g. study.two 


# -- Task 4 -- Inspect the data file
# -- hint: Task 4 -- Use the summary() or head() functions to take a look
head(study.two)
summary(study.two)
# -- hint: Task 4 -- Even though you have done this before, you will want to do it 
# again, here, and pay particular attention to what you see, for the numeric variables, 
# and for the variables of class: character



############################################################################################################
## Part 3: Use a linear model to to answer the research questions -- one predictor #########################


# -- revision: practice to strengthen skills --


# -- revision: we start by revising how to use lm() with one predictor --


# -- One of our research questions is:
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?


# -- We can address this question by examining whether someone's rated evaluation
# of their own understanding matches their performance on a test of that
# understanding, and by investigating what variables predict variation in 
# mean self-rated accuracy

# -- Note that ratings of accuracy are ordinal data but that, here, we may choose
# to examine the average of participants' ratings of their own understanding of health
# information to keep things fairly simple

# -- For these data, participants were asked to respond to questions about health 
# information to get 'mean.acc' scores and were asked to rate their own understanding 
# of the same information

# -- If you *can* evaluate your own understanding then ratings of understanding *should*
# be associated with performance on tests of understanding


# -- Task 5 -- Estimate the relation between outcome mean self-rated accuracy ('mean.self') 
# and tested accuracy of understanding ('mean.acc')
# -- hint: Task 5 -- For these data, participants were asked to respond to questions
# about health information to get 'mean.acc' scores and were asked to rate their own 
# understanding of the same information

# -- hint: Task 5 -- We can use lm() to estimate whether the ratings of accuracy
# actually predict the outcome tested accuracy levels
model <- lm(mean.acc ~ mean.self, data = study.two)
summary(model)


# -- Revision: in R analysis code, we write method(outcome ~ predictor) so
# lm(mean.acc ~ mean.self, data = study.two)
# -- gets us an analysis of whether or how 'mean.self' predicts variation in
# outcome 'mean.acc'

  
# -- If you look at the model summary you can answer the following questions  

# -- Q.1. -- What is the estimate for the coefficient of the effect of the predictor 
# 'mean.self' on the outcome 'mean.acc' in this model?
# -- A.1. -- 0.053566 

# -- Q.2. -- Is the effect significant?
# -- A.2. -- It is significant, p < .05

# -- Q.3. -- What are the values for t and p for the significance test for the coefficient?
# -- A.3. -- t = 8.499, p = 9.36e-15

# -- Q.4. -- What do you conclude is the answer to the research question, given the 
# linear model results?
# -- A.4. -- The model slope estimate suggests that higher levels of rated understanding
# can predict higher levels of tested understanding so, yes: it does appear
# that people can evaluate their own understanding

# -- Q.5. -- What is the F-statistic for the regression? Report F, DF and the p-value.
# -- A.5. -- F-statistic: 72.24 on 1 and 170 DF,  p-value: 9.356e-15

# -- Q.6. -- Is the regression significant?
# -- A.6. -- Yes: the regression is significant.

# -- Q.7. -- What is the Adjusted R-squared?
# -- A.7. -- Adjusted R-squared:  0.2941

# -- Q.8. -- Explain in words what this R-squared value indicates?
# -- A.8. -- The R-squared suggests that 30% of outcome variance can be explained 
# by the model



############################################################################################################
## Part 4: Use a linear model to to answer the research questions -- multiple predictors ###################


# -- encounter: make some new moves --


# -- One of our research questions is:
# 2. Can people accurately evaluate whether they correctly understand written health information?


# -- We have already looked at this question by asking whether ratings of understanding
# predict performance on tests of understanding
# -- But there is a problem with that analysis -- it leaves open the question: 
# what actually predicts ratings of understanding?

# -- We can look at that follow-up question, next


# -- Task 6 -- Examine the relation between outcome mean self-rated accuracy ('mean.self') 
# and  multiple predictors including:
# health literacy ('HLVA'); vocabulary ('SHIPLEY'); 'AGE'; reading strategy ('FACTOR3');
# as well as 'mean.acc'
# -- hint: Task 6 -- We use lm(), as before, but now specify each variable listed 
# here by variable name
model <- lm(mean.self ~ HLVA + SHIPLEY + FACTOR3 + AGE + mean.acc, 
            data = study.two)
summary(model)


# -- If you look at the model summary you can answer the following questions  

# -- Q.9. -- What predictors are significant in this model?
# -- A.9. -- Vocabulary ('SHIPLEY'), reading strategy ('FACTOR3'), 'AGE',
# and performance on tests of accuracy of understanding ('mean.acc') all appear 
# to significantly predict variation in mean ratings of understanding ('mean.self')

# -- Q.10. -- What is the estimate for the coefficient of the effect of the predictor, 
# 'mean.acc', in this model?
# -- A.10. -- 4.763278 

# -- Q.11. -- Is the effect significant?
# -- A.11. -- It is significant, p < .05

# -- Q.12. -- What are the values for t and p for the significance test for the coefficient?
# -- A.12. -- t = 6.726, p = 2.69e-10

# -- Q.13. -- What do you conclude is the answer to the follow-up question, 
# what actually predicts ratings of understanding?
# -- A.13. -- Ratings of understanding appear to be predicted by performance on 
# tests of accuracy of understanding, together with variation in age, vocabulary knowledge,
# health literacy and reading strategy



############################################################################################################
## Part 5: Understanding linear model predictions by comparing one outcome-predictor relation ##############


# Next, we focus in on whether 'mean.self' predicts 'mean.acc' or, in reverse,
# whether 'mean.acc' predicts 'mean.self'?

# -- Note that the comparison between these models teaches us something about 
# *what* linear models predict
  

# -- Q.14. -- Why do you think it appears that the slope coefficient estimate is different
# if you compare :
# (1.) the model, mean.acc ~ mean.self, versus 
# (2.) the model, mean.self ~ mean.acc?
# -- hint: Q.14. -- You want to fit two simple models here, using the verbal description
# in the Q.14 wording
model.1 <- lm(mean.acc ~ mean.self, data = study.two)
summary(model.1)

model.2 <- lm(mean.self ~ mean.acc, 
              data = study.two)
summary(model.2)
# -- hint: Q.14. -- You may benefit by reflecting on the lm-intro lecture and
# practical materials, especially where they concern predictions
# -- A.14. -- Linear models are prediction models. We use them to predict variation in
# outcomes given some set of predictor variables. Predictions will necessarily be scaled
# in the same way as the outcome variable. 
# -- So:
# 
# (1.) the model, mean.acc ~ mean.self -- means the outcome is 'mean.acc' so if
# we are predicting change in 'mean.acc' (scaled 0-1) then we are looking at coefficients
# that will lie somewhere on the same scale (also 0-1)
# -- Here: 
# -- the model suggests that unit change in 'mean.self' predicts increase of 0.053566
# in 'mean.acc'
# 
# (2.) the model, mean.self ~ mean.acc -- means the outcome is 'mean.self' so if
# we are predicting change in 'mean.self' (scaled 1-9) then we are looking at coefficients
# that will lie somewhere on the same scale (also 1-9)
# -- Here: 
# -- the model suggests that unit change in 'mean.acc' predicts increase of 5.5670
# in 'mean.self'

# -- Q.15. Can you plot the predictions from each model?
# -- A.15. Here is the code to plot the predictions from both models
# -- hint: A.15. -- First fit the models -- give the model objects distinct names
model.1 <- lm(mean.acc ~ mean.self, data = study.two)
summary(model.1)

model.2 <- lm(mean.self ~ mean.acc, 
            data = study.two)
summary(model.2)
# -- hint: A.15. -- Then get the predictions
dat.1 <- ggpredict(model.1, "mean.self")
dat.2 <- ggpredict(model.2, "mean.acc")
# -- hint: A.15. -- Then make the plots
plot(dat.1)
plot(dat.2)

# -- Q.16. Look at the two plots: what do you see?
# -- hint: Q.16. -- Look at changes in height of the prediction line, given 
# changes in x-axis position of the line
# -- A.16. A side-by-side comparison shows that for (1.) mean.acc ~ mean.self,
# increases in mean.self from about 4-9 are associated with a change in mean.acc
# from about .6 to about .85, while for (2.) mean.self ~ mean.acc,
# increases in mean.acc from about .4-1.0 are associated with a change in mean.self
# from about 5 to about 9



############################################################################################################
## Part 6: Estimate the effects of factors as well as numeric variables ####################################


# -- consolidate: build your skills --


# -- We have not yet included any categorical or nominal variables as predictors
# but we can, and should: lm() can cope with any kind of variable as a predictor

# -- There are different ways to do this, here we ask you to use the R default method:


# -- Task 7 -- Fit a linear model to examine what variables predict outcome
# mean self-rated accuracy of 'mean.self'
# -- Task 7 -- include in the model both numeric variables and categorical 
# variables as predictors:
# health literacy ('HLVA'); vocabulary ('SHIPLEY'); 'AGE'; reading strategy ('FACTOR3');
# as well as 'mean.acc' and 'EDUCATION'
model <- lm(mean.self ~ HLVA + SHIPLEY + FACTOR3 + AGE + mean.acc +
                        EDUCATION, 
            data = study.two)


# -- Q.17. -- Can you report the overall model and model fit statistics?
# -- A.17. -- We fitted a linear model with mean self-rated accuracy as the 
# outcome and with the predictors: health literacy ('HLVA'), vocabulary ('SHIPLEY'),
# reading strategy ('FACTOR3'), 'AGE', as well as mean accuracy ('mean.acc') and 
# education level ('EDUCATION').
# The model is significant overall, with F(7, 164) = 24.38, p < .001, and 
# explains 49% of variance (adjusted R2 = 0.489).


# -- Q.18. -- Can you plot the predicted effect of 'EDUCATION' given your model?
# -- hint: Q.18. -- We first fit the model, including 'EDUCATION'
model <- lm(mean.self ~ HLVA + SHIPLEY + FACTOR3 + AGE + mean.acc +
              EDUCATION, 
            data = study.two)
# then use the ggpredict() function to get the predictions
dat <- ggpredict(model, "EDUCATION")
plot(dat)


# -- Q.19. -- The plot should give you dot-and-whisker representations of the
# estimated 'mean.self' outcome for different levels of 'EDUCATION'.
# What is the difference in the estimated 'mean.self' between the groups?
# -- hint: Q.19. -- The effect or prediction plot will show you dot-and-whisker
# representations of predicted outcome 'mean.self'. In these plots, the dots
# represent the estimated 'mean.self' while the lines (whiskers) represent
# confidence intervals
# -- A.19. -- The difference in the estimated 'mean.self' between these groups
# is small: the groups vary between ratings of about 7, 7.10 and 7.5.

# -- Q.20. -- Compare the difference in the estimated 'mean.self' between these groups,
# given the plot, with the coefficient estimate from the model summary: what do you see?
# -- A.20. -- The effect of 'EDUCATION' is presented in the summary as two estimates:
# EDUCATIONHigher    -0.082217
# EDUCATIONSecondary  0.346161
# -- The reference level for 'EDUCATION' is 'Further'. The estimates therefore show
# that people with 'Higher' education have 'mean.self' scores about -.08 lower than
# 'mean.self' for people with 'Further' education. People with 'Secondary' education
# have 'mean.self' scores about .35 higher than 'mean.self' for people with 'Further' 
# education.



############################################################################################################

