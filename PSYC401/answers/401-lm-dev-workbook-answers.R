


############################################################################################################


# In Week 10, we aim to *further* develop skills in working with the linear model,
# and in visualizing and testing the associations between variables in psychological 
# data


# We do this to learn how to answer research questions like:

# 1. What person attributes predict success in understanding?
# 2. Can people accurately evaluate whether they correctly understand written health information?

# These kinds of research questions can often be answered through analyses using linear models
# -- We will use linear models to estimate the association between predictors and outcomes


# When we do these analyses, we will need to think about how we report the results:  
# -- we usually need to report information about the kind of model we specify;
# -- and we will need to report the nature of the associations estimated in our model;
# -- we usually need to decide, is the association between the outcome and any one
# predictor significant?
# -- does that association reflect a positive or negative relationship between the
# outcome and that predictor?
# -- are the associations we see in sample data relatively strong or weak?


# We will consolidate and extend learning on data visualization:
# -- Use scatterplots to examine the relationships we may observe or predict
# -- Generate predictions given model estimates of slope coefficients



############################################################################################################
## Part 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to empty the R environment
rm(list=ls())                            


# -- Task 2 -- Run this code to load relevant libraries
library("ggeffects")
library("patchwork")
library("psych")
library("tidyverse")



############################################################################################################
############################################################################################################


# -- In this how-to guide, we use a collection of data, drawing together data from
# a series of studies, completed by BSc and MSc students, as replications of the
# clearly-understood health comprehension project investigations

# 2022-12-08_all-studies-subject-scores.csv



############################################################################################################
## Part 2: Load data #######################################################################################


# -- consolidation: we use read_csv() again --
# -- introduce: now we add some extra efficiency --


# -- Task 3 -- Read in the data file we will be using: 
# 2022-12-08_all-studies-subject-scores.csv
# -- hint: Task 3 -- Use the read_csv() function to read the data file into R
# -- hint: Task 3 -- Use the col_types = cols() function to tell R how
# to identify nominal variables as factors within the dataset

# -- We use the read_csv() function to read the data file into R
all.studies.subjects <- read_csv("2022-12-08_all-studies-subject-scores.csv", 
                                 col_types = cols(
                                   ResponseId = col_factor(),
                                   study = col_factor(),
                                   EDUCATION = col_factor(),
                                   GENDER = col_factor(),
                                   ETHNICITY = col_factor(),
                                   NATIVE.LANGUAGE = col_factor()
                                 )
                                )

# Notice what we are doing here:
# 1. all.studies.subjects <- read_csv("2022-12-08_all-studies-subject-scores.csv" ..) 
# -- reads in the dataset we name, the .csv
# 2. col_types = cols(
#   ResponseId = col_factor(),
#   ...
# -- tells R how to handle some of the variables in the dataset;
# -- the variables that are listed -- notice: not all of the variables -- are
# categorical or nominal variables that we want R to recognize as factors.
# 3. ))
# -- We embed one function cols() *inside* another function read_csv() and that means
# at the end we need to *close* the brackets

# See here for further information:
# https://robayedavies.github.io/PSYC401-book/visualization.html#sec-ricketts-process-data  


# -- Task 4 -- Get summary statistics for *only* the numeric variables: AGE, HLVA
# -- hint: Task 4 -- Here, we use the describe() function from the 'psych' library 
# -- hint: Task 4 -- Here, we can do this in two steps: 
# -- 1 -- we select the variables we care about
# -- 2 -- we get just the descriptive statistics we want for those variables
all.studies.subjects %>%
  select(AGE, HLVA) %>%
  describe(skew = FALSE)

# These are the steps:
# -- 1 -- all.studies.subjects %>% 
# -- you tell R to use the 'all.studies.subjects' dataset, 
# -- then use %>% to ask R to take those data to the next step (ie to %>% pipe it)
# -- 2 -- select(AGE, HLVA) %>% 
# -- you tell R to select just those variables in 'all.studies.subjects' that you 
# name and pipe %>% them to the next step
# -- 3 -- describe(...) -- you tell R to give you descriptive statistics for the 
# variables you have selected
# -- 4 -- describe(skew = FALSE) -- critically, you add the argument "skew = FALSE" 
# to turn off the option in describe() to report skew, kurtosis 
# -- because we do not typically see these statistics reported in psychology

# -- Here is an explanation for how to use pipes %>% when you code and why it may be helpful to do so:
# https://r4ds.had.co.nz/pipes.html
# -- Note we are not going to require the use of pipes in PSYC401

# -- Notice: 
# -- we modify here how the function describe() works by adding an argument
# describe(skew = FALSE)

# -- You can get a guide to the 'psych' library here:
# http://personality-project.org/r/psych/vignettes/intro.pdf  
# -- This is the information you see if you ask for help in R for a function e.g.
?describe
# -- or
help(describe)


# -- Q.1. -- What is the mean health literacy (HLVA) score in this sample?
# -- A.1. -- 7.22

# -- Q.2. -- What are the minimum and maximum ages in this sample?
# -- A.2. -- 18-100

# -- Q.3. -- Do you see any reason to be concerned about the data in this sample?
# -- hint: Q.3. -- It is always a good idea to use your visualization skills to 
# examine the distribution of variable values in a dataset
all.studies.subjects %>%
  ggplot(aes(x = AGE)) +
  geom_histogram()
# -- A.3. -- It looks like the AGE of 100 is maybe an error, or an extreme outlier



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
model <- lm(mean.acc ~ mean.self, data = all.studies.subjects)
summary(model)

  
# -- If you look at the model summary you can answer the following questions  

# -- Q.1. -- What is the estimate for the coefficient of the effect of the predictor 
# 'mean.self' on the outcome 'mean.acc' in this modelo?
# -- A.1. -- 0.058613 

# -- Q.2. -- Is the effect significant?
# -- A.2. -- It is significant, p < .05

# -- Q.3. -- What are the values for t and p for the significance test for the coefficient?
# -- A.3. -- t = 12.90, p = <2e-16

# -- Q.4. -- What do you conclude is the answer to the research question, given the 
# linear model results?
# -- A.4. -- The model slope estimate suggests that higher levels of rated understanding
# can predict higher levels of tested understanding so, yes: it does appear
# that people can evaluate their own understanding

# -- Q.5. -- What is the F-statistic for the regression? Report F, DF and the p-value.
# -- A.5. -- F-statistic: 166.4 on 1 and 559 DF,  p-value: < 2.2e-16

# -- Q.6. -- Is the regression significant?
# -- A.6. -- Yes: the regression is significant.

# -- Q.7. -- What is the Adjusted R-squared?
# -- A.7. -- Adjusted R-squared:  0.228

# -- Q.8. -- Explain in words what this R-squared value indicates?
# -- A.8. -- The R-squared suggests that 23% of outcome variance can be explained 
# by the model



############################################################################################################
## Part 5: Use a linear model to to answer the research questions -- multiple predictors ###################


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
            data = all.studies.subjects)
summary(model)


# -- If you look at the model summary you can answer the following questions  

# -- Q.9. -- What predictors are significant in this model?
# -- A.9. -- Health literacy ('HLVA'), reading strategy ('FACTOR3') and performance
# on tests of accuracy of understanding ('mean.acc') all appear to significantly
# predict variation in mean ratings of understanding ('mean.self')

# -- Q.10. -- What is the estimate for the coefficient of the effect of the predictor, 
# 'mean.acc', in this model?
# -- A.10. -- 2.502024 

# -- Q.11. -- Is the effect significant?
# -- A.11. -- It is significant, p < .05

# -- Q.12. -- What are the values for t and p for the significance test for the coefficient?
# -- A.12. -- t = 6.929, p = 1.18e-11

# -- Q.13. -- What do you conclude is the answer to the follow-up question, 
# what actually predicts ratings of understanding?
# -- A.13. -- Ratings of understanding appear to be predicted by performance on 
# tests of accuracy of understanding, together with variation in health literacy and
# reading strategy



############################################################################################################
## Part 6: Understanding linear model predictions by comparing one outcome-predictor relation ##############


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
model.1 <- lm(mean.acc ~ mean.self, data = all.studies.subjects)
summary(model.1)

model.2 <- lm(mean.self ~ mean.acc, 
              data = all.studies.subjects)
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
# -- the model suggests that unit change in 'mean.self' predicts increase of 0.058613
# in 'mean.acc'
# 
# (2.) the model, mean.self ~ mean.acc -- means the outcome is 'mean.self' so if
# we are predicting change in 'mean.self' (scaled 1-9) then we are looking at coefficients
# that will lie somewhere on the same scale (also 1-9)
# -- Here: 
# -- the model suggests that unit change in 'mean.acc' predicts increase of 3.9135
# in 'mean.self'

# -- Q.15. Can you plot the predictions from each model?

# -- A.15. Here is the code to plot the predictions from both models
# -- hint: A.15. -- First fit the models -- give the model objects distinct names
model.1 <- lm(mean.acc ~ mean.self, data = all.studies.subjects)
summary(model.1)

model.2 <- lm(mean.self ~ mean.acc, 
            data = all.studies.subjects)
summary(model.2)
# -- hint: A.15. -- Then get the predictions
dat.1 <- ggpredict(model.1, "mean.self")
dat.2 <- ggpredict(model.2, "mean.acc")
# -- hint: A.15. -- Then make the plots
plot(dat.1)
plot(dat.2)

# -- Q.16. Look at the two plots side-by-side: what do you see?
# -- hint: Q.16. -- Look at changes in height of the prediction line, given 
# changes in x-axis position of the line
# -- A.16. A side-by-side comparison shows that for (1.) mean.acc ~ mean.self,
# increases in mean.self from about 2-8 are associated with a change in mean.acc
# from about .4 to about .85, while for (2.) mean.self ~ mean.acc,
# increases in mean.self from about .2-1.0 are associated with a change in mean.acc
# from about 4 to about 8



############################################################################################################
## Part 7: Estimate the effects of factors as well as numeric variables ####################################


# -- consolidate: build your skills --


# -- We have not yet included any categorical or nominal variables as predictors
# but we can, and should: lm() can cope with any kind of variable as a predictor

# -- There are different ways to do this, here we ask you to use the R default method:


# -- Task 11 -- Fit a linear model to examine what variables predict outcome
# mean self-rated accuracy of 'mean.self'
# -- Task 11 -- include in the model both numeric variables and categorical 
# variables as predictors:
# health literacy ('HLVA'); vocabulary ('SHIPLEY'); 'AGE'; reading strategy ('FACTOR3');
# as well as 'mean.acc' and 'NATIVE.LANGUAGE'
model <- lm(mean.self ~ HLVA + SHIPLEY + FACTOR3 + AGE + mean.acc +
                        NATIVE.LANGUAGE, 
            data = all.studies.subjects)


# -- Q.17. -- Can you report the estimated effect of 'NATIVE.LANGUAGE' (the 
# coding of participant language status: 'English' versus 'other')?
# -- hint: Q.17. -- You will need to get a summary of the model
summary(model)
# -- A.17. -- The effect of language status ('NATIVE.LANGUAGE') on mean accuracy of 
# understanding is not significant (estimate = -0.225432, t = -1.750, p = .081)
# indicating that not being a native speaker of English ('Other') is not associated 
# with lower self-rated accuracy

# -- Q.18. -- Can you report the overall model and model fit statistics?
# -- A.18. -- We fitted a linear model with mean self-rated accuracy as the 
# outcome and with the predictors: health literacy (HLVA),  reading strategy (FACTOR3), 
# vocabulary (SHIPLEY) and AGE (years), as well as mean accuracy (mean.acc) and 
# language status.
# The model is significant overall, with F(6, 554) = 45.82, p < .001, and 
# explains 32% of variance (adjusted R2 = 0.32).


# -- Q.18. -- Can you plot the predicted effect of 'NATIVE.LANGUAGE' given your model?
# -- hint: Q.18. -- We first fit the model, including 'NATIVE.LANGUAGE'
model <- lm(mean.self ~ HLVA + SHIPLEY + FACTOR3 + AGE + mean.acc +
              NATIVE.LANGUAGE, 
            data = all.studies.subjects)
# then use the ggpredict() function to get the predictions
dat <- ggpredict(model, "NATIVE.LANGUAGE")
plot(dat)


# -- Q.19. -- The plot should give you dot-and-whisker representations of the
# estimated 'mean.self' for 'English' versus 'Other' participants in the dataset.
# What is the difference in the estimated 'mean.self' between these groups?
# -- hint: Q.19. -- The effect or prediction plot will show you dot-and-whisker
# representations of predicted outcome 'mean.self'. In these plots, the dots
# represent the estimated 'mean.self' while the lines (whiskers) represent
# confidence intervals
# -- A.19. -- The difference in the estimated 'mean.self' between these groups
# is about .2.

# -- Q.20. -- Compare the difference in the estimated 'mean.self' between these groups,
# given the plot, with the coefficient estimate from the model summary: what do you see?
# -- A.20. -- The coefficient estimate = -0.225432. This matches the difference
# shown in the plot.



############################################################################################################
## Part optional: Examine associations comparing data from different samples ###############################


# -- encounter: make some new moves --


# -- The lecture for developing the linear model includes a discussion of the ways
# in which the observed associations between variables or the estimated effects of 
# predictor variables on some outcome may differ between different studies, different
# samples of data


# -- Task optional -- Change the factor in facet_wrap() to show how the association
# between 'mean.self' and 'mean.acc' can vary between the different studies in the
# dataset
all.studies.subjects %>%
  ggplot(aes(x = mean.self, y = mean.acc)) +
  geom_point(size = 2, alpha = .5, colour = "darkgrey") +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = FALSE) +
  xlim(0, 10) +
  ylim(0, 1.1)+
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.15)),
    axis.title = element_text(size = rel(1.5))
  ) +
  xlab("Mean self-rated accuracy") + ylab("Mean accuracy") +
  facet_wrap(~ study)


# -- You can read more about faceting here:
#   https://ggplot2.tidyverse.org/reference/facet_wrap.html


# -- Task optional -- You may need to edit the x-axis labeling to make it readable
# -- Can you work out how to do that, given ggplot() help information?
# -- hint: Task optional -- check out the continuous scale information in
# https://ggplot2.tidyverse.org/reference/scale_continuous.html



############################################################################################################
## Part Optional: Save or export plots so that you can insert them in reports ##############################


# -- This is something you have not seen before but it is worth exploring because
# it may help you later with reports


# -- Task optional -- Save or export a plot that you produce, so that you can insert 
# it in a report or presentation
# -- hint: Task optional -- There are different ways to do this, we can look at 
# one simple example


# -- We can save the last plot we produce using the tidyverse function ggsave():
ggsave("facet-plots.png")
# -- Notice, here, that:
# -- ggsave("facet-plots...") -- we need to give the plot a name
# -- ggsave("...png") -- and we need to tell R what format we require


# -- The plot is saved as a file with the name you specify, in the working directory you are using


# -- R will save the plot in the format you specify: here, I choose .png because 
# .png image files can be imported into Microsoft Word documents easily
# -- Notice that ggsave() will use pretty good defaults but that you can over-ride 
# the defaults, by adding arguments to specify the plot width, height and resolution 
# (in dpi)


# -- Check out reference information here:
# https://ggplot2.tidyverse.org/reference/ggsave.html
# http://www.cookbook-r.com/Graphs/Output_to_a_file/


# -- Now try it for yourself: make a plot, and save it, using what you have 
# learnt so far
# -- Fit a model:
model <- lm(mean.acc ~ HLVA + SHIPLEY + FACTOR3 + AGE, 
            data = all.studies.subjects)
# -- Create a set of predictions we can use for plotting  
dat <- ggpredict(model, "FACTOR3")
# -- make a plot
p.model <- plot(dat)
p.model +
  geom_point(data = all.studies.subjects, 
             aes(x = FACTOR3, y = mean.acc), size = 1.5, alpha = .75, colour = "darkgrey") +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.15)),
    axis.title = element_text(size = rel(1.25)),
    plot.title = element_text(size = rel(1.4))
  ) +
  xlab("Reading strategy (FACTOR3)") + ylab("Mean accuracy") +
  ggtitle("Effect of reading strategy on \n mean comprehension accuracy")

# -- Save it
ggsave("reading-strategy-prediction.png", width = 10, height = 10, units = "cm")


# -- Some advice:
# -- It is often helpful to present plots that are almost square ie height about = width
# -- This helps to present the best fit line in scatterplots in a way that is easier to perceive
# -- We may experiment with width-height (aspect ratio) until we get something that "looks" right 
# -- Notice also that different presentation venues will require different levels if image
# resolution e.g. journals may require .tiff file plots at dpi = 180 or greater



############################################################################################################

