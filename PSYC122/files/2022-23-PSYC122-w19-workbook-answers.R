


############################################################################################################


# In Week 19, we aim to *further* develop skills in working with the linear model

# We focus on the health comprehension project research questions:

# 1. What person attributes predict success in understanding?
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?


# In Week 19, we use data contributed by 2022-23 PSYC122 students to figure out
# our answers to these questions

# We compare the PSYC122 results to the results from analyses of data from a previous 
# similar study to assess the robustness of our results



############################################################################################################
## Part 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to empty the R environment
rm(list=ls())                            


# -- Task 2 -- Run this code to load relevant libraries
library("ggeffects")
library("patchwork")
library("tidyverse")



############################################################################################################
############################################################################################################


# -- In this hworkbook, we use data from two studies of the response to written health information:

# -- 1 -- a study we conducted on the response of adults from a UK national participant sample
# study-two-general-participants.csv

# -- 2 -- the 2022-23 PSYC122 study in which we recorded the responses of PSYC122 students
# 2022-23_PSYC122-subjects.csv

# -- Notice that study-two and PSYC122 participants were given similar sets of health information texts
# to read and respond to



############################################################################################################
## Part 2: Load data #######################################################################################


# -- Task 3 -- Read in the data files we will be using: 
# study-two-general-participants.csv
# 2022-23_PSYC122-subjects.csv

# -- We use the read_csv() function to read the data files into R
study.two <- read_csv("study-two-general-participants.csv")  
study.122 <- read_csv("2022-23_PSYC122-subjects.csv")  


# -- Task 4 -- Inspect the data files
# -- hint: Task 4 -- Use the summary() or head() functions to take a look
summary(study.two)
summary(study.122)

# -- summary() will give you either descriptive statistics for variable columns
# classified as numeric or will tell you that columns in the dataset are not numeric



############################################################################################################
## Part 3: Compare data ####################################################################################


# -- revision: make sure you are confident about doing these things --


# -- Task 5 --  Compare the data distributions from the two different studies


# -- Q.1. -- What is the mean of the mean.acc and SHIPLEY variables in the two
# different studies?
# -- A.1. -- The means are:
# study two -- mean.acc -- mean = 0.7596
# study two -- SHIPLEY -- mean = 35.13
# study PSYC122 -- mean.acc -- mean = 0.7356
# study PSYC122 -- SHIPLEY -- mean = 32.71


# -- Q.2. -- Draw histograms of both mean.acc and mean.self for both studies
# -- A.2. -- You can write the code as you have been shown to do -- see:
# 2022-23-PSYC122-w19-how-to.R

ggplot(data = study.two, aes(x = mean.acc)) + 
  geom_histogram(binwidth = .1) +
  theme_bw() +
  labs(x = "Mean accuracy (mean.acc)", y = "frequency count") +
  xlim(0, 1)

ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0, 40)

ggplot(data = study.122, aes(x = mean.acc)) + 
  geom_histogram(binwidth = .1) +
  theme_bw() +
  labs(x = "Mean accuracy (mean.acc)", y = "frequency count") +
  xlim(0, 1)

ggplot(data = study.122, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0, 40)



# -- introduce: make some new moves --


# -- Task 6 --  Create grids of plots to make the comparison easier to do
# -- hint: Task 6 -- What we are going to do is to create two histograms and then
# present them side by side to allow easy comparison of variable distributions
# -- hint: Task 6 -- We need to make two small changes to the plotting code

# -- Here's an example: run these line of code and see the result in the Plots window in R-Studio

# -- Create plot objects but do not show them:
plot.two <- ggplot(data = study.two, aes(x = mean.acc)) + 
  geom_histogram(binwidth = .1) +
  theme_bw() +
  labs(x = "Mean accuracy (mean.acc)", y = "frequency count", title = "Study Two") +
  xlim(0, 1)

plot.122 <- ggplot(data = study.122, aes(x = mean.acc)) + 
  geom_histogram(binwidth = .1) +
  theme_bw() +
  labs(x = "Mean accuracy (mean.acc)", y = "frequency count", title = "Study PSYC122") +
  xlim(0, 1)

# Now show the plots, side-by-side
plot.two + plot.122

# These are the steps -- you repeat the process for each of two plots:

# -- 1 -- ggplot(...) -- you tell R you want to make a plot using the ggplot() function
# -- 2 -- plot.one <- you tell R you want to give the plot a name; the name appears in the environment
# -- 3 -- ggplot(data = study.two ...) -- you tell R you want to make a plot with the 'study.two' data
# -- 4 -- ggplot(..., aes(x = mean.acc)) -- you tell R that you want to make a plot with the variable 'mean.acc'
# -- 4 -- here, you specify the aesthetic mapping, x = mean.acc
# -- 4 -- + -- and then
# -- 5 -- geom_histogram() -- you tell R you want to plot values of 'mean.acc' as a histogram
# -- 6 -- binwidth = .1 -- you adjust the binwidth to show enough detail but not too much
  # in the distribution
# -- 6 -- + -- and then
# -- 7 -- theme_bw() + -- you tell R what theme you want, adjusting the plot appearance
# -- 8 -- labs(x = "Mean accuracy (mean.acc)", y = "frequency count", title = "Study Two") +
#   -- you fix the x-axis and y-axis labels
#   -- you now add a title for the plot, so you can tell the two plots apart
# -- 9 -- xlim(0, 1) -- you adjust the x-axis limits to show the full range of possible
#   score values on this variable
# -- 10 -- You do this again for the other dataset

# -- and then:
  
# -- 10 -- plot.two + plot.122 -- having constructed -- and named -- both plots, you
# enter their names, separated by a +, to show them in a plot window


# -- warning: until you get to step 10, nothing will appear in the plot window
# -- This is how you construct complex arrays of plots


# -- Task 7 --  Create grids of plots to make the comparison easier to do
# -- Try this out for yourself, focusing now on the distribution of SHIPLEY scores in the
# two studies

# -- Create plot objects but do not show them:
plot.two <- ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count", title = "Study Two") +
  xlim(0, 40)

plot.122 <- ggplot(data = study.122, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count", title = "PSYC122") +
  xlim(0, 40)

# Now show the plots, side-by-side
plot.two + plot.122


# -- Q.3. -- Now use the plots to do some data analysis work: how do the SHIPLEY
# distributions compare, when you compare the SHIPLEY of study.two versus SHIPLEY
# of study.122?
# -- A.3. -- When you compare the plots side-by-side you can see that the
# SHIPLEY distributions are mostly similar: most people have high SHIPLEY scores
# -- But you can also striking differences: 
# -- the peak of the distribution -- where the tallest bar is -- is at a higher
# SHIPLEY score in study.two (around SHIPLEY = 37-38) than in study.122 (where
# is it around SHIPLEY = 30);
# -- there appear to be more participants with lower SHIPLEY scores 
# in study.122 than in study.two

# -- Q.4. -- Is the visual impression you get from comparing the distributions 
# consistent with the statistics you see in the summary?
# -- A.4. -- Yes: If you go back to the summary of SHIPLEY, comparing the two
# studies, then you can see that the median and mean are lower in study.122



############################################################################################################
## Part 4: Now use scatterplots and correlation to examine associations between variables ##################


# -- revision: make sure you are confident about doing these things --


# -- Task 8 -- Draw scatterplots to compare the potential association between mean.acc
# and mean.self in both study.two and study.122 datasets
# -- hint: Task 8 -- The plotting steps are explained in some detail in:
# 2022-23-PSYC122-w17-how-to.R
# -- and you can see example code in:
# 2022-23-PSYC122-w19-how-to.R
  
ggplot(data = study.two, aes(x = mean.self, y = mean.acc)) +
  geom_point(alpha = 0.75, size = 3, colour = "darkgrey") +
  geom_smooth(method = "lm", size = 1.5, colour = "green") +
  theme_bw() +
  labs(x = "mean self-rated accuracy", y = "mean accuracy") +
  xlim(0, 10) + ylim(0, 1)

ggplot(data = study.122, aes(x = mean.self, y = mean.acc)) +
  geom_point(alpha = 0.75, size = 3, colour = "darkgrey") +
  geom_smooth(method = "lm", size = 1.5, colour = "green") +
  theme_bw() +
  labs(x = "mean self-rated accuracy", y = "mean accuracy") +
  xlim(0, 10) + ylim(0, 1)


# -- Task 9 --  Create a grid of plots to make the comparison easier to do
# -- hint: Task 9 -- We follow the same steps as we used in Task 6 to create
# the plots
# -- hint: Task 9 -- We follow similar steps as we used in Task 5 to:
# -- 1 -- name the plots  
# -- 2 -- create and show a grid of named plots

# -- First create the plots, giving them names:
plot.two <- ggplot(data = study.two, aes(x = mean.self, y = mean.acc)) +
  geom_point(alpha = 0.75, size = 3, colour = "darkgrey") +
  geom_smooth(method = "lm", size = 1.5, colour = "green") +
  theme_bw() +
  labs(x = "mean self-rated accuracy", y = "mean accuracy") +
  xlim(0, 10) + ylim(0, 1)

plot.122 <- ggplot(data = study.122, aes(x = mean.self, y = mean.acc)) +
  geom_point(alpha = 0.75, size = 3, colour = "darkgrey") +
  geom_smooth(method = "lm", size = 1.5, colour = "green") +
  theme_bw() +
  labs(x = "mean self-rated accuracy", y = "mean accuracy") +
  xlim(0, 10) + ylim(0, 1)

# -- Second name the plots, to show them side-by-side in the plot window:
plot.two + plot.122


# -- Q.5. -- How does the association, shown in the plots, between mean.self
# and mean.acc compare when you look at the study.two versus the study.122 plot?
# -- hint: Q.5 -- When comparing evidence about associations in different
# studies, we are mostly going to focus on the slope -- the angle --
# of the prediction lines, and the ways in which points do or do not cluster
# about the prediction lines
# -- A.5. -- If you examine the study.two versus the study.122 plots then you 
# can see that in both plots higher mean.self scores appear to be associated with
# higher mean.acc scores
# -- But the trend maybe is a bit stronger -- the line is steeper -- in
# study.two


# -- We are now in a position to answer one of our research questions:
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?

# -- If people *can* accurately evaluate whether they correctly understand written 
# # health information
# -- Then mean.self (a score representing their evaluation) should be associated
# with mean.acc (a score representing their accuracy of understanding)


# -- revision: make sure you are confident about doing these things --


# -- Task 10 --  Can you estimate the association between mean.acc
# and mean.self in both datasets?
# -- hint: Task 10 -- We use cor.test() as you have been shown how to do e.g. in:
# 2022-23-PSYC122-w16-how-to.R
# -- hint: Task 10 -- Do the correlation for both datasets: don't forget to change the dataset name:

# -- First, the correlation between mean.acc and mean.self in study.two
cor.test(study.two$mean.acc, study.two$mean.self, method = "pearson",  alternative = "two.sided")

# -- Q.6. -- What is r, the correlation coefficient?
# -- A.6. -- r = 0.5460792
# -- Q.7. -- Is the correlation significant?
# -- A.7. -- r is significant
# -- Q.8. -- What are the values for t and p for the significance test for the correlation?
# -- A.8. -- t = 8.4991, p = 9.356e-15

# -- Second, the correlation between mean.acc and mean.self in study.122
cor.test(study.122$mean.acc, study.122$mean.self, method = "pearson",  alternative = "two.sided")

# -- Q.9. -- What is r, the correlation coefficient?
# -- A.9. -- r = 0.3232728
# -- Q.10. -- Is the correlation significant?
# -- A.10. -- r is significant
# -- Q.11. -- What are the values for t and p for the significance test for the correlation?
# -- A.11. -- t = 3.2046, p = 0.001884


# -- Now we can answer the research question:
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?


# -- Q.12. -- What do the correlation estimates tell you is the answer to the research question?
# -- A.12. -- The correlations are positive and significant, indicating:
# -- higher mean.self (evaluations) are associated with higher mean.acc (understanding);
# -- suggesting that people *can* judge their accuracy of understanding


# -- Q.13. --  Can you compare the estimates, given the two datasets, to evaluate if
# the result in study.two is replicated in study.122?
# # -- hint: Q.13. -- We can judge if the result in a study is replicated in another
# study by examining if -- here -- the correlation coefficient is significant in both
# studies *and* if the coefficient has the same size and sign in both studies
# -- A.13. -- If you compare the correlation estimates from both study.two and study.122
# you can see:
# -- first, the correlation is significant in both study.two and study.122;
# -- second, the correlation is positive in both studies.
# -- A.13. -- But, if you compare the correlation estimates, you can see that the 
# coefficient estimate is quite a bit smaller in study.122 (where r = .32) than
# in study.two (where r = .55).
# 
# -- This may suggest that the association observed in study.two is different from
# the association in study.122, for some reason.


# -- Task 11 -- In working with R to do data analysis, we often work with libraries of function
# like {tidyverse} that enable us to do things (see the week 19 lecture for discussion).
# -- In this way, we are using the {patchwork} library so that we can create plots
# and then present them in a grid.
# -- Can you find the online information about {patchwork} and use it to adjust the layout of
# the grids of plots you are using?
# -- hint: Task 11 -- To find out more information about a function or a library in R, do a search
# for the keywords: in r ...
# e.g. paste the words in "" into google: "in r patchwork"
# -- hint: Task 11 -- You will then see a list of results including the link to the {patchwork} 
# information:
# https://patchwork.data-imaginist.com  



############################################################################################################
## Part 5: Use a linear model to to answer the research questions -- multiple predictors ###################


# -- revision: make sure you are confident about doing these things --


# -- Task 12 -- Examine the relation between outcome mean accuracy (mean.acc)
# and multiple predictors including:
# health literacy (HLVA); vocabulary (SHIPLEY); and reading strategy (FACTOR3)
# -- hint: Task 10 -- We use lm(), as before, see:
# 2022-23-PSYC122-w18-how-to.R


# -- Task 12 -- Examine the predictors of mean accuracy (mean.acc), first,
# for the study.two data
model <- lm(mean.acc ~ HLVA + SHIPLEY + FACTOR3, data = study.two)
summary(model)


# -- Using the model estimates, we can answer the research question:
# 1. What person attributes predict success in understanding?


# -- If you look at the model summary you can answer the following questions:  

# -- Q.18. -- What is the estimate for the coefficient of the effect of the predictor, 
# 'SHIPLEY', in *this* model?
# -- A.18. -- 0.008397 

# -- Q.19. -- Is the effect significant?
# -- A.19. -- It is significant, p < .05

# -- Q.20. -- What are the values for t and p for the significance test for the coefficient?
# -- A.20. -- t = 4.533, p = 1.1e-05

# -- Q.21. -- Now consider the estimates for all the variables, what do you 
# conclude is the answer to the research question -- given the study.two data:
# 1. What person attributes predict success in understanding?
# -- hint: Q.21. -- Can you report the model and the model fit statistics using the 
# language you have been shown in the week 18 lecture?
# -- A.21. -- We fitted a linear model with mean comprehension accuracy as the 
# outcome and health literacy (HLVA),  reading strategy (FACTOR3), and vocabulary 
# (SHIPLEY) as predictors.
# The model is significant overall, with F(3, 168) = 31.99, p< .001, and 
# explains 35% of variance (adjusted R2 = 0.35).
# Mean accuracy was predicted to be higher given higher scores in health literacy
# (HLVA estimate = .018, t = 4.90, p < .001), vocabulary knowledge (SHIPLEY estimate = .008, 
# t = 4.53, p < .001), and reading strategy (FACTOR3 estimate = .003, t = 2.68, p = .008).


# -- Task 13 -- Examine the predictors of mean accuracy (mean.acc), now,
# for the study.122 data
model <- lm(mean.acc ~ HLVA + SHIPLEY + FACTOR3, data = study.122)
summary(model)


# -- Using the model estimates, we can answer the research question:
# 1. What person attributes predict success in understanding?


# -- If you look at the model summary you can answer the following questions:  

# -- Q.22. -- What is the estimate for the coefficient of the effect of the predictor, 
# 'HLVA', in *this* model?
# -- A.22. -- 0.003300 

# -- Q.23. -- Is the effect significant?
# -- A.23. -- It is *not* significant, p > .05 because p = 0.57050

# -- Q.24. -- What are the values for t and p for the significance test for the coefficient?
# -- A.24. -- t = 0.570, p = 0.57050

# -- Q.25. -- Now consider the estimates for all the variables, what do you 
# conclude is the answer to the research question -- given the study.122 data:
# 1. What person attributes predict success in understanding?
# -- hint: Q.25. -- Can you report the model and the model fit statistics using the 
# language you have been shown in the week 18 lecture?
# -- A.25. -- We fitted a linear model with mean comprehension accuracy as the 
# outcome and health literacy (HLVA),  reading strategy (FACTOR3), and vocabulary 
# (SHIPLEY) as predictors.
# The model is significant overall, with F(3, 86) = 11.29, p< .001, and 
# explains 26% of variance (adjusted R2 = 0.258).
# Mean accuracy was predicted to be higher given higher scores in vocabulary knowledge 
# (SHIPLEY estimate = .011, t = 3.32, p = .001), and reading strategy (FACTOR3 
# estimate = .007, t = 3.26, p = .002). However, the effect of health literacy
# was not significant (HLVA estimate = .003, t = .57, p = .571). 

# -- At this point, we can evaluate the evidence from the PSYC122 sample -- based on your
# responses -- to assess if the patterns, the estimates, we saw previously are repeated
# in analyses of PSYC122 responses


# -- Q.26. -- Are the findings from study.two replicated in study.122?
# # -- hint: Q.26. -- We can judge if the results in an earlier study are replicated in another
# study by examining if -- here -- the linear model estimates are significant in both
# studies *and* if the coefficient estimates have the same size and sign in both studies
# -- A.26. -- If you compare the linear model coefficient estimates from both the study.two and 
# study.122 models, you can see:
# -- first, that the SHIPLEY and FACTOR3 estimates are significant in both study.two and study.122;
# -- second, that the estimates have the same sign -- positive -- in both studies;
# -- but, third, that the HLVA effect is significant in study.two but not in study.122.  
  
# -- This suggests that the attributes -- the set of abilities -- that predict comprehension
# accuracy are similar but not the same in the participants tested for study.two compared
# to study.122 participants.

# -- Q.27. -- How would you describe the outstanding difference between the results of 
# the two studies?
# -- hint: Q.27. -- We can look at the estimates but we can also use the model prediction
# plotting code you used before, see:
# 2022-23-PSYC122-w18-how-to.R
# 2022-23-PSYC122-w19-how-to.R
# -- hint: Q.27. -- Let's focus on comparing the study.two and study.122 estimates for
# the effect of HLVA in both models: we can plot model predictions, for comparison:

# -- First: fit the models -- using different names for the different models
model.two <- lm(mean.acc ~ HLVA + SHIPLEY + FACTOR3, data = study.two)
summary(model.two)
model.122 <- lm(mean.acc ~ HLVA + SHIPLEY + FACTOR3, data = study.122)
summary(model.122)

# -- Second, create prediction plots for the HLVA effect for each model
dat.two <- ggpredict(model.two, "HLVA")
plot.two <- plot(dat.two) + labs(title = "Study Two")
dat.122 <- ggpredict(model.122, "HLVA")
plot.122 <- plot(dat.122) + labs(title = "Study PSYC122")

# -- Third, show the plots side-by-side
plot.two + plot.122

# -- A.27. -- If we compare the estimates for the coefficient of the HLVA effect in
# the study.two and study.122 models we can see that:
# -- 1 -- the health literacy HLVA effect is significant in study.two but not study.122
# -- 2 -- the effect trends positive in both studies
# -- 3 -- the coefficient estimate is much bigger in study.two than in study.122
# -- 4 -- the prediction plots suggest the prediction line slope is steeper
# in study.two
# -- 5 -- the grey shaded area around the trend line (indicating our uncertainty
# about the estimated trend) is much wider for study.two than for study.122, suggesting
# we are much more uncertain about the association for the study.122 data

# -- This suggests that the effect of health literacy is stronger in Study Two: why is that?


# -- Task 14 -- In producing prediction plots, we are using functions from the {ggefects} library.
# Can you locate online information about working with the library functions?
# -- hint: Task 14 -- Try doing a search with the key words: in r ggeffects 
# -- hint: Task 14 -- If you do that, you will see links to the website:
# https://strengejacke.github.io/ggeffects/

# -- Task 15 -- In the {ggeffects} online information, you can see links to practical examples.
# -- Can you use the information under "Practical examples" to adjust the appearance of the
# prediction plots: to make them black and white; to add points?

# -- Create the plots
dat.two <- ggpredict(model.two, "HLVA")
plot.two <- plot(dat.two, colors = "bw", add.data = TRUE) + labs(title = "Study Two")
dat.122 <- ggpredict(model.122, "HLVA")
plot.122 <- plot(dat.122, colors = "bw", add.data = TRUE) + labs(title = "Study PSYC122")
# -- Show the plots
plot.two + plot.122

# -- Q.28. -- Given the information in the adjusted plots, can you explain why we appear
# to be more uncertain about the HLVA effect given the study.122 data?
# -- A.28. -- Adding points allow us to see:
# -- 1 -- There are far fewer observations in the study.122 dataset;
# -- 2 -- The range of HLVA scores is limited in study.122 data -- mostly high scores -- and range
# limits have an impact on estimates of association: see Margriet's lecture on correlation
# and linear model assumptions and issues
  


############################################################################################################

