


############################################################################################################


# In Week 9, we aim to *further* develop skills in visualizing and testing the 
# associations between variables in psychological data
# -- In weeks 9-10, we do this in the context of working with the linear model


# We do this to learn how to answer research questions like:

# 1. What person attributes predict success in understanding?
# 2. Can people accurately evaluate whether they correctly understand written health information?

# These kinds of research questions can often be answered through analyses using linear models
# -- We will use linear models to estimate the association between predictors and outcomes


# When we do these analyses, we will need to think about how we report the results:  
# -- we usually need to report information about the kind of model we specify;
# -- and we will need to report the nature of the association estimated in our model;
# -- we usually need to decide, is the association significant?
# -- does the association reflect a positive or negative relationship between outcome 
# and predictor?
# -- is the association we see in our sample data relatively strong or weak?


# We will consolidate and extend learning on data visualization:
# -- Use histograms to examine the distributions of variables
# -- Develop our capacity to edit the histograms
# -- Use scatterplots to examine the relationships we may observe or predict
# -- Develop our capacity to edit the scatterplots

# The idea is that as we work, we will develop skills in producing appealing plots 
# for professional audiences 


############################################################################################################


# -- A note about terms --

# -- I will put dataset names in quotes like this: 
#   'study-one-general-participants.csv'
# -- I will put variable (data column) names in quotes like this: 'variable' 
# -- And I will put value or other data object (e.g. cell value) names in quotes like this: 'studyone'

# -- This is to help you distinguish between the ordinary use of words like variable
# from the special use of these words when they are parts of datasets.


# -- A note about coding --

# -- I will structure the code in the practical workbooks in a way that is designed to help you to keep clear 
# what you are doing, even as you increase the power of the plots you produce



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
library("patchwork")
library("tidyverse")



############################################################################################################
############################################################################################################


# -- In this workbook, we use data from a second 2020 study of the response of adults from a UK national
# sample to written health information

# study-two-general-participants.csv



############################################################################################################
## Part 2: Load data #######################################################################################


# -- introduce: do something new --


# -- Task 3 -- Read in the first data file we will be using: 
# study-two-general-participants.csv
# -- hint: Task 3 -- Use the read_csv() function to read the data file into R
study.two.gen <- read_csv("study-two-general-participants.csv")
# -- hint: Task 3 -- When you read the data file in, give the data object you create a clear name 
# e.g. study.two.gen 


# -- Notice the difference, here: read_csv()
# -- read_csv() and read.csv() work in similar ways

# -- You can check that this is true by uncommenting and running:
# study.two.gen.compare <- read.csv("study-two-general-participants.csv")
# -- then inspecting the dataset using head() or summary()

# The reasons for changing the read command are:
# -- read_csv() is faster and more efficient, this will help when you work with
# bigger files;
# -- read_csv() can be combined with other commands to pre-process data for
# analysis -- we will use this next week;
# -- read_csv() is the modern tidyverse version of the command.

# But if you do not like it, you can stick to read.csv()

# -- I discuss read_csv() here:
# https://robayedavies.github.io/PSYC401-book/visualization.html#sec-ricketts-process-data
# -- You can read the technical information about the function here:
# https://readr.tidyverse.org/reference/read_delim.html


# -- consolidate: there should be no surprises here --


# -- Task 4 -- Inspect the data file
# -- hint: Task 4 -- Use the summary() or head() functions to take a look
head(study.two.gen)
summary(study.two.gen)
# -- hint: Task 4 -- Even though you have done this before, you will want to do it again, here, and
# pay particular attention to what you see, for the numeric variables, in the information about the
# minimum (Min.) and maximum (Max.) values for the variables



############################################################################################################
## Part 3: Use histograms to examine the distributions of variables ########################################


# -- revision: practice to strengthen skills --


# -- Task 5 -- Draw histograms to examine the distributions of variables
# -- hint: Task 5 -- Use ggplot() with geom_histogram()

# -- When we create a plot, we take things step-by-step
# -- Here's an example: run the line of code and see the result in the Plots window in R-Studio
ggplot(data = study.two.gen, aes(x = mean.acc)) + geom_histogram()

# These are the steps:
# -- 1 -- ggplot(...) -- you tell R you want to make a plot using the ggplot() function
# -- 2 -- ggplot(data = study.two.gen ...) -- you tell R you want to make a plot with 
# the 'study.two.gen' data
# -- 3 -- ggplot(..., aes(x = mean.acc)) -- you tell R that you want to make a plot with 
# the variable 'mean.acc'
# -- 3 -- here, you specify the aesthetic mapping, x = mean.acc
# -- 4 -- ggplot(...) + geom_histogram() -- you tell R you want to plot values of 
# 'mean.acc' as a histogram


# -- revision: make sure you are confident about doing these things --


# -- Task 6 -- Practice editing the appearance of the plot step-by-step
# -- hint: Task 6 -- Choose whichever numeric variable from the study.two.gen dataset you please
# -- hint: Task 6 -- Use the line-by-line format to break the plot code into steps
# -- it will make it easier to read, and it will make it easier to add edits e.g.
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram()

# -- We are going to revise editing:
# -- 1 -- the appearance of the bars using binwidth
# -- 2 -- the colour of the background using theme_bw()
# -- 3 -- the appearance of the labels using labs()

# -- Then we are going to try some new moves:
# -- 4 -- setting the x-axis limits to reflect the full range of possible scores 
# on the x-axis variable
# -- 5 -- add annotation -- here, a vertical line indicating the sample average for a variable

# -- Q.1. -- Edit the appearance of the bars by specifying a binwidth value
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2)

# -- Q.2. -- Then add an edit to the appearance of the background using theme_bw()
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw()

# -- Q.3. -- Then add an edit to the appearance of the labels using labs()
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "SHIPLEY", y = "frequency count")


# -- introduce: make some new moves --


# -- Q.4. -- Now add an edit by setting the x-axis limits using x.lim()
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40)

# -- Q.5. -- Then add an edit to draw a vertical line to show the mean value of 
# the variable you are plotting
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two.gen$SHIPLEY), colour = "red", size = 1.5)

# -- Q.6. -- Can you find information on how to define the limits on the x-axis 
# and on the y-axis?
# -- hint: Q.6. -- You can see the information in this week's how-to but try a 
# search online for "ggplot reference xlim"
# -- A.6. -- See ggplot reference information on setting limits here:
# https://ggplot2.tidyverse.org/reference/lims.html

# -- Q.7. -- Can you find information on how to a reference line?
# -- hint: Q.7. -- You can see the information in this week's how-to but try a 
# search online for "ggplot reference vline"
# -- A.7. -- See ggplot reference information on setting limits here:
# https://ggplot2.tidyverse.org/reference/geom_abline.html


# -- revision: make sure you are confident about doing these things --


# -- Task 7 -- Does the binwidth value you are using for the histogram help you 
# to see information about the distribution of values in the variable i.e. the 
# general pattern (where is there a peak,  what skew etc.)?
# -- hint: Task 7 -- You probably need to try different values: the final value 
# may be subjective, depending on your impression  

# -- Here: binwidth = 2 for 'SHIPLEY'
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two.gen$SHIPLEY), colour = "red", size = 1.5)

# -- Here: binwidth = 5 for 'SHIPLEY'
ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 5) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two.gen$SHIPLEY), colour = "red", size = 1.5)

# -- I think a binwidth of 2 makes more sense if you care about detailed differences
# i.e. between people who score 20 versus 22 on the test



############################################################################################################
## Part 4: Use grids of histograms to examine the distributions of variables ###############################


# -- introduce: make some new moves --


# -- One of the convenient and powerful things about R plotting code is that you can create a series
# of plots and put them together in a grid of plots for east comparison: we do that here
# -- We will use the patchwork library: check it out
# https://patchwork.data-imaginist.com/articles/patchwork.html


# -- Task 8 -- Create multiple plots showing the distributions of different variables 
# and then plot them together in a grid to allow comparison between variables
# -- hint: Task 8 -- We will need to do things in stages: 
# (1.) make plots 
# (2.) put them together


# -- Q.8. -- Can you? (1.) draw a histogram plot to show the distributions of the 
# variables in the 'study.two.gen' dataset 'SHIPLEY' 'HLVA' and 'AGE' 
# (2.) assign a name to each plot object you create using <-
# -- hint: Q.8. -- See example code in the how-to for how to do this
# -- hint: Q.8. -- Give each plot a different name e.g. for the histogram for 
# SHIPLEY scores, you could use the name p.shipley (p. for plot)
# -- hint: Q.8. -- When you create plots like this, they *will not* appear at first 
# in the R-Studio Plots window, that happens when you do Q.9.

# -- Make some plots: each time, assign the plot object a name, like this:

p.shipley <- ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count")   

p.HLVA <- ggplot(data = study.two.gen, aes(x = HLVA)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "HLVA", y = "frequency count") 

p.age <- ggplot(data = study.two.gen, aes(x = AGE)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Age (years)", y = "frequency count") 

# -- Notice the steps:
# -- 1 -- p.... <- ggplot(...) -- create a plot using ggplot(), give the plot a name
# -- 2 -- ...x = ... -- define the x-axis variable (aesthetic) mapping
# -- 3 -- ...binwidth = ... -- you may need to adjust the binwidth for each variable 
# -- 4 -- ...labs(x = )... -- and adjust the labels

# -- Q.9. -- Can you now put the plots together in a grid for comparison?
# -- hint: Q.8. -- See example code in the how-to for how to do this: you need to add the plots together
# using the names you gave them
p.shipley + p.HLVA + p.age


# -- For this to work well, the plot window needs to be wide

# -- Notice that presenting grids of plots is a powerful way to enable your audience 
# to do efficient comparisons between variables or relationships
# -- It is a common tactic in data visualization -- in business, research and 
# journalism -- and is discussed in terms of visualizations with names like grids, 
# lattices, trellises or small multiples

# -- Notice also that we are doing something different here:
# -- we create plot objects but do not show or print the plots out
# -- in a separate move, we put the plot objects together in a grid of plots and show *that*

# -- This is a very powerful aspect of coding plots in R because it means you can 
# create complex plots with multiple elements to communicate the message you need to 
# communicate to your audience
# -- Here, we are using this capacity to allow the audience to view and inspect 
# the distributions of multiple different variables at the same time


  
############################################################################################################
## Part 5: Now draw scatterplots to examine associations between variables #################################


# -- consolidation: should be no surprises here --


# -- Task 9 -- Create a scatterplot to examine the association between the 
# outcome 'mean.acc' and each of three numeric potential predictor variables 
# 'SHIPLEY' 'HLVA' and 'AGE'
# -- hint: Task 9 -- We are working with geom_point() and you need x and y 
# aesthetic mappings
# -- hint: Task 9 -- The outcome variable 'mean.acc' has to be mapped to the 
# y-axis using "...y = ..."

ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point()

ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point()

ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point()


# -- revision: make sure you are confident about doing these things --


# -- Task 10 -- Edit the appearance of *each and every* plot step-by-step
# -- hint: Task 10 -- You may want to use the same plot appearance choices for 
# all plots because a consistent appearance is generally neater and easier for 
# your audience to process
# -- hint: Task 10 -- You can find links to reference information on colour and 
# shape options in the how-to guide:
# -- use the information to make the plots pleasing in appearance to you
# -- hint: Task 10 -- Do not be afraid to copy then paste code you re-use
# -- hint: Task 10 -- But be careful that things like axis values are sensible 
# for each variable


# -- Q.10. -- First, edit the appearance of the points using alpha, size, shape, and colour

ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

# -- Q.11. -- Then edit the colour of the background using theme_bw()

ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

# -- Q.12. -- Then edit the appearance of the labels using labs()

ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "SHIPLEY", y = "mean accuracy")

ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy")

ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "Age (Years)", y = "mean accuracy")


# -- introduce: make some new moves --


# -- Q.13. -- Then set the x-axis and y-axis limits to the minimum-maximum ranges 
# of the variables you are plotting
# -- hint: Q.13. -- For these plots the y-axis limits will be the same because 
# the outcome stays the same
# -- hint: Q.13. -- But the x-axis limits will be different for each different 
# predictor variable
# -- hint: Q.13. -- The minimum value will always be 0

ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "SHIPLEY", y = "mean accuracy") +
  xlim(0, 40) + ylim(0, 1)

ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy") +
  xlim(0, 15) + ylim(0, 1)

ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "Age (Years)", y = "mean accuracy") +
  xlim(0, 80) + ylim(0, 1)


# -- revision: make sure you are confident about doing these things --


# -- Task 11 -- Make a grid of scatterplots so that you can compare the association 
# between predictor and outcome variables for the different predictor variables you 
# have been examining
# -- hint: Task 11 -- As you did before with the histograms, first create a set 
# of scatterplot objects, then produce a grid of the plots, adding each plot by name

p.shipley <- ggplot(data = study.two.gen, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "SHIPLEY", y = "mean accuracy") +
  xlim(0, 40) + ylim(0, 1)

p.hlva <- ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy") +
  xlim(0, 15) + ylim(0, 1)

p.age <- ggplot(data = study.two.gen, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "Age (Years)", y = "mean accuracy") +
  xlim(0, 80) + ylim(0, 1)

p.shipley + p.hlva + p.age


# -- Task 12 -- Now you have a grid of plots to look at: use it to compare the 
# apparent association between the predictor and outcome variables under consideration here
# -- hint: Task 12 -- I am going to ask some questions, next, that rely on your 
# subjective impression given what you see in the plots
# -- Do not worry that there is no "right" answer here
# -- What we are doing is helping you to build experience so that you can develop 
# an intuitive sense of patterns


# -- Q.14. -- Which predictor variables do or do not show an identifiable association 
# with the outcome?
# -- A.14. -- An association is apparent between the outcome and SHIPLEY or the
# outcome and HLVA scores but not between the outcome and AGE

# -- Q.15. -- What do you see that leads you to this conclusion?
# -- A.15. -- The scatter of points appears to trend upwards for the outcome and 
# SHIPLEY or the outcome and  HLVA scores only. It is hard to identify a pattern for 
# the outcome and AGE

# -- Q.16. -- Refer back to the histograms you produced earlier: is there anything 
# salient about the distributions you plotted that may be relevant, here, to your 
# understanding of why you may or may not be able to identify an association between 
# the outcome and any one of the predictor variables?
# -- A.16. -- If you look at the histogram showing the distribution of sample ages 
# then you can see that most people in the sample were quite young: there were few 
# older participants. Maybe this makes it harder to identify a relationship between 
# ages and mean accuracy
  


############################################################################################################
## Part 6: Use correlation to to answer the research questions #############################################


# -- revision: make sure you are confident about doing these things --


# -- One of our research questions is:
# 1. What person attributes predict success in understanding?


# -- Task 13 -- Examine the correlations between the outcome variable and potential 
# predictor variables
# -- We have been drawing scatterplots to examine the associations between the outcome 
# 'mean.acc' and each of the predictor variables 'SHIPLEY', 'HLVA' and 'AGE'
# -- we will look at these variables 'SHIPLEY', 'HLVA' and 'AGE'
# -- hint: Task 13 -- We use cor.test()
# -- hint: Task 13 -- You need to run three separate correlations: 
# (1.) between mean accuracy and SHIPLEY
# (2.) between mean accuracy and HLVA and 
# (3.) between mean accuracy and AGE
cor.test(study.two.gen$SHIPLEY, study.two.gen$mean.acc, method = "pearson",  alternative = "two.sided")
cor.test(study.two.gen$HLVA, study.two.gen$mean.acc, method = "pearson",  alternative = "two.sided")
cor.test(study.two.gen$AGE, study.two.gen$mean.acc, method = "pearson",  alternative = "two.sided")

# -- Q.17. -- What is r, the coefficient for the correlation between 'mean.acc' and 'SHIPLEY'?
# -- A.17. -- r = 0.4650537

# -- Q.18. -- Is the correlation between 'mean.acc' and 'HLVA'  significant?
# -- A.18. -- r is significant, p < .05

# -- Q.19. -- What are the values for t and p for the significance test for the correlation between 
# 'mean.acc' and 'AGE'?
# -- A.19. -- t = 0.30121, p = 0.7636

# -- Q.20. -- For which pair of outcome-predictor variables is the correlation the largest?
# -- A.20. -- The correlation is the largest between 'mean.acc' and 'HLVA'

# -- Q.21. -- What is the sign or direction of each of the correlations?
# -- A.21. -- All the correlations are positive

# -- Q.22. -- Some correlations will be larger than others: using what you see in the 
# scatterplots, and what you saw in the histograms, can you offer an explanation 
# -- in terms of the distribution of values, or the apparent pattern of scatterplot 
# points for why some correlation coefficients are larger than others?
# -- hint: Q.22. -- Here, we are again seeking to train your intuitions
# -- A.22. -- It can be seen, by comparing the plots, that:
# -- Both the SHIPLEY and the HLVA plots show more definite trends in their associations 
# with the outcome than is apparent for AGE
# -- The range of the variation in SHIPLEY scores is quite restricted across the sample 
# compared to the range of the variation in HLVA scores



############################################################################################################
## Part 7: Use a linear model to to answer the research questions ##########################################


# -- introduce: make some new moves --


# -- One of our research questions is:
# 1. What person attributes predict success in understanding?

# -- Task 14 -- Examine the relation between outcome mean accuracy (mean.acc) and 
# each of the predictors: 'SHIPLEY', 'HLVA' and 'AGE'
# -- hint: Task 14 -- Use lm()
# -- hint: Task 14 -- Run three separate lm() analyses, all with 'mean.acc' as the 
# outcome but each with one predictor variable
# -- hint: Task 14 -- See the how-to for example code that does what you need to do

model <- lm(mean.acc ~ SHIPLEY, data = study.two.gen)
summary(model)

model <- lm(mean.acc ~ HLVA, data = study.two.gen)
summary(model)

model <- lm(mean.acc ~ AGE, data = study.two.gen)
summary(model)


# -- Notice that we do the linear model in the steps:
# -- 1 -- model <- lm(...) -- fit the model using lm(...), give the model a name 
# -- here, we call it "model"
# -- 2 -- ...lm(mean.acc ~ SHIPLEY...) -- tell R you want a model of 
# the outcome 'mean.acc' predicted (~) by the predictor 'SHIPLEY'
# -- 3 -- ...data = study.two.gen) -- tell R that the variables you name in the formula live in the 
# 'study.two.gen' dataset
# -- 4 -- summary(model) -- ask R for a summary of the model you called "model"

# -- Notice that R has a general formula syntax: outcome ~ predictor *or* y ~ x
# -- and uses the same format across a number of different functions
# -- each time, the left of the tilde symbol ~ is some output or outcome
# -- and the right of the tilde ~ is some input or predictor or set of predictors

  
# -- If you look at the model summary you can answer the following questions  

# -- Q.23. -- What is the estimate for the coefficient of the effect of the predictor 
# 'HLVA' on 'mean.acc'?
# -- A.23. -- 0.026207 

# -- Q.24. -- Is the effect significant?
# -- A.24. -- It is significant, p < .05

# -- Q.25. -- What are the values for t and p for the significance test for the coefficient?
# -- A.26. -- t = 7.529, p = 2.87e-12

# -- Q.27. -- How would you describe in words the shape or direction of the association 
# between 'HLVA' and 'mean.acc'?
# -- A.27. -- The slope coefficient -- and the scatterplots -- suggest that as HLVA scores 
# increase so also do mean accuracy scores

# -- Q.28. -- How how would you describe the relations apparent between the predictor and 
# outcome in all three models?
# -- A.28. -- It is possible to see, given coefficient estimates, that the association 
# between predictor and outcome is positive for each model: mean accuracy appears to increase 
# for increasing values of SHIPLEY vocabulary, HLVA health literacy, and age



############################################################################################################
## Part 8: Use a linear model to generate predictions ######################################################


# -- introduce: make some new moves --


# -- Task 15 -- We can use the model we have just fitted to plot the model predictions
# -- hint: Task 15 -- We are going to draw a scatterplot and add a line showing the 
# predictions, given the model intercept and effect coefficient estimates


# -- First fit a model and get a summary: model the relationship betweeen 'mean.acc' and 'HLVA'
model <- lm(mean.acc ~ HLVA, data = study.two.gen)
summary(model)


# -- Q.29. -- What is the coefficient estimate for the intercept?
# -- A.29. -- 0.522016

# -- Q.30. -- What is the coefficient estimate for the slope of HLVA (see earlier)?
# -- A.30. -- 0.026207


# -- Second, use the geom_point() to draw a scatterplot and geom_abline() function 
# to draw the prediction line representing the association between this outcome and predictor
ggplot(data = study.two.gen, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  geom_abline(intercept = 0.522016, slope = 0.026207, colour = "red", size = 1.5) +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy") +
  xlim(0, 15) + ylim(0, 1)

# -- See reference information here:  
#   https://ggplot2.tidyverse.org/reference/geom_abline.html  



############################################################################################################
## Part 9: Optional extension -- Use community information to figure things out ############################


# -- Task 16 -- Experienced users of R know that they can take advantage of the vast 
# community resources that people post online, for free, to help others: use the ggplot 
# reference line information you found in answer to Q.7 to work out how to draw a 
# horizontal line in your plot
# -- hint: Task 16 -- In the how-to guide, you can see an example of how to draw a 
# vertical line using geom_vline() where you set the position of the line by specifying 
# an x-axis value
# -- hint: Task 16 -- Here, you will need to look at the ggplot online information 
# on drawing reference lines to find a code example for drawing a horizontal line
# -- hint: Task 16 -- You will need to specify, now, a y-axis position and that 
# position will need to make sense, given how the heights of the bars vary, for the 
# variable you are using, in the plot you are developing e.g. something like
# -- hint: Task 16 -- You will need to add a line of code, and "make sense" means: 
# choose a number for the y-axis location of the line that
# is larger than the minimum and smaller than the maximum of the heights of the bars in the plot

ggplot(data = study.two.gen, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two.gen$SHIPLEY), colour = "red", size = 1.5) +
  geom_hline(yintercept = 20)



############################################################################################################


