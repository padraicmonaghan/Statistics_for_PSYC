


############################################################################################################


# In Week 17, we aim to *further* develop skills in visualizing and testing the 
# associations between variables in psychological data

# We do this to learn how to answer research questions like:

# 1. What person attributes predict success in understanding?
# 2. Can people accurately evaluate whether they correctly understand written 
# health information?

# These kinds of research questions can be answered using methods like the linear model

# When we do these analyses, we need to think about how we report the results:  
# -- we usually need to report information about the kind of model we specify;
# -- and we will need to report the nature of the association estimated in our model;
# -- we usually need to decide, is the association significant?
# -- does the association reflect a positive or negative relationship between outcome 
# and predictor?
# -- is the association we see in our sample data relatively strong or weak?

# We will consolidate and extend learning on data visualization:
# -- focusing on how we edit ggplot() code to produce professional looking plots



############################################################################################################


# -- I will put dataset names in quotes like this: 
#   'study-one-general-participants.csv'
# -- I will put variable (data column) names in quotes like this: 'variable' 
# -- And I will put value or other data object (e.g. cell value) names in quotes 
# like this: 'studyone'


# We will take things step-by-step

# -- I will split .R scripts by parts, tasks and questions:  
# -- different parts for different phases of the analysis process;
# -- different tasks for different steps in each phase;
# -- different questions to examine different ideas or coding steps



############################################################################################################
## Part 1: Set-up ##########################################################################################


# -- Task 1 -- Run this code to empty the R environment
rm(list=ls())                            


# -- Task 2 -- Run this code to load relevant libraries
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
# in the information about the minimum (Min.) and maximum (Max.) values for the variables



############################################################################################################
## Part 3: Use histograms to examine the distributions of variables ########################################


# -- revision: practice to strengthen skills --


# -- Task 5 -- Draw histograms to examine the distributions of variables
# -- hint: Task 5 -- Use ggplot() with geom_histogram()

# -- When we create a plot, we take things step-by-step
# -- Here's an example: run the line of code and see the result in the Plots window in R-Studio
ggplot(data = study.two, aes(x = mean.acc)) + 
  geom_histogram()

# These are the steps:
# -- 1 -- ggplot(...) -- you tell R you want to make a plot using the ggplot() function
# -- 2 -- ggplot(data = study.two ...) -- you tell R you want to make a plot with the 
# 'study.two' data
# -- 3 -- ggplot(..., aes(x = mean.acc)) -- you tell R that you want to make a plot with the 
# variable 'mean.acc'
# -- 3 -- here, you specify the aesthetic mapping, x = mean.acc
# -- 3 -- + -- and then
# -- 4 -- geom_histogram() -- you tell R you want to plot values of 'mean.acc' as a histogram


# -- revision: make sure you are confident about doing these things --


# -- Task 6 -- Practice editing the appearance of a histogram plot step-by-step
# -- Start by constructing a basic histogram
# -- hint: Task 6 -- Choose whichever numeric variable from the study.two dataset you please
# -- hint: Task 6 -- Use the line-by-line format to break the plot code into steps
# -- it will make it easier to read, and it will make it easier to add edits e.g.
ggplot(data = study.two, aes(x = SHIPLEY)) + 
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
ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2)

# -- Q.2. -- Then add an edit to the appearance of the background using theme_bw()
ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw()

# -- Q.3. -- Then add an edit to the appearance of the labels using labs()
ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "SHIPLEY", y = "frequency count")


# -- introduce: make some new moves --


# -- Q.4. -- Now add an edit by setting the x-axis limits using x.lim()
ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40)

# -- Q.5. -- Then add an edit to draw a vertical line to show the mean value of 
# the variable you are plotting
ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two$SHIPLEY), colour = "red", size = 1.5)

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


  
############################################################################################################
## Part 4: Now draw scatterplots to examine associations between variables #################################


# -- consolidation: should be no surprises here --


# -- Task 7 -- Create a scatterplot to examine the association between the 
# outcome 'mean.acc' and each of three numeric potential predictor variables 
# 'SHIPLEY' 'HLVA' and 'AGE'
# -- hint: Task 7 -- We are working with geom_point() and you need x and y 
# aesthetic mappings
# -- hint: Task 7 -- The outcome variable 'mean.acc' has to be mapped to the 
# y-axis using "...y = ..."

ggplot(data = study.two, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point()

ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point()

ggplot(data = study.two, aes(x = AGE, y = mean.acc)) +
  geom_point()


# -- revision: make sure you are confident about doing these things --


# -- Task 8 -- Edit the appearance of *each and every* plot step-by-step
# -- hint: Task 8 -- You may want to use the same plot appearance choices for 
# all plots because a consistent appearance is generally neater and easier for 
# your audience to process
# -- hint: Task 8 -- You can find links to reference information on colour and 
# shape options in the how-to guide:
# -- use the information to make the plots pleasing in appearance to you
# -- hint: Task 8 -- Do not be afraid to copy then paste code you re-use
# -- hint: Task 8 -- But be careful that things like axis values are sensible 
# for each variable


# -- Q.8. -- First, edit the appearance of the points using alpha, size, shape, and colour

ggplot(data = study.two, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

ggplot(data = study.two, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')

# -- Q.9. -- Then edit the colour of the background using theme_bw()

ggplot(data = study.two, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

ggplot(data = study.two, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw()

# -- Q.10. -- Then edit the appearance of the labels using labs()

ggplot(data = study.two, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "SHIPLEY", y = "mean accuracy")

ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy")

ggplot(data = study.two, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "Age (Years)", y = "mean accuracy")


# -- introduce: make some new moves --


# -- Q.11. -- Then set the x-axis and y-axis limits to the minimum-maximum ranges 
# of the variables you are plotting
# -- hint: Q.11. -- For these plots the y-axis limits will be the same because 
# the outcome stays the same
# -- hint: Q.11. -- But the x-axis limits will be different for each different 
# predictor variable
# -- hint: Q.11. -- The minimum value will always be 0

ggplot(data = study.two, aes(x = SHIPLEY, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "SHIPLEY", y = "mean accuracy") +
  xlim(0, 40) + ylim(0, 1)

ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy") +
  xlim(0, 15) + ylim(0, 1)

ggplot(data = study.two, aes(x = AGE, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  theme_bw() +
  labs(x = "Age (Years)", y = "mean accuracy") +
  xlim(0, 80) + ylim(0, 1)




############################################################################################################
## Part 5: Use correlation to to answer the research questions #############################################


# -- revision: make sure you are confident about doing these things --


# -- One of our research questions is:
# 1. What person attributes predict success in understanding?


# -- Task 9 -- Examine the correlations between the outcome variable and potential 
# predictor variables
# -- hint: Task 9 -- We use cor.test()
# -- hint: Task 9 -- You can look at: 2022-23-PSYC122-w16-how-to.R -- if you want
# more guidance
# -- hint: Task 9 -- You need to run three separate correlations: 
# (1.) between mean accuracy and SHIPLEY
# (2.) between mean accuracy and HLVA and 
# (3.) between mean accuracy and AGE
cor.test(study.two$SHIPLEY, study.two$mean.acc, method = "pearson",  alternative = "two.sided")
cor.test(study.two$HLVA, study.two$mean.acc, method = "pearson",  alternative = "two.sided")
cor.test(study.two$AGE, study.two$mean.acc, method = "pearson",  alternative = "two.sided")

# -- Q.12. -- What is r, the coefficient for the correlation between 'mean.acc' and 'SHIPLEY'?
# -- A.12. -- r = 0.4650537

# -- Q.13. -- Is the correlation between 'mean.acc' and 'HLVA'  significant?
# -- A.13. -- r is significant, p < .05

# -- Q.14. -- What are the values for t and p for the significance test for the correlation between 
# 'mean.acc' and 'AGE'?
# -- A.14. -- t = 0.30121, p = 0.7636

# -- Q.15. -- For which pair of outcome-predictor variables is the correlation the largest?
# -- A.15. -- The correlation is the largest between 'mean.acc' and 'HLVA'

# -- Q.16. -- What is the sign or direction of each of the correlations?
# -- A.16. -- All the correlations are positive



############################################################################################################
## Part 6: Use a linear model to to answer the research questions ##########################################


# -- introduce: make some new moves --


# -- One of our research questions is:
# 1. What person attributes predict success in understanding?

# -- Task 10 -- Examine the relation between outcome mean accuracy (mean.acc) and 
# each of the predictors: 'SHIPLEY', 'HLVA' and 'AGE'
# -- hint: Task 10 -- Use lm()
# -- hint: Task 10 -- Run three separate lm() analyses, all with 'mean.acc' as the 
# outcome but each with one predictor variable
# -- hint: Task 10 -- See the how-to for example code that does what you need to do

model <- lm(mean.acc ~ SHIPLEY, data = study.two)
summary(model)

model <- lm(mean.acc ~ HLVA, data = study.two)
summary(model)

model <- lm(mean.acc ~ AGE, data = study.two)
summary(model)


# -- Notice that we do the linear model in the steps:
# -- 1 -- model <- lm(...) -- fit the model using lm(...), and give the model a name 
# -- here, we call it "model"
# -- 2 -- ...lm(mean.acc ~ SHIPLEY...) -- tell R you want a model of 
# the outcome 'mean.acc' predicted (~) by the predictor 'SHIPLEY'
# -- 3 -- ...data = study.two) -- tell R that the variables you name in the formula live in the 
# 'study.two' dataset
# -- 4 -- summary(model) -- ask R for a summary of the model you called "model"

# -- Notice that R has a general formula syntax: outcome ~ predictor *or* y ~ x
# -- and uses the same format across a number of different functions
# -- each time, the left of the tilde symbol ~ is some output or outcome
# -- and the right of the tilde ~ is some input or predictor or set of predictors

  
# -- If you look at the model summary you can answer the following questions  

# -- Q.17. -- What is the estimate for the coefficient of the effect of the predictor 
# 'HLVA' on 'mean.acc'?
# -- A.17. -- 0.026207 

# -- Q.18. -- Is the effect significant?
# -- A.18. -- It is significant, p < .05

# -- Q.19. -- What are the values for t and p for the significance test for the coefficient?
# -- A.19. -- t = 7.529, p = 2.87e-12

# -- Q.20. -- How would you describe in words the shape or direction of the association 
# between 'HLVA' and 'mean.acc'?
# -- A.20. -- The slope coefficient -- and a scatterplot (draw it) -- suggest that 
# as HLVA scores increase so also do mean accuracy scores

# -- Q.21. -- How how would you describe the relations apparent between the predictor and 
# outcome in all three models?
# -- A.21. -- It is possible to see, given coefficient estimates, that the association 
# between predictor and outcome is positive for each model: mean accuracy appears to increase 
# for increasing values of SHIPLEY vocabulary, HLVA health literacy, and age



############################################################################################################
## Part 7: Use a linear model to generate predictions ######################################################


# -- introduce: make some new moves --


# -- Task 11 -- We can use the model we have just fitted to plot the model predictions
# -- hint: Task 11 -- We are going to draw a scatterplot and add a line showing the 
# predictions, given the model intercept and effect coefficient estimates


# -- First fit a model and get a summary: model the relationship betweeen 'mean.acc' and 'HLVA'
model <- lm(mean.acc ~ HLVA, data = study.two)
summary(model)


# -- Q.22. -- What is the coefficient estimate for the intercept?
# -- A.22. -- 0.522016

# -- Q.23. -- What is the coefficient estimate for the slope of HLVA (see earlier)?
# -- A.23. -- 0.026207


# -- Second, use the geom_point() to draw a scatterplot and geom_abline() function 
# to draw the prediction line representing the association between this outcome and predictor
ggplot(data = study.two, aes(x = HLVA, y = mean.acc)) +
  geom_point(alpha = 0.5, size = 2, colour = "blue", shape = 'square')   +
  geom_abline(intercept = 0.522016, slope = 0.026207, colour = "red", size = 1.5) +
  theme_bw() +
  labs(x = "HLVA", y = "mean accuracy") +
  xlim(0, 15) + ylim(0, 1)

# -- See reference information here:  
#   https://ggplot2.tidyverse.org/reference/geom_abline.html  



############################################################################################################
## Part 8: Optional extension -- Use community information to figure things out ############################


# -- Task 12 -- Experienced users of R know that they can take advantage of the vast 
# community resources that people post online, for free, to help others: 
# -- use the ggplot reference online line information to work out how to draw a 
# horizontal line in your plot

# -- hint: Task 12 -- In the how-to guide, you can see an example of how to draw a 
# vertical line using geom_vline() where you set the position of the line by specifying 
# an x-axis value
# -- hint: Task 12 -- Here, you will need to look at the ggplot online information 
# on drawing reference lines to find a code example for drawing a horizontal line
# -- hint: Task 12 -- You will need to specify, now, a y-axis position and that 
# position will need to make sense, given how the heights of the bars vary, for the 
# variable you are using, in the plot you are developing e.g. something like
# -- hint: Task 12 -- You will need to add a line of code, and "make sense" means: 
# choose a number for the y-axis location of the line that
# is larger than the minimum and smaller than the maximum of the heights of the bars in the plot

ggplot(data = study.two, aes(x = SHIPLEY)) + 
  geom_histogram(binwidth = 2) +
  theme_bw() +
  labs(x = "Vocabulary (SHIPLEY)", y = "frequency count") +
  xlim(0,40) +
  geom_vline(xintercept = mean(study.two$SHIPLEY), colour = "red", size = 1.5) +
  geom_hline(yintercept = 20, colour = "red", size = 1.5)



############################################################################################################


