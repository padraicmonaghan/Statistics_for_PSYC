library(tidyverse)

# TASK 1 - data processing

# read in the csv file

view() # view the data Q3

*MISSING_data_object_name* %>% 
  ggplot() +
  geom_density(aes()) +# you need to EDIT this for Q4
  theme_dark()

# create a new column which is the average time - Q5
*MISSING_data_object_name* <- 
  *MISSING_data_object_name* %>% 
  group_by(*MISSING_IV*) %>% # for each participant...
  mutate(avg_time = mean(time)) # create a new average time from the mean of the time column

view() # view the data

# distribution of average times
*MISSING_data_object_name* %>% 
  ggplot() +
  geom_histogram(aes()) # you need to EDIT this for Q6

# Do we need to filter out outliers? - Q7
# add a filter to keep data within a certain range
*MISSING_data_object_name* <-# do you want to make a new data object or overwrite?
  *MISSING_data_object_name* %>% 
  filter() # you need to EDIT this for Q7

# distribution of average times - Q8
*MISSING_data_object_name* %>% 
  ggplot() +
  geom_histogram(aes()) # you need to EDIT this for Q8

# TASK 2 - conducting related samples t-tests

# calculate the means for the 3 levels of the condition IV
*MISSING_data_object_name* %>%
  group_by(*MISSING_IV*) %>% # you need to EDIT this for Q1
  summarise(stroop_mean = mean(*MISSING_DV*)) # you need to EDIT this for Q1

# filter is used to select two levels of the IV - Q3-5
stroop_comparison <-
  *MISSING_data_object_name* %>%
  filter(condition == "compatible" | condition == "incompatible")

# run the t-test comparing the means of these two levels
t.test(data = stroop_comparison, *MISSING_DV* ~ *MISSING_IV*, paired = TRUE)

# run more tests for comparisons between the other conditions. 


# TASK 3 - summarising and visualisation 

# calculate the means for the 3 levels of the condition IV
*NEW_MISSING_data_object_name* <-# Notice we need to create a new object here 
  *MISSING_data_object_name* %>%
  group_by(*MISSING_IV*) %>% # you need to EDIT this for Q2
  summarise(stroop_mean = mean(*MISSING_DV*),
            stroop_SE = sd(*MISSING_DV*)/sqrt(n())) # you need to EDIT the sd() for Q2

view(*NEW_MISSING_data_object_name*) # if you've done the above step correctly, you should have different means and SEs for each condition

# let's first plot the means 
*NEW_MISSING_data_object_name* %>%
  ggplot(aes()) + # map variables to x and y for Q4
  geom_point(size = 5)

# let's do the same again, but now with error bars - Q5
*NEW_MISSING_data_object_name* %>%
  ggplot(aes()) + # map variables to x and y
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = *MISSING_CALCULATION_HERE*, # edit this for Q5
                    ymax = *MISSING_CALCULATION_HERE*), # edit this for Q5
                width = .2) 





