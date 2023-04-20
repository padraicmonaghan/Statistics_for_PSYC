library(tidyverse) 
options(scipen = 999) # this stops numbers appearing as scientific notation

# TASK 1

# read in the data with read_csv - Q1

# quick look at the salary estimates
*MISSING_data_set_name* %>% 
  ggplot(aes(*MISSING_DV*))) + 
  geom_histogram() # EDIT THIS CODE FOR Q2

# let's look at those unusual values
*MISSING_data_set_name* %>% 
  arrange(desc(*MISSING_DV*)) # arrange the data according to descending values of uk_salary estimates

# let's remove those really high values
*MISSING_data_set_name* %>% # with the data...
  filter(*MISSING*) # EDIT THIS CODE FOR Q4 





# TASK 2

# What is the mean estimate? 
mean(*MISSING_data_set_name*$uk_salary) # EDIT THIS CODE FOR Q8
sd() # you might also calculate the standard deviation

# Let's check if the mean estimate is significantly different from the true salary
t.test(x = *MISSING_data_set_name*$uk_salary, mu = 30000) # You will need to edit this for Q9

# EXTRA
# this is a "by hand" t statistic - you can add the column of salary estimates here and check with the result of t.test
# check with staff if you're not sure
t_val <- (mean(*MISSING_data_set_name*$uk_salary) - 30000) / (sd(*MISSING_data_set_name*$uk_salary)/sqrt(length(*MISSING_data_set_name*$uk_salary))) # denominator

# TASK 3

# more filtering practice

*MISSING_data_set_name* %>% # change this to your data object
  filter(home_location_in_UK == "NW" & sibling_order == "oldest")

# add more filter commands for Q12-17