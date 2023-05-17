library(tidyverse)
options(scipen = 999) # this stops numbers appearing as scientific notation

# TASK 1

# read in the data
data_w7 <- read_csv("salary_data_wk7.csv")

# quick look at the salary estimates
data_w7 %>% 
  ggplot() + 
  geom_histogram(aes(x = uk_salary),
                 bins = 50) # EDIT THIS CODE FOR Q2

# let's look at those unusual values
data_w7 %>% 
  arrange(desc(uk_salary)) # arrange the data according to descending values of uk_salary estimates

# let's remove those really high values
data_w7_f <- # note the assignment (<-): the result of this calculation will make a new object
  data_w7 %>% # with the data...
  filter(uk_salary < 200000) # EDIT THIS CODE FOR Q4 

# now let's plot the salary estimates again
data_w7_f %>% 
  ggplot()+
  geom_histogram(aes(x = uk_salary)) # EDIT THIS CODE FOR Q5

# display the data as a boxplot
data_w7_f%>% 
  ggplot() +
  geom_boxplot(aes(y = uk_salary)) # EDIT THIS CODE FOR Q6

# display the data as a boxplot
data_w7_f%>% 
  ggplot() +
  geom_violin(aes(y = uk_salary, x = "")) # note violin is a bit picky with requiring an X - this gets around it

data_w7_f%>% 
  ggplot() +
  geom_density(aes(x = uk_salary, fill = home_location), alpha = .3) +
  theme_void()

# TASK 2

# What is the mean estimate? 
mean(data_w7_f$uk_salary) # EDIT THIS CODE FOR Q8
sd(data_w7_f$uk_salary) # you might also calculate the standard deviation

# Let's check if the mean estimate is significantly different from the true salary
t.test(x = data_w7_f$uk_salary, mu = 30000) # You will need to edit this for Q9

# EXTRA
# this is a "by hand" t statistic - you can add the column of salary estimates here and check with the result of t.test
# check with staff if you're not sure
t_val <- (mean(data_w7_f$uk_salary) - 30000) / (sd(data_w7_f$uk_salary)/sqrt(length(data_w7_f$uk_salary))) # denominator

# TASK 3

# more filtering practice

data_w7_f %>% # change this to your data object
  filter(home_location_in_UK == "NW" & sibling_order == "oldest")

# add more filter commands for Q12-17

# Q12
data_w7_f %>% 
  filter(home_location_in_UK == "NE")

# Q13
data_w7_f %>% 
  filter(home_location_in_UK == "Wales" & sibling_order == "middle")

# Q14
data_w7_f %>% 
  filter(attention_check == "strongly agree" & home_location == "UK" & sibling_order == "only")

# Q15
data_w7_f %>% 
  filter(home_location_in_UK != "NW")

# Q16
data_w7_f %>% 
  filter(attention_check != "strongly agree")

# Q17
data_w7_f %>% 
  filter(home_location_in_UK == "SE" | home_location_in_UK == "SW")
