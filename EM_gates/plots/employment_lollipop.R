library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)


#import data 
data<- read_csv("~/git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20

emp_data <- data %>%
  slice(9:11)

#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_emp <- data %>%
  slice(12)

#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- emp_data %>%
  gather("state", "score", c(3:5))

plot_data_1 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) 

#to get a subdomain-wise plot slice the data for your domain for just the row that pertains to the specific subdomain 
org <- emp_data %>%
  slice(1)

#and then follow the same process as before  
plot_data_2 <- org %>%
  gather("state", "score", c(3:5))

org_plot <- plot_data_2 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) 

