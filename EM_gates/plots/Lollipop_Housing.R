library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

#import data
em_data <- read.csv("git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
head(em_data)

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20



housing <- em_data %>%
  slice(1:4)

housing



#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_housing <- housing %>%
  slice(4)
composite_housing


#gather the data to compress the diff scores into one state and score variable



plot_data_1 <- composite_housing %>%
  gather("state", "score", c(3:5))




housing_domain_plot <- plot_data_1 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +ggtitle("Composite Housing and Zoning Scores")

housing_domain_plot

#Segment each domain

#Housing Assistance
housing_assistance <- housing %>% slice(1)
housing_assistance

#Housing Development
housing_development <- housing %>% slice(2)
housing_development

#Housing Financial
housing_financial <- housing %>% slice(3)
housing_financial



#Get plot for housing assistance
plot_data_2 <- housing_assistance %>%
  gather("state", "score", c(3:5))

housing_assistance_plot <- plot_data_2 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Housing and Zoning Assistance Policy Scores")

housing_assistance_plot

#Get plot for secondary affordability
plot_data_3 <- housing_development %>%
  gather("state", "score", c(3:5))

housing_development_plot <- plot_data_3 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Housing and Zoning Development Policy Scores")

housing_development_plot

#Get plot for School Climate
plot_data_4 <- housing_financial %>%
  gather("state", "score", c(3:5))

housing_financial_plot <- plot_data_4 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Housing and Zoning Financial Policy Scores")

housing_financial_plot



