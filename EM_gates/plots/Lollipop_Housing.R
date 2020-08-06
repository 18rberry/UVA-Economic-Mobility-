library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

setwd('~/git/dspg20uvaEM/EM_gates/WWW')

#import data
em_data <- read.csv("git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
#head(em_data)

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20
housing <- em_data %>%
  slice(1:4)


#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_housing <- housing %>%
  slice(4)

#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- composite_housing %>%
  gather("state", "score", c(3:5))


png("composite_housing_lollipop.png", width = 600, height = 400)
  ggplot(aes(x=score, y= state), data = plot_data_1) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


#Segment each domain

#Housing Assistance
housing_assistance <- housing %>% slice(1)

#Housing Development
housing_development <- housing %>% slice(2)

#Housing Financial
housing_financial <- housing %>% slice(3)



#Get plot for housing assistance
plot_data_2 <- housing_assistance %>%
  gather("state", "score", c(3:5))

png("assistance_lollipop.png", width = 600, height = 400)
  ggplot(aes(x=score, y= state), data = plot_data_2) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


#Get plot for Development
plot_data_3 <- housing_development %>%
  gather("state", "score", c(3:5))


png("development_lollipop.png", width = 600, height = 400)
  ggplot(aes(x=score, y= state), data = plot_data_3) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


#Get plot for Financial
plot_data_4 <- housing_financial %>%
  gather("state", "score", c(3:5))


png("financial_lollipop.png", width = 600, height = 400)
  ggplot(aes(x=score, y= state), data = plot_data_4) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()

