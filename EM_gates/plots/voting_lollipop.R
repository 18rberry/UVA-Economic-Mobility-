library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

setwd('~/git/TestDdspg20uvaEM/EM_gates/WWW')

#import data
data<- read_csv("~/git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")

#slice the data to just your domain
vote_data <- data %>%
  slice(18:19)

#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_vote <- data %>%
  slice(20)

#gather the data to compress the diff scores into one state and score variable
plot_data_vote <- vote_data %>%
  gather("state", "score", c(3:5))

comp_data <- composite_vote %>%
  gather("state", "score", c(3:5))




#Composite Overall Score for Voting
png("vote_domain_plot.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = comp_data) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()



# Accessibility (subdomain)
#to get a subdomain-wise plot slice the data for your domain for just the row that pertains to the specific subdomain
access <- vote_data %>%
  slice(1)
#and then follow the same process as before
plot_data_access <- access %>%
  gather("state", "score", c(3:5))

#Composite Score for Access
png("vote_sub_access.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_access) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


# Registration (subdomain)
reg <- vote_data %>%
  slice(2)
#and then follow the same process as before
plot_data_reg <- reg %>%
  gather("state", "score", c(3:5))

#Composite Score for Registration
png("vote_sub_reg.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_reg) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


