library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

# set working directory so that png images save to the correct folder.
setwd('~/git/dspg20uvaEM/EM_gates/WWW')


#import data
em_data <- read.csv("git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20
education <- em_data %>%
  slice(21:26)


#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_education <- education %>%
  slice(5)

#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- composite_education %>%
  gather("state", "score", c(3:5))


#All subdomains Composite plot
png("edu_composite.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_1) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state))+
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()

#Segment each domain

#Early Childhood Education
early_childhood_education <- education %>% slice(1)

#Post-Seconadry Affordability
post_secondary_affordability <- education %>% slice(2)

#School Climate
school_climate <- education %>% slice(3)

#Workforce Development
workforce_development <- education %>% slice(4)



#Get plot for early education
plot_data_2 <- early_childhood_education %>%
  gather("state", "score", c(3:5))

png("edu_earlychildhood.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_2) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


#Get plot for secondary affordability
plot_data_3 <- post_secondary_affordability %>%
  gather("state", "score", c(3:5))

png("edu_postsecondartafford.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_3) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state))+
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()

#Get plot for School Climate
plot_data_4 <- school_climate %>%
  gather("state", "score", c(3:5))

png("edu_schoolclimate.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_4) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state))+
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


#Get plot for Workforce Development
plot_data_5 <- workforce_development %>%
  gather("state", "score", c(3:5))

png("education_workforcedev.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_5) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


