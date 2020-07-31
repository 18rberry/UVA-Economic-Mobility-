library(tidyr)
library(readr)
#import data
em_data <- read.csv("git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
head(em_data)

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20



education <- em_data %>%
  slice(21:26)

education



#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_education <- education %>%
  slice(5)
composite_education


#gather the data to compress the diff scores into one state and score variable



plot_data_1 <- composite_education %>%
  gather("state", "score", c(3:5))




education_domain_plot <- plot_data_1 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +ggtitle("Composite Education Scores")

education_domain_plot

#Segment each domain

#Early Childhood Education
early_childhood_education <- education %>% slice(1)
early_childhood_education

#Post-Seconadry Affordability
post_secondary_affordability <- education %>% slice(2)
post_secondary_affordability

#School Climate
school_climate <- education %>% slice(3)
school_climate

#Workforce Development
workforce_development <- education %>% slice(4)
workforce_development



#Get plot for early education
plot_data_2 <- early_childhood_education %>%
  gather("state", "score", c(3:5))

early_childhood_education_plot <- plot_data_2 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Early Childhood Education Scores")

early_childhood_education_plot

#Get plot for secondary affordability
plot_data_3 <- post_secondary_affordability %>%
  gather("state", "score", c(3:5))

post_secondary_affordability_plot <- plot_data_3 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Post Secondary Affordability Scores")

post_secondary_affordability_plot

#Get plot for School Climate
plot_data_4 <- school_climate %>%
  gather("state", "score", c(3:5))

school_climate_plot <- plot_data_4 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("School Climate Scores")

school_climate_plot


#Get plot for Workforce Development
plot_data_5 <- workforce_development %>%
  gather("state", "score", c(3:5))

workforce_development_plot <- plot_data_5 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state))+ggtitle("Workforce Development Scores")

school_climate_plot


