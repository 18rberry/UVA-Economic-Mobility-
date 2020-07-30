library(tidyr)
library(readr)
#import data 

#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20

law_data <- em_data %>%
  slice(10:13)

#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_law <- law_data %>%
  slice(4)

#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- composite_law %>%
  gather("state", "score", c(3:5))


law_domain_plot <- plot_data_1 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) 

#to get a subdomain-wise plot slice the data for your domain for just the row that pertains to the specific subdomain 
arrest <- law_data %>%
  slice(1)


#and then follow the same process as before  
plot_data_2 <- arrest %>%
  gather("state", "score", c(3:5))

arrest_plot <- plot_data_2 %>%
  ggplot(aes(x=score, y= state)) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) 

