library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

setwd('~/git/dspg20uvaEM/EM_gates/WWW')

#import data
data<- read_csv("~/git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")

#slice the data to just your domain
emp_data <- data %>%
  slice(9:11)
emp_data
#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_emp <- data %>%
  slice(12)
composite_emp
#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- emp_data %>%
  gather("state", "score", c(3:5))
comp_data <- composite_emp %>%
  gather("state", "score", c(3:5))



#Composite Overall Score for Employment
png("employment_domain_plot.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = comp_data) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()

# Interactive grouped bar chart w/ all subdomains
#fig <- plot_ly(emp_data, x = ~Subdomain, y = ~Oregon, type = 'bar', name = 'Oregon')
#fig <- fig %>% add_trace(y = ~Iowa, name = 'Iowa')
#fig <- fig %>% add_trace(y = ~Virginia, name = 'Virginia')
#fig <- fig %>% layout(title = 'Employment Policies', barmode = 'group')
#fig


# Organizing (subdomain)
#to get a subdomain-wise plot slice the data for your domain for just the row that pertains to the specific subdomain 
org <- emp_data %>%
  slice(1)
#and then follow the same process as before  
plot_data_org <- org %>%
  gather("state", "score", c(3:5))

#Composite Score for Organizing
png("employment_sub_org.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_org) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


# Protections (subdomain)
protect <- emp_data %>%
  slice(2)
#and then follow the same process as before  
plot_data_protect <- protect %>%
  gather("state", "score", c(3:5))

#Composite Score for Protections
png("employment_sub_protect.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_protect) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()


# Wage (subdomain)
wage <- emp_data %>%
  slice(3)
#and then follow the same process as before  
plot_data_wage <- wage %>%
  gather("state", "score", c(3:5))

#Composite Score for Wage
png("employment_sub_wage.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_wage) +
  geom_point(aes(colour = state)) +  
  geom_segment( aes(x= 0, xend= score, yend= state, colour = state)) +
  xlim(-0.01, 1) +
  theme(legend.position = "none", axis.title.y  = element_blank())

dev.off()