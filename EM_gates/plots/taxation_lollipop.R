library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

#import data
data<- read_csv("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
data
#slice the data to just your domain
# for example, for law encforcement i'm slicing from 1:20

tax_data <- data %>%
  slice(13:16)

#from your domain-specific data, slice for just the composite row (don't filter because that may be wierd)
composite_tax <- data %>%
  slice(17)

#gather the data to compress the diff scores into one state and score variable
plot_data_1 <- tax_data %>%
  gather("state", "score", c(3:5))


plot_data_1 <- plot_data_1 %>%
  ggplot(aes(x=score, y= state, text = paste("Subdomain:", Subdomain))) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +
  ggtitle("Taxation By State")

ggplotly(plot_data_1, tooltip = "text")

#generate subdomain wise plot by plugging in desired index
display.subdomain <- function(index) {
  #slice for the row that contains particular data
  org <- tax_data %>%
    slice(index)
  title <- org$Subdomain
  #same process as above
  plot_data_2 <- org %>%
    gather("state", "score", c(3:5))
  org_plot <- plot_data_2 %>%
    ggplot(aes(x=score, y= state, text = paste("Score:", score))) +
    geom_point(aes(colour = state)) +
    geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +
    ggtitle(paste("Tax Policy Dimension:", title))
}

wealth <- display.subdomain(3)
wealth

ggplotly(wealth, tooltip = "text")

