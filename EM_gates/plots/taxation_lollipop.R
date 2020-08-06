library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)


setwd('~/git/TestDSPG/dspg20uvaEM/EM_gates/WWW')

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
plot_data_1 <- composite_tax %>%
  gather("state", "score", c(3:5))

png("tax_lolli_domain_plot.png", width = 600, height = 400)
ggplot(aes(x=score, y= state), data = plot_data_1) +
  geom_point(aes(colour = state)) +
  geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +
  theme(legend.position = "none", axis.title.y  = element_blank())
dev.off()
# ggplotly(plot_data_1, tooltip = "text")

#generate subdomain wise plot by plugging in desired index
display.subdomain <- function(index) {
  #slice for the row that contains particular data
  org <- tax_data %>%
    slice(index)
  title <- org$Subdomain
  #same process as above
  plot_data_2 <- org %>%
    gather("state", "score", c(3:5))
  # org_plot <- plot_data_2 %>%
  ggplot(aes(x=score, y= state), data = plot_data_2) +
    geom_point(aes(colour = state)) +
    geom_segment( aes(x= 0, xend= score, y= state, yend= state, colour = state)) +
    theme(legend.position = "none", axis.title.y  = element_blank())
  #ggplotly(plot_data_2, tooltip = "text")
}

#gini index plot
# png("tax_lolli_gini_index.png", width = 600, height = 400)
display.subdomain(1)
# dev.off()

#tax credit plot
# png("tax_lolli_credits.png", width = 600, height = 400)
display.subdomain(2)
# dev.off()

#taxes on wealth plot
# png("tax_lolli_wealth.png", width = 600, height = 400)
display.subdomain(3)
# dev.off()

#taxes related to business plot
png("tax_lolli_business.png", width = 600, height = 400)
display.subdomain(4)
dev.off()


