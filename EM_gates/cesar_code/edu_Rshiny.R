library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

#setwd("~/Documents/DSPG/EM project")

sidebar <- dashboardSidebar(
  # textInput("name", "Enter your name:", value = "Heike"),
  # selectInput("state", "Enter your State:", choices=c("Virginia", "Oregon", "Iowa", "Other"), selected = "Iowa"),
  selectInput("year", "Enter your State:", choices=c("2010", "2019"), selected = "2010")
  #sliderInput("size", "Sample Size:", min = 1, max = nrow(mtcars), value = 10)
)

body <- dashboardBody(
  plotlyOutput("myplot")
  #,
  #DTOutput("mytable")
)

ui <- dashboardPage(
  dashboardHeader(title = "My shiny app"),
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  #pop3states <- read_csv("~/Documents/Census/Population/productDownload_2020-07-21T223855/pop3states.csv")
  

  
  edu_3states <- read_csv("edu_3states.csv")
  
  #pop3states <- read_csv("~/Documents/Census/Population/productDownload_2020-07-21T223855/pop3states.csv")
  #dat2019 <- edu_3states %>% filter(year== "2019")
  
  #melt data base
  library(reshape2)
  edu <- melt(data = edu_3states, id.vars = c("domain", "dimension", "subcategory"), measure.vars = c("Oregon", "Virginia", "Iowa"))
  
  #averages by subcategory
  length(unique(edu$subcategory))
  
  #edu_av<- edu %>% group_by(subcategory) %>% summarize(mean=mean(value))
  edu_av2<- edu %>% group_by(variable,subcategory) %>% summarize(mean=mean(value))
  
  
  source("~/Documents/DSPG/EM project/sources/R_rainclouds.R")
  source("~/Documents/DSPG/EM project/sources/summarySE.R")
  source("~/Documents/DSPG/EM project/sources/simulateData.R")
  
  output$myplot <- renderPlotly({
     q<- ggplot(edu_av2, aes(x=variable, y= mean, fill=variable ) )+
       #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 3)+
       geom_point(aes(fill = factor(subcategory)), position = position_jitter(width = .15), size = .6)+
       #geom_point(aes(colour = factor(cyl)))
       
       geom_boxplot(aes(x = as.numeric(variable)+ 0.15, y = mean), outlier.shape = 1 , alpha = 0.1, width = .1, colour = "BLACK", show.legend = FALSE) +
       
       #geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
       ylab('Percentage')+xlab('')+
       coord_flip()+ 
       theme_cowplot()+
       guides(fill = FALSE)+
       ggtitle('Averages of policy presence per state ') +
       theme(
         panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
         axis.line = element_line(colour = "transparent"), legend.position = "none"
       ) 
     
ggplotly(q)

    
  })
  
}


shinyApp(ui, server)

