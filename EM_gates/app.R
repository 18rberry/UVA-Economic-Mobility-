library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinythemes)
library(tableHTML)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(RColorBrewer)
library(readxl)
library(plotly)
library(GGally)
library(ggrepel)
library(shinydashboard)
library(readxl)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(shinydashboardPlus)

# load data ---------------------------------------------------------------

policing_data <- read_excel("~/policing_data.xlsx", 
                            sheet = "Final_Composite")

domain_data <- policing_data %>%
  slice(54:56)

sub_domain_data <- policing_data %>%
  slice(41:52)

sub_category_data <- policing_data %>%
  slice(1:39)
#plots page ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Landing page --------------------------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
  tags$style(
    ".leaflet .legend {width:200px; text-align: left;}",
    ".leaflet .legend i{float: left;}",
    ".leaflet .legend label{float:left; text-align: left;}"
  ),
  tags$head(tags$style(HTML(" .sidebar { font-size: 40%; } "))),
  
  headerPanel(
    img(src = "MyImage.jpg", 
        class = "topimage", width = "100%", style = "display: block; margin-left: auto; margin-right: auto;" 
    )),
  hr(),
  
  fluidRow(width = 12,style = "margin = 20px",  column(12, align = "center", h2(strong("Political Community Capital ")))),
  
  hr(),
  
  navbarPage("", 
             
             tabPanel(h4("Project Overview"),
                      flipBox(
                        id = 1,
                        header_img = "white.PNG",
                        main_img = "white.PNG",
                        front_title = "Overview and Goals",
                        back_title = "Approach and Ethical Considerations",
                        #landing page write- up front side of flip box
                        "Lorem ipsum is the nonsense filler text that typically demonstrates the font and style of a text in a document or visual demonstration. Originally from Latin, lorem ipsum has no intelligible meaning, but is simply a display of letters and characteristics to be viewed as a sample with given graphical elements in a file.",
                        verticalProgress(
                          value = 50,
                          active = TRUE,
                          status = "warning",
                          size = "xs"
                        ),
                        back_content = tagList(
                          column(
                            width = 12 
                          ), 
                          #landing page back side of flip box
                          "Lorem ipsum is the nonsense filler text that typically demonstrates the font and style of a text in a document or visual demonstration. Originally from Latin, lorem ipsum has no intelligible meaning, but is simply a display of letters and characteristics to be viewed as a sample with given graphical elements in a file."
                        )
                      )
                      
  ),
  
  
  navbarMenu(h4("Domains of analysis"),
             tabPanel("All",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong("")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             
             tabPanel("Law Enforcement and Policing",
                      
                      fluidRow(
                        navlistPanel(tabPanel("numeric data",
                                              fluidRow(width =12,
                                                       column(1),
                                                       column(10, h3(strong( " Law Enforcement and Policing")),
                                                              hr(),
                                                              strong(""),
                                                              p()),
                                                       column(1)),
                                              
                                              fluidRow(
                                                sidebarPanel(
                                                  selectInput("graph", "Select a data level", 
                                                              choices = c("Domain level", "Sub-domain level", "Sub-category level")
                                                  )
                                                )), 
                                                mainPanel(plotOutput("selected_graph"))),
                                              tabPanel("textual data")))),
             
             tabPanel("Education",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Zoning",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Taxation",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Voting",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Health",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Employment",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px')))
  ),
  
  navbarMenu(h4("Data, Measures and Methods"),
             tabPanel("All",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             
             tabPanel("Law Enforcement and Policing",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( " Law Enforcement and Policing")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Education",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("word_cloud", height = '700px'))),
             tabPanel("Zoning",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Taxation",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Voting",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Health",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px'))),
             tabPanel("Employment",
                      
                      fluidRow(width =12,
                               column(1),
                               column(10, h3(strong( "Education")),
                                      hr(),
                                      strong("Composite"),
                                      p()),
                               column(1)), 
                      fluidRow(width = 12, style = "margin: 20px",
                               plotOutput("", height = '700px')))
  ),
  
  
  
  tabPanel(h4("Project Team"), 
           fluidRow(width = 12, 
                    column(1, align = "center", h3(strong("Approach"))),
                    column(1))),
  
  
  tabPanel(h4("Acknowledgements & Contacts"), 
           fluidRow(width = 12, 
                    column(1, align = "center", h3(strong("Approach"))),
                    column(1)))
))
# server-----------------------------------------------------------------------------------------


server <- shinyServer(function(input,output){  
  
  plot1 <-   reactive({ domain_data %>%
    ggplot(aes(x=Composite, y= State)) + 
      facet_grid(State ~ .) +
    geom_point(aes(colour = State, size = 2)) +  
    geom_segment( aes(x= 0, xend= Composite, y= State, yend= State, colour = State))
                       }) 
  
  plot2 <-   reactive({ sub_domain_data %>%
      ggplot(aes(x= Composite, y= `Sub-domain`))+
      facet_grid(State ~ .) +
      geom_point(aes(colour = State, size = 2)) +  
      geom_segment( aes(x= 0, xend= Composite, y= `Sub-domain`, yend= `Sub-domain`, colour = State)) 
                         }) 
  
  plot3 <- reactive({ sub_category_data %>%
      ggplot(aes(x=Composite, y= `Sub Categories`)) +
      facet_grid(State ~ .) +
      geom_point(aes(colour = State)) +  
      geom_segment( aes(x= 0, xend= Composite, y= `Sub Categories`, yend= `Sub Categories`, colour = State)) 
   
  }) 
     
  
  graphInput <- reactive({
    switch(input$graph,
           "Domain level" = plot1(),
           "Sub-domain level" = plot2(),
           "Sub-category level" = plot3()
    )
  })
  
  

  output$selected_graph <- renderPlot({ 
    graphInput()
  })
  
  

})   






# Run the application 
shinyApp(ui = ui, server = server)