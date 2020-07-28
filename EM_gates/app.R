install.packages('shinythemes')
install.packages('tableHTML')
install.packages('ggthemes')
install.packages('GGally')
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

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
<<<<<<< HEAD
Policing_data <- read_excel("~/policing_data.xlsx",
                            sheet = "overall", col_types = c("text",
                                                             "text", "text", "text", "text", "text", "text"), col_names = c("score_criteria", "sub_domain", "state", "details", "score", "type", "link"))

policing_2 <- read_excel("~/policing_data.xlsx",
                         sheet = "Composite", col_types = c("text",
                                                            "text", "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric"), col_names = c("state", "domain", "stop_iden", "bail_cash", "bail_bond", "CAF_convic", "CAF_burden", "ban_box", "health_care", "pre_natal", "abortion", "learning", "body_cams", "dem", "custodial_sexual_mis", "private_prisons", "death_penalty", "juv_age", "composite" ))

policing_2 <-policing_2[-c(1), ]

arrest_policies <- policing_2 %>%
  select(state, domain, stop_iden, bail_cash, bail_bond, CAF_convic, CAF_burden, composite)

score_data <- Policing_data %>%
  filter(type == "descriptive") %>%
  select(score)
docs <- Corpus(VectorSource(score_data))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)

#Stye page ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
=======
>>>>>>> a6d9836edc1916c3088dd568db7f8e6b6b8294ed

policing_data <- read_excel("~/git/dspg20uvaEM/EM_gates/data/policing_data.xlsx", 
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
<<<<<<< HEAD


  fluidRow(width = 12,
           column(1),
           column(10,
                  p(),
                  p('Description')),
           column(1)),
  hr(),

  navbarPage("",
                      navbarMenu("Domain",
                                 tabPanel("Overview",

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
                                                     plotOutput("policing_plot", height = '700px'))),
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



    tabPanel(h4("Data, Measures and Methods"),
             fluidRow(width = 12,
                      column(12, align = "center", h3(strong("Approach"))),
                      column(1))),

    tabPanel(h4("Acknowledgements and Contacts"),
             fluidRow(width = 12,
                      column(1, align = "center", h3(strong("Approach"))),
                      column(1))),

    tabPanel(h4("Datasets"),
             fluidRow(width = 12,
                      column(1, align = "center", h3(strong("Approach"))),
                      column(1))),

    tabPanel(h4("Team"),
             fluidRow(width = 12,
                      column(1, align = "center", h3(strong("Approach"))),
                      column(1)))
    ))
=======
  
  navbarPage("", 
             
             tabPanel(h4("Project Overview"),
                      flipBox(
                        id = 1,
                        header_img = "white.PNG",
                        main_img = "white.PNG",
                        front_title = "Overview and Goals",
                        back_title = "Approach and Ethical Considerations",
                        #landing page write- up front side of flip box
                        #To add newlines:
                        #https://stackoverflow.com/questions/26368192/how-to-insert-new-line-in-r-shiny-string 
                        "As defined by Flora, Flora, and Gasteyer in the 5th edition of their book “Rural Communities: Legacy + Change”, Political Capital is “… a group’s ability to influence the distribution of resources within a social unit, including helping set the agenda of what resources are available. … Political capital consists of organization, connections, voice, and power as citizens turn shared norms and values into standards that are codified into rules, regulations, and resources distributions that are enforced.” (pg 184)", 
                        "Communities can leverage their political capital to make change when their norms and values are not reflected in the policies that govern them. In keeping with the Community Capitals Framework of asset mapping, we have created a policy asset map for the domains of education, taxation, employment, voting, law enforcement, and housing/zoning with a focus on policies that have the potential to impact economic mobility. By identifying those policies that can impede the economic mobility a community can better strategize for effective change.",
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
>>>>>>> a6d9836edc1916c3088dd568db7f8e6b6b8294ed
# server-----------------------------------------------------------------------------------------


<<<<<<< HEAD
output$policing_plot <- renderPlot({
  policing_2 %>%
    ggplot(aes(x=composite, y= domain)) + geom_point(aes(colour=state, size = composite))
})


output$word_cloud <- renderPlot({
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))})
}
# Run the application
shinyApp(ui = ui, server = server)
=======
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
>>>>>>> a6d9836edc1916c3088dd568db7f8e6b6b8294ed
