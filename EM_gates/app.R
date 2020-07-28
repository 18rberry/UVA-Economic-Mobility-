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

# load data ---------------------------------------------------------------
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
# server-----------------------------------------------------------------------------------------

server <- function(input, output, session) {

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
