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
library(tidyr)

# load data ---------------------------------------------------------------

em_data <- read_csv("~/git/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
law_data <- em_data %>%
  slice(5:8)
composite_law <- law_data %>%
  slice(4)
plot_data_1 <- composite_law %>%
  gather("state", "score", c(3:5))
arrest <- law_data %>%
  slice(1)
plot_data_2 <- arrest %>%
  gather("state", "score", c(3:5))
community <- law_data %>%
  slice(2)
plot_data_3 <- community  %>%
  gather("state", "score", c(3:5))
incarceration<- law_data %>%
  slice(3)
plot_data_4 <- incarceration %>%
  gather("state", "score", c(3:5))

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
                        
                        tabPanel("Law Enforcement",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong( " Law Enforcement")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("graphlaw", "Subdomain", 
                                                                         choices = c("Domain level", "Arrest and Court Proceedings", "Incarceration Practices", "Community Policing")
                                                             ) , 
                                                             mainPanel(uiOutput("imglaw"))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p(" 

The heatmap visualized the three subdomains and the 20 law enforcement policy questions.  A “Yes” identifies the presence of the policy in the state while a “No” represents a lack of the policy.
A summary of the overall scores for each state is presented below, with a higher number representing an increased number of policies that promote economic mobility.
Our results show the following:"),  

p("In terms of Arrest and Court Proceedings policies, Virginia performs the worst with a 0.6/1 while Oregon performs the best with a 1/1."),

p("For Incarceration Practices policies, all three states perform equally with a 0.6/1."),

p("For Community Policing Practices, both Oregon and Virginia perform at a 0.4/1 while Iowa does better with a 0.6/1."),

p("Overall, under our scoring criteria Oregon and Iowa do equally the best in terms of law enforcement policies with a 0.67/1 and Virginia does the worst with a 0.53/1.  ")),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = "heat_map_law.png"))
                                                           )),
                                                tabPanel("References")
                                                ))),
                        
                        tabPanel("Taxation",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong()),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("", "Subdomain", 
                                                                         choices = c("Domain level", "", "", "")
                                                             ) , 
                                                             mainPanel(uiOutput(""))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = ""))
                                                         )),
                                                tabPanel("References")
                                   ))),
                        tabPanel("Zoning",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong()),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("", "Subdomain", 
                                                                         choices = c("Domain level", "", "", "")
                                                             ) , 
                                                             mainPanel(uiOutput(""))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = ""))
                                                         )),
                                                tabPanel("References")
                                   ))),
                        tabPanel("Education",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong()),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("", "Subdomain", 
                                                                         choices = c("Domain level", "", "", "")
                                                             ) , 
                                                             mainPanel(uiOutput(""))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = ""))
                                                         )),
                                                tabPanel("References")
                                   ))),
                        tabPanel("Voting",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong()),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("", "Subdomain", 
                                                                         choices = c("Domain level", "", "", "")
                                                             ) , 
                                                             mainPanel(uiOutput(""))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = ""))
                                                         )),
                                                tabPanel("References")
                                   ))),
                        tabPanel("Employment",
                                 
                                 fluidRow(
                                   navlistPanel(tabPanel("Composites",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong()),
                                                                         hr(),
                                                                         strong(""),
                                                                         p()),
                                                                  column(1)),
                                                         
                                                         fluidRow(
                                                           sidebarPanel(
                                                             selectInput("", "Subdomain", 
                                                                         choices = c("Domain level", "", "", "")
                                                             ) , 
                                                             mainPanel(uiOutput(""))))),
                                                tabPanel("Heat Map",
                                                         fluidRow(width =12,
                                                                  column(1),
                                                                  column(10, h3(strong("Heat map for individual policy question")),
                                                                         hr(),
                                                                         strong(""),
                                                                         p("")),
                                                                  column(1)),
                                                         
                                                         fluidRow( mainPanel(img(height = 300, width = 400, src = ""))
                                                         )),
                                                tabPanel("References")
                                   )))
                       
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
                        
                        tabPanel("Law Enforcement",
                                 
                                 fluidRow(width =12,
                                          column(1),
                                          column(10, h3(strong( " Law Enforcement")),
                                                 hr(),
                                                 strong("Background"),
                                                 p("Law enforcement policies play an essential role in economic
                                                   mobility in American communities. Having a criminal record 
                                                   is shown to make getting a job difficult and, with half of US 
                                                   children having at least one parent with a criminal record, 
                                                   economic progression becomes challenging for children of those
                                                   convicted too. Moreover, the ramifications of a criminal record or an encounter with the law are felt most by male citizens of colour, 
                                                   particularly Hispanic or Black men, having a significant impact on the economic mobility of
                                                   entire communities. Therefore, law enforcement becomes an increasingly important aspect of 
                                                   political capital that must be studied to understand economic mobility.  "),
                                                 p("Our research on law enforcement practices and policies resulted in the identification of three main subdomains of interest: arrest and court proceedings, incarceration and community policing practices. The three subdomain comprise of 20 policy questions which assess the existence or non-existence of a practice in the binary 1/0 format detailed in the scoring outline. In addition to such binary data, the research also yielded qualitative data that provides greater nuance to the nature of a policy in a given state. The entire dataset, both binary and qualitative, can be found by clicking on the “download CSV” button below.  "),
                                                 p("a. Arrest and Court Proceeding Policies- Arrest and Court Proceedings Policies focused on the process of arresting and trying individuals in court. In this subdomain we analysed stop and identify, bail, and civil asset forfeiture policies. Practices in these areas target distinct socio-economic groups differently and exploring them gives a sense of how individuals in the community are impacted by them. For example, paying cash bail or having your assets seized has an affect on and is affected by an individual’s financial standing. In addition to this set of binary data, we descriptively explored zero tolerance policies related to driving under the influence. "),
                                                 p("b. Incarceration Practices- Incarceration Practices covers the practices and policies that impact individuals held in state facilities. We focus on inmates’ rights as well as the equitability and social justness of practices within the facility and upon return to their communities. We focus on the type of state facilities (eg public and private) as well as policies within the facility. Specifically, we assess the ability to acquire skills and certifications, as well as the ability to access necessary healthcare. Additionally, we consider youth adjudication and the death penalty. "),
                                                 p("c Community Policing Practices- Community Policing Practices explores the standards that officers must abide by in policing the community with a focus on the equality of the standards. For example, custodial sexual misconduct policies are analysed, both numerically and qualitatively, to assess how states hold officers accountable for allegations of misconduct against them by an individual in their custody. In addition, body camera usage, demographic information collection and domestic violence- related polices are considered in this subdomain. We also qualitatively assess the nature of officer training programmes in the states, particularly those pertaining to treating individuals with mental health issues.  ")), 
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
  
  
  output$imglaw <- renderUI({
    if(input$graphlaw == "Domain level"){            
      img(height = 300, width = 400, src = "law_domain_plot.png")
    }                                        
    else if(input$graphlaw == "Arrest and Court Proceedings"){
      img(height = 300, width = 400, src = "law_sub_arrest.png")
    }
    
    
    else if(input$graphlaw == "Incarceration Practices"){
      img(height = 300, width = 400, src = "law_sub_incarceration.png")
    }
    else if(input$graphlaw == "Community Policing"){
      img(height = 300, width = 400, src = "law_sub_community.png")
    } 
    
  }) 
  
  
  
}) 




# Run the application 
shinyApp(ui = ui, server = server) 