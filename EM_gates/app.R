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
library(readr)

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
                      boxPlus(
                        title = "",
                        align = "center",
                        closable = FALSE, 
                        width = 12,
                        status = "warning", 
                        solidHeader = TRUE, 
                        collapsible = FALSE,
                        h1("Project Overview"),
                        img(height = 300, width = 400, src = "summary_3states.png", align = "center"),
                        p("As defined by Flora, Flora, and Gasteyer in the 5th edition of their book “Rural Communities: Legacy + Change”, Political Capital is “… a group’s ability to influence the distribution of resources 
within a social unit, including helping set the agenda of what resources are available. … Political capital consists of organization, connections, voice, and power as citizens turn shared norms and values into standards that are codified into rules, regulations, and resources distributions that are enforced.” (pg 184), 
                          Communities can leverage their political capital to make change when their norms and values are not reflected in the policies that govern them. 
                          In keeping with the Community Capitals Framework of asset mapping, we have created a policy asset map 
                          for the domains of education, taxation, employment, voting, law enforcement, and 
                          housing/zoning with a focus on policies that have the potential to impact economic mobility.
                          By identifying those policies that can impede the economic mobility a community can better strategize for effective change."),
                        h1("Approach and Ethical Considerations"),
                        p("This project benefits 'public good' in identifying and assessin policies that can later be aggregated
                          and used to determine public policy. While this project is essential to undertake, doing so comes with the ethical challenges and risks. First, 
                          the way we defined 'political capital' and hand coded is determined by our own understanding of the economic mobility. Moreover, the process of creating a
                          comspoite indicator, in the creation of binary scoring cards, reduces the policies to their bare minimum and can strip them of their nuance. Consulting domain and policy experts, 
                          providing the raw dataset and including the source of each policy information allows us to minimise the impact of biases on our process.")
                      )), 
             
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
                                   navlistPanel(
                                                 tabPanel("Visualizations",
                                                          fluidRow(width =4,
                                                                   column(1),
                                                                   column(10, h3(strong( " Visualizations")),
                                                                          hr(),
                                                                          strong(""),
                                                                          p()),
                                                                   column(1)),
                                                          
                                                          fluidRow( 
                                                                    sidebarPanel(
                                                                      selectInput("graphlaw", "Subdomain", 
                                                                                  choices = c("Domain level", "Arrest and Court Proceedings", "Incarceration Practices", "Community Policing")
                                                                      ) , 
                                                                      mainPanel( uiOutput("imglaw"), align = "center"))),
                                                                     
                                                          fluidRow( sidebarPanel( 
                                                                       selectInput("graphlawheat", "Subdomain", 
                                                                                   choices = c("Domain level", "Arrest and Court Proceedings", "Incarceration Practices", "Community Policing")
                                                                       ) , 
                                                                       mainPanel(uiOutput("imglawheat"), align = "center"))
                                                                     
                                                          )),
                                                 
                                                 
                                                 tabPanel("Results and Analysis",
                                                          fluidRow(width =4,
                                                                   column(1),
                                                                   column(10, h3(strong("")),
                                                                          hr(),
                                                                          strong(""),
                                                                          p(" 
                                                                            
                                                                            The heatmap visualized the three subdomains and the 20 law enforcement policy questions.  A “Yes” or 1 identifies the presence of the policy in the state while a “No” or 0 represents a lack of the policy.
                                                                            A summary of the overall scores for each state is presented below, with a higher number representing an increased number of policies that promote economic mobility.
                                                                            Our results show the following:"),  
                                                                          p(img( src = "heat_table.jpg")),
                                                                          p("In terms of Arrest and Court Proceedings policies, Virginia performs the worst with a 0.6/1 while Oregon performs the best with a 1/1."),
                                                                          
                                                                          p("For Incarceration Practices policies, all three states perform equally with a 0.6/1."),
                                                                          
                                                                          p("For Community Policing Practices, both Oregon and Virginia perform at a 0.4/1 while Iowa does better with a 0.6/1."),
                                                                          
                                                                          p("Overall, under our scoring criteria Oregon and Iowa do equally the best in terms of law enforcement policies with a 0.67/1 and Virginia does the worst with a 0.53/1.  ")),
                                                                   column(1))
                                                          
                                                          ),
                                                 tabPanel( width =4,
                                                   "References")
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
                               column(1)),
                      fluidRow( 
                        boxPlus( 
                          title = "Team", 
                          closable = FALSE, 
                          width = NULL, 
                          status = "warning", 
                          solidHeader = TRUE, 
                          collapsible = TRUE, 
                          h2("DSPG Team Members"), 
                          #Possible way to display info? 
                          fluidRow( 
                            
                            style = "height:10px;"), 
                          fluidRow( 
                            # Lara 
                            column(2,   
                                   div(class="panel panel-default",     
                                       div(class="panel-body", width = "600px",  
                                           align = "center",  
                                           div( 
                                             tags$img(src = "Lara_Haase.jpg", 
                                                      width = "100px", height = "100px") 
                                           ), 
                                           div( 
                                             tags$h5("Lara"), 
                                             tags$h6( tags$i("Graduate Fellow")) 
                                           ), 
                                           div( 
                                             "Lara is pursing at Masters of Science in Data Science and Public Policy at at Carnegie Mellon." 
                                           ) 
                                       ) 
                                   ) 
                            ), 
                            # Martha 
                            column(2, 
                                   div(class="panel panel-default", 
                                       div(class="panel-body", width = "600px", 
                                           align = "center", 
                                           div( 
                                             tags$img(src = "Martha.jpg", 
                                                      width = "100px", height = "100px") 
                                           ), 
                                           div( 
                                             tags$h5("Martha Czernuszenko"), 
                                             tags$h6( tags$i("Intern")) 
                                           ), 
                                           div( 
                                             "Martha recently graduated from The University of Texas where she studied Information Systems & Business Honors." 
                                           ) 
                                       ) 
                                   ) 
                            ), 
                            # Riya 
                            column(2, 
                                   div(class="panel panel-default", 
                                       div(class="panel-body", width = "600px", 
                                           align = "center", 
                                           div( 
                                             tags$img(src = "riya.png", 
                                                      width = "100px", height = "100px")), 
                                           div( 
                                             tags$h5("Riya Berry"), 
                                             tags$h6( tags$i("Intern")) 
                                           ), 
                                           div( 
                                             ""                                           ) 
                                       ) 
                                   ) 
                            ), 
                            
                            
                            # Tasfia 
                            column(2, 
                                   div(class="panel panel-default", 
                                       div(class="panel-body", width = "600px", 
                                           align = "center", 
                                           div( 
                                             tags$img(src = "tasfia.png", 
                                                      width = "100px", height = "120px")), 
                                           div( 
                                             tags$h5("Tasfia Chowdhury"), 
                                             tags$h6( tags$i("Intern")) 
                                           ), 
                                           div( 
                                             ""                                           ) 
                                       ) 
                                   ) 
                            ), 
                            # Vatsala  
                            column(2, 
                                   div(class="panel panel-default", 
                                       div(class="panel-body", .width = "600px", 
                                           align = "center", 
                                           div( 
                                             tags$img(src = "Vatsala_Ramanan.jpg", 
                                                      width = "100px", height = "100px")), 
                                           div( 
                                             tags$h5("Vatsala Ramanan"), 
                                             tags$h6( tags$i("Intern")) 
                                           ), 
                                           div( 
                                             "Vatsala is a rising junior at Smith College studying Quantitative Economics and Government."                                           ) 
                                       ) 
                                   ) 
                            ), 
                            column(1)), 
                          
                          
                          
                          
                          #SDAD 
                          h2("UVA SDAD Team Members"), 
                          p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia. SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research and quantitative methods to inform policy decision-making and evaluation. The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology, political science, policy, health IT, public health, program evaluation, and data science. 
                            The SDAD office is located near our nation's capital in Arlington, VA. You can 
                            learn more about us at", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "https://biocomplexity.virginia.edu/social-decision-analytics"), "."), 
                          fluidRow( 
                            
                            style = "height:50px;"), 
                          
                          fluidRow(  
                            # Vicki 
                            column(2, 
                                   div(class="panel panel-default", 
                                       div(class="panel-body", width = "600px", 
                                           align = "center", 
                                           div( 
                                             tags$img(src = "teamphotos/KATHRYN.jpg", 
                                                      width = "100px", height = "100px") 
                                           ), 
                                           div( 
                                             tags$h5("Kathryn Linehan"), 
                                             tags$h6( tags$i("Principal Investigator")) 
                                           ), 
                                           div( 
                                             "Kathryn Linehan" 
                                           ) 
                                       ) 
                                   ) 
                            ), 
                            column(1)), 
                          h2("Project Sponsors"), 
                          p("[Photos, information, and/or links about your sponsor go about here. You may want to use materials that your sponsors have already shared with you about their institution or coordinate with your stakeholders to include pertinent information here.]"), 
                          h2("Acknowledgements"), 
                          p("[Optional: You can also include external collaborators in this section or a separate section.]") 
                          ) 
                      )),        
             
             tabPanel(h4("Acknowledgements & Contacts"), 
                      fluidRow(width = 12, 
                               column(1, align = "center", h3(strong("Approach"))),
                               column(1)))
             )) 






# server-----------------------------------------------------------------------------------------


server <- shinyServer(function(input,output){  
  
  
  output$imglaw <- renderUI({
    if(input$graphlaw == "Domain level"){            
      img(height = 300, width = 400, src = "law_domain_plot.png", align = "left")
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
  
  output$imglawheat <- renderUI({
    if(input$graphlawheat == "Domain level"){            
      img(height = 300, width = 400, src = "arrest_heat.png", align = "left")
    }                                        
    else if(input$graphlawheat == "Arrest and Court Proceedings"){
      img(height = 300, width = 400, src = "")
    }
    
    
    else if(input$graphlawheat == "Incarceration Practices"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphlawheat == "Community Policing"){
      img(height = 300, width = 400, src = "")
    }   
    
    
  }) 
  
  
  
}) 




# Run the application 
shinyApp(ui = ui, server = server) 


