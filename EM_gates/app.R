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
library(DT)


# load data -----------------------------------------------------------------------------
em_data <- read_csv("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")

#full data, not composites
all_data <- read_excel("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/em_master_data_final.xlsx")


#---------------------------------
#dataTable data
emp_dt_data<- all_data %>%
  filter(domain == 'Employment')

vote_dt_data<- all_data %>%
  filter(domain == 'Voting')

edu_dt_data<- all_data %>%
  filter(domain == 'Education')

house_dt_data<- all_data %>%
  filter(domain == 'Housing')

law_dt_data<- all_data %>%
  filter(domain == 'Policing')

tax_dt_data<- all_data %>%
  filter(domain == 'Taxation')


#----------------------------
#Law Enforcement / Policing Data
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
                        img(height = 390, width = 890, src = "landing_pg_dual_graphic.png", align = "center"),
                        h4("Political Capital definition:"),
                        p("“… a group’s ability to influence the distribution of resources
                          within a social unit, including helping set the agenda of what resources are available. … Political capital consists of organization, connections, voice, and power as citizens turn shared norms and values into standards that are codified into rules, regulations, and resources distributions that are enforced,”",
                          br(),
                          "as defined by Flora, Flora, and Gasteyer in the 5th edition of their book", em("Rural Communities: Legacy + Change"), "(pg 184).",
                          br(),
                          br(),
                          "Communities can leverage their political capital to make change when their norms and values are not reflected in the policies that govern them.
                          In keeping with the Community Capitals Framework of asset mapping, we have created a policy asset map for the domains of education, taxation, employment, voting, law enforcement, and
                          housing/zoning with a focus on policies that have the potential to impact economic mobility.
                          By identifying those policies that can impede the economic mobility a community can better strategize for effective change."),
                        h1("Approach and Ethical Considerations"),
                        p("This project benefits 'public good' in identifying and assessin policies that can later be aggregated
                          and used to determine public policy. While this project is essential to undertake, doing so comes with the ethical challenges and risks. First,
                          the way we defined 'political capital' and hand coded is determined by our own understanding of the economic mobility. Moreover, the process of creating a
                          comspoite indicator, in the creation of binary scoring cards, reduces the policies to their bare minimum and can strip them of their nuance. Consulting domain and policy experts,
                          providing the raw dataset and including the source of each policy information allows us to minimise the impact of biases on our process.",
                          br(),
                          br())
                        )),

             navbarMenu(h4("Domains of analysis"),
                        tabPanel("Summary",

                                 fluidRow(width =12,
                                          column(1),
                                          column(10, h3(strong("Our Domains and Taxonomy")),
                                                 hr(),
                                                 p(
                                                   "Political capital takes various forms of participation and representation of groups and individuals in the community.  This project seeks to summarize several aspects of political capital that largely affect economic mobility of communities.  The information focuses on six domains that include the following:",
                                                   br(),
                                                   br(),
                                                   "Law Enforcement and Criminal Justice", br(),
                                                   "Taxation", br(),
                                                   "Housing and Zoning", br(),
                                                   "Education", br(),
                                                   "Voting", br(),
                                                   "Employment", br(),
                                                   br(),
                                                   "Estimating a quantitative measure of the existence of political capital is a challenging task. We propose the quantification of the identified policy domains into an index. This index consists of sub-domains (subject areas) which contain specific questions about existence of policies related to Economic Mobility. For instance, the Law Enforcement and Criminal Justice domain is explained with the following structure: "
                                                   ),
                                                 img(height = 390, width = 750, src = "taxonomy_graphic.png", align = "center")
                                                 ),
                                          column(1)),
                                 fluidRow(width = 12, style = "margin: 20px",
                                          plotOutput("", height = '700px'))),

                        tabPanel("Law Enforcement",

                                 fluidRow(
                                   navlistPanel(
                                     tabPanel("Composite Indicators",
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h4(strong("Law Enforcement & Criminal Justice")),
                                                              column(1))),
                                              hr(),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5(strong("Composite Indicators")),
                                                              column(1))),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."
                                                                     ),
                                                              column(1))),
                                              fluidRow(
                                                sidebarPanel( width = 4,
                                                              selectInput("graphlaw", "Subdomain",
                                                                          choices = c("Domain level", "Arrest and Court Proceedings", "Incarceration Practices", "Community Policing")
                                                                      )),
                                                                      mainPanel(  uiOutput("imglaw"), align = "center"))),

                                     tabPanel("Policy Asset Map",
                                              fluidRow(width =4,
                                                          column(1),
                                                          column(12, h4(strong("Law Enforcement & Criminal Justice")),
                                                                 column(1))),
                                              hr(),

                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5(strong("Policy Asset Map")),
                                                              column(1))),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5("The plots below demonstrate the existence (yes) or non-existence (no) of policies in the states considered based on the policy questions that were asked."
                                                                     ),
                                                              column(1))),
                                              fluidRow(
                                                sidebarPanel( width = 4,
                                                              selectInput("graphlawheat", "Subdomain",
                                                                                   choices = c("Domain level", "Arrest and Court Proceedings", "Incarceration Practices", "Community Policing")
                                                                       )) ,
                                                                       mainPanel(uiOutput("imglawheat"), align = "center"))),


                                                 tabPanel("Analysis & Results",
                                                          fluidRow(width =4,
                                                                   column(1),
                                                                   column(12, h4(strong("Law Enforcement & Criminal Justice")),
                                                                          column(1))),
                                                          hr(),

                                                          fluidRow(width =4,
                                                                   column(1),
                                                                   column(12, h5(strong("Analysis and Results")),
                                                                          column(1))),
                                                          fluidRow(width =4,
                                                                   column(1),
                                                                   column(10,
                                                                          p("The polisy asset map visualizes the three subdomains and the 20 law enforcement policy questions.  A “Yes” or 1 identifies the presence of the policy in the state while a “No” or 0 represents a lack of the policy.
                                                                            A summary of the overall scores for each state is presented below, with a higher number representing an increased number of policies that promote economic mobility.
                                                                            Our results show the following:"),
                                                                          p(img( src = "heat_table.jpg")),
                                                                          p("In terms of Arrest and Court Proceedings policies, Virginia performs the worst with a 0.60/1 while Oregon performs the best with a 1.00/1."),

                                                                          p("For Incarceration Practices policies, all three states perform equally with a 0.60/1."),

                                                                          p("For Community Policing Practices, both Oregon and Virginia perform at a 0.40/1 while Iowa does better with a 0.60/1."),

                                                                          p("Overall, under our scoring criteria Oregon and Iowa do equally the best in terms of law enforcement policies with a 0.67/1 and Virginia does the worst with a 0.53/1.  ")),
                                                                   column(1))

                                                 ) ))),


    #----------------------------------------------------Tax
                        tabPanel("Taxation",

                                 fluidRow(
                                   navlistPanel(
                                     tabPanel("Composite Indicators",
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h4(strong("Taxation")),
                                                              column(1))),
                                              hr(),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5(strong("Composite Indicators")),
                                                              column(1))),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."),
                                                              column(1))),

                                              fluidRow(
                                                sidebarPanel( width = 4,
                                                              selectInput("graphtax", "Subdomain",
                                                                          choices = c("Domain level", "Tax Credits", "Taxes on Wealth",
                                                                                      "Taxes Related to Business", "Gini Index")
                                                              )),
                                                mainPanel(  uiOutput("imgtax"), align = "center"))
                                              ),

                                     tabPanel("Policy Asset Map",
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h4(strong("Taxation")),
                                                              column(1))),
                                              hr(),

                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5(strong("Policy Asset Map")),
                                                              column(1))),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5("The plots below demonstrate the existence (yes) or non-existence (no) of policies in the states considered based on the policy questions that were asked."
                                                                     ),

                                                              column(1))),

                                              fluidRow(sidebarPanel( width = 4,
                                                                     selectInput("graphtaxheat", "Subdomain",
                                                                                 choices = c("Domain level", "Tax Credits", "Taxes on Wealth",
                                                                                             "Taxes Related to Business", "Gini Index")
                                                                     )) ,
                                                       mainPanel(uiOutput("imgtaxheat"), align = "center"))
                                                      # Alternate way to show img that doesn't use the server?:
                                                      #fluidRow( mainPanel(img(height = 300, width = 400, src = "taxation_heatmap.png")))
                                              ), # close Asset Map tabPanel


                                     tabPanel("Analysis & Results",
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h4(strong("Taxation")),
                                                              column(1))),
                                              hr(),

                                              fluidRow(width =4,
                                                       column(1),
                                                       column(12, h5(strong("Analysis and Results")),
                                                              column(1))),
                                              fluidRow(width =4,
                                                       column(1),
                                                       column(10,
                                                              p("The policy asset map visualizes the four subdomains and the tax policies that influence them. A “Yes” identifies the presence of the policy in the state while a “No” represents a lack of the policy. A summary of the overall scores for each state is presented below, with a higher number
representing an increased number of policies that promote economic mobility.
                                                                           Our results show the following: "),
                                                              p(img( src = "heat_table.jpg")),
                                                              p(strong("Tax credits:"), " Oregon and Iowa perform the highest with a score of 0.75.
                                                                           On the other hand, Virginia holds a score of 0.25, highlighting a need for improvement."),
                                                              p(strong("Wealth-related taxes:"), "Oregon and Iowa perform the highest with a score of 0.67.
                                                                           In contrast, Virginia holds the lowest score of 0.33, highlighting a need for improvement.  "),
                                                              p(strong("Taxes related to businesses and corporations:"), "Oregon demonstrates outstanding performance with a score of 1.00.
                                                                           In contrast, Virginia and Iowa had scores of 0.67 and 0.33 respectively, highlighting a need for improvement.  "),
                                                              p(strong("Gini index:"), "all states had a score of 0.50. All states could improve in this policy domain aspect"),
                                                              p(strong("Overall:"), "______ had a score of 0.__. All states could improve taxation policies to encourage Economic Mobility")
                                                              ),
                                                       column(1))
                                              ) # close anaylsis & results
                                     ) #close NavlistPanel to select sub-domain for tax heatmap
                                   ) #close fluid row
                                 ), #close Tax panel



  #--Housing and Zoning---------------------------------------
                          tabPanel("Housing and Zoning",

                                   fluidRow(
                                     navlistPanel(
                                       tabPanel("Composite Indicators",
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h4(strong("Housing and Zoning")),
                                                                column(1))),
                                                hr(),
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h5(strong("Composite Indicators")),
                                                                column(1))),
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."),
                                                                column(1))),

                                                fluidRow(
                                                  sidebarPanel( width = 4,
                                                                selectInput("graphhouse", "Subdomain",
                                                                            choices = c("Domain level", "Housing Assistance Policies", "Housing Development Policies", "Housing Financial Policies")
                                                                )),
                                                  mainPanel(  uiOutput("imghouse"), align = "center"))
                                       ),

                                       tabPanel("Policy Asset Map",
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h4(strong("Housing and Zoning")),
                                                                column(1))),
                                                hr(),

                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h5(strong("Policy Asset Map")),
                                                                column(1))),
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h5("The plots below demonstrate the existence (yes) or non-existence (no) of policies in the states considered based on the policy questions that were asked."
                                                         ),

                                                         column(1))),

                                                fluidRow(sidebarPanel( width = 4,
                                                                       selectInput("graphhouseheat", "Subdomain",
                                                                                   choices = c("Domain level", "Housing Assistance Policies", "Housing Development Policies", "Housing Financial Policies")
                                                                       )) ,
                                                         mainPanel(uiOutput("imghouseheat"), align = "center"))
                                                # Alternate way to show img that doesn't use the server?:
                                                #fluidRow( mainPanel(img(height = 300, width = 400, src = "taxation_heatmap.png")))
                                       ), # close Asset Map tabPanel


                                       tabPanel("Analysis & Results",
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h4(strong("Housing and Zoning")),
                                                                column(1))),
                                                hr(),

                                                fluidRow(width =4,
                                                         column(1),
                                                         column(12, h5(strong("Analysis and Results")),
                                                                column(1))),
                                                fluidRow(width =4,
                                                         column(1),
                                                         column(10,
                                                                p("The policy asset map visualizes the three subdomains and the housing policies that influence them. A “Yes” identifies the presence of the policy in the state while a “No” represents a lack of the policy. A summary of the overall scores for each state is presented below, with a higher number representing an increased number of policies that promote economic mobility. Our results show the following:"),
                                                                p(img( src = "heat_table.jpg")),


                                                                p(strong("Housing Assistance:"), "Virginia performs the worst with a score of 0.50/1, and Iowa performs the best with a score of 1/1. While Oregon performs well with a score of 0.88, it still demonstrates room for improvement."),
                                                                p(strong("Housing Development:"), "Oregon and Iowa perform the same with a score of 0.67/1, and Iowa falls behind with a score of 0.56/1. All three states demonstrate room for improvement.   "),
                                                                p(strong("Financial:"), "Virginia performs the worst with a score of 0.44/1, and Iowa performs the best with a score of 0.67/1. All three states demonstrate room for improvement.  "),
                                                                p(strong("Overall:"), "under our scoring criteria, Iowa has the greatest number of policies that advance economic mobility while Virginia has the least, indicating that Virginia is the state with the most room for improvement. Oregon’s score falls in the middle of the two but is closer to Iowa than Virginia.")
                                                                ),
                                                         column(1))
                                                                ) # close anaylsis & results
                                     ) #close NavlistPanel to select sub-domain for Housing and Zoning heatmap
                                       ) #close fluid row
                          ), #close Housing and Zoning panel



#-- Education---------------------------------------------
                  tabPanel("Education",

                           fluidRow(
                             navlistPanel(
                               tabPanel("Composite Indicators",
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h4(strong("Education")),
                                                        column(1))),
                                        hr(),
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h5(strong("Composite Indicators")),
                                                        column(1))),
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."),
                                                        column(1))),

                                        fluidRow(
                                          sidebarPanel( width = 4,
                                                        selectInput("graphedu", "Subdomain",
                                                                    choices = c("Domain level", "School Climate Policies", "Early Childhood Education Policies", "Post-Secondary Affordability Policies", "Workforce Development Policies")
                                                        )),
                                          mainPanel(  uiOutput("imgedu"), align = "center"))
                               ),

                               tabPanel("Policy Asset Map",
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h4(strong("Education")),
                                                        column(1))),
                                        hr(),

                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h5(strong("Policy Asset Map")),
                                                        column(1))),
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h5("The policy asset maps below show a composite of 19 subcategories of education (derived from 73 education policy questions). \"1\" means the policy advances economic mobility, while <1 means the policy hinders economic mobility."
                                                 ),

                                                 column(1))),

                                        fluidRow(sidebarPanel( width = 4,
                                                               selectInput("grapheduheat", "Subdomain",
                                                                           choices = c("Domain level","School Climate Policies", "Early Childhood Education Policies", "Post-Secondary Affordability Policies", "Workforce Development Policies")
                                                               )) ,
                                                 mainPanel(uiOutput("imgeduheat"), align = "center"))
                                        ,
                                        fluidRow( mainPanel(img(height = 300, width = 400, src = "Education_heatmap.png"))
                                        )
                                        # Alternate way to show img that doesn't use the server?:
                                        #fluidRow( mainPanel(img(height = 300, width = 400, src = "taxation_heatmap.png")))
                               ), # close Asset Map tabPanel


                               tabPanel("Analysis & Results",
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h4(strong("Education")),
                                                        column(1))),
                                        hr(),

                                        fluidRow(width =4,
                                                 column(1),
                                                 column(12, h5(strong("Analysis and Results")),
                                                        column(1))),
                                        fluidRow(width =4,
                                                 column(1),
                                                 column(10,
                                                        p("The heatmap visualized the 19 subcategories within four domains and the education policies that influence them. Our results show the following:"),
                                                        p(img(height = 300, width = 600, align = "center", src = "educationtable.PNG")),


                                                        p(strong("School Climate:"), "Iowa has the lowest score of .37/1. This is primarily because Iowa does not have many laws/regulations for school climate as well as has practices of corporal punishment and a lack of limitations on suspensions and expulsion."),
                                                        p(strong("Early Childhood Education:"), "Virginia and Oregon scored the highest while Iowa scored slightly lower.  However, states vary in different subcategories from student-centric policies to ensuring teacher quality."),
                                                        p(strong("Post-Secondary Affordability:"), "Oregon scored the highest with .89/1 while Iowa and Virginia scored in the 60th percentile. This difference is primarily caused by the differences if states have considered and enacted free college policies as well as merit and need based policies. "),
                                                        p(strong("Workforce Development:"), "Oregon scored the lowest with a score of .47/1. This is primarily caused by not having a state statue define at least one financial aid program.  "),
                                                        p(strong("Overall:"), "under our scoring criteria, each state can improve in different subdomains as there is not a consistent state with the highest ranking. Iowa does have the lowest education capital score primarily because of school climate policies while Virginia has the highest score among these three states.")
                                                 ),
                                                 column(1))
                               ), # close anaylsis & results

                               tabPanel("Box Plot (Will take few seconds to load)",
                                        fluidRow(
                                          titlePanel("Box Plot of Education Policies"),
                                          mainPanel(uiOutput("bp")
                                          )
                                        ))
                               ) #close NavlistPanel to select sub-domain for Education heatmap
                             ) #close fluid row
                           ), #close Education panel




#-------Voting---------------------------------
                    tabPanel("Voting",

                             fluidRow(
                               navlistPanel(
                                 tabPanel("Composite Indicators",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Voting")),
                                                          column(1))),
                                          hr(),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Composite Indicators")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."),
                                                          column(1))),

                                          fluidRow(
                                            sidebarPanel( width = 4,
                                                          selectInput("graphvote", "Subdomain",
                                                                      choices = c("Domain level", "Voting Accessibility", "Voting Registration")
                                                          )),
                                            mainPanel(  uiOutput("imgvote"), align = "center"))
                                 ),

                                 tabPanel("Policy Asset Map",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Voting")),
                                                          column(1))),
                                          hr(),

                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Policy Asset Map")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5("The plots below demonstrate the existence (yes) or non-existence (no) of policies in the states considered based on the policy questions that were asked."
                                                   ),

                                                   column(1))),

                                          fluidRow(sidebarPanel( width = 4,
                                                                 selectInput("graphvoteheat", "Subdomain",
                                                                             choices = c("Domain level", "Voting Accessibility", "Voting Registration")
                                                                 )) ,
                                                   mainPanel(uiOutput("imgvoteheat"), align = "center"))
                                          ,
                                          fluidRow( mainPanel(img(height = 300, width = 400, src = "heat_map_vote.png"))
                                          )

                                          # Alternate way to show img that doesn't use the server?:
                                          #fluidRow( mainPanel(img(height = 300, width = 400, src = "taxation_heatmap.png")))
                                 ), # close Asset Map tabPanel


                                 tabPanel("Analysis & Results",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Voting")),
                                                          column(1))),
                                          hr(),

                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Analysis and Results")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(10,
                                                          p("The policy asset map visualizes the four subdomains and the tax policies that influence them. A “Yes” identifies the presence of the policy in the state while a “No” represents a lack of the policy. A summary of the overall scores for each state is presented below, with a higher number
                    representing an increased number of policies that promote economic mobility.
                                                                                               Our results show the following: "),
                                                          p(img( src = "heat_table.jpg")),
                                                          p(strong("Voting Accessibility:"), " Oregon and Iowa perform the _________ with a score of 0.__.
                                                                                               On the other hand, Virginia holds a _______ score of 0.__, highlighting a need for improvement."),
                                                          p(strong("Voting Registration"), "Oregon and Iowa perform the highest with a score of 0.__.
                                                                                               In contrast, Virginia holds the lowest score of 0.33, highlighting a need for improvement.  "),
                                                          p(strong("Overall:"), "___ states had a score of 0.__. All states could improve in making Voting policies that encourage Economic Mobility")
                                                   ),
                                                   column(1))

                                 ) # close anaylsis & results
                               ) #close NavlistPanel to select sub-domain for Voting heatmap
                             ) #close fluid row
                    ), #close Voting panel


#----Employment------------------------
                    tabPanel("Employment",

                             fluidRow(
                               navlistPanel(
                                 tabPanel("Composite Indicators",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Employment")),
                                                          column(1))),
                                          hr(),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Composite Indicators")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5("The plots below demonstrate numeric composites calculated using policy data for each subdomain, allowing for comparison between a given policy dimension in each state."),
                                                          column(1))),

                                          fluidRow(
                                            sidebarPanel( width = 4,
                                                          selectInput("graphemp", "Subdomain",
                                                                      choices = c("Domain level", "Worker Organizing Policies", "Worker Protections", "Wage Policies")
                                                          )),
                                            mainPanel(  uiOutput("imgemp"), align = "center"))
                                 ),

                                 tabPanel("Policy Asset Map",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Employment")),
                                                          column(1))),
                                          hr(),

                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Policy Asset Map")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5("The plots below demonstrate the existence (yes) or non-existence (no) of policies in the states considered based on the policy questions that were asked."
                                                   ),

                                                   column(1))),

                                          fluidRow(sidebarPanel( width = 4,
                                                                 selectInput("graphempheat", "Subdomain",
                                                                             choices = c("Domain level", "Worker Organizing Policies", "Worker Protections", "Wage Policies")
                                                                 )) ,
                                                   mainPanel(uiOutput("imgempheat"), align = "center"))
                                          ,
                                          fluidRow( mainPanel(img(height = 300, width = 400, src = "heat_map_employment.png"))
                                          )
                                          # Alternate way to show img that doesn't use the server?:
                                          #fluidRow( mainPanel(img(height = 300, width = 400, src = "taxation_heatmap.png")))
                                 ), # close Asset Map tabPanel


                                 tabPanel("Analysis & Results",
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h4(strong("Employment")),
                                                          column(1))),
                                          hr(),

                                          fluidRow(width =4,
                                                   column(1),
                                                   column(12, h5(strong("Analysis and Results")),
                                                          column(1))),
                                          fluidRow(width =4,
                                                   column(1),
                                                   column(10,
                                                          p("The policy asset map visualizes the three subdomains and  17 employment policy questions. A “Yes” identifies the presence of the policy in the state while a “No” represents a lack of the policy. A summary of the overall scores for each state is presented below, with a higher number
                              representing an increased number of policies that promote economic mobility.
                                                                                                         Our results show the following: "),
                                                          p(img( src = "heat_table.jpg")),
                                                          p(strong("Worker Organizing:"), " Oregon and Iowa perform the _________ with a score of 0.__.
                                                                                                         On the other hand, Virginia holds a _______ score of 0.__, highlighting a need for improvement."),
                                                          p(strong("Worker Protections:"), "Oregon and Iowa perform the highest with a score of 0.__.
                                                                                                         In contrast, Virginia holds the lowest score of 0.33, highlighting a need for improvement.  "),
                                                          p(strong("Wage:"), "Oregon and Iowa perform the highest with a score of 0.__.
                                                                                                         In contrast, Virginia holds the lowest score of 0.33, highlighting a need for improvement.  "),
                                                          p(strong("Overall:"), "___ states had a score of 0.__. All states could improve in making Voting policies that encourage Economic Mobility")
                                                   ),
                                                   column(1))

                                 ) # close anaylsis & results
                               ) #close NavlistPanel to select sub-domain for Employment heatmap
                             ) #close fluid row
                    ) #close Employment panel


), #Close narbarMenu for Domain of Analysis






             navbarMenu(h4("Data, Measures & Methods"),
                        tabPanel("Summary",
                                 fluidRow(
                                   navlistPanel(
                                     tabPanel( "Scoring Methods",
                                               width =12,
                                               column(1),
                                               column(10, h3(strong( "Scoring Methods")),
                                                      hr(),
                                                      p("After an extensive research effort, our team decided to score states with a composite indicator system to measure whether states have policies likely to advance economic mobility. We rooted our composite indicator system in dummy variables to make use of qualitative information."),
                                                      br(),
                                                      p("If the existence of a state’s policy exhibits potential to advance economic mobility, we assigned a value of 1. If the state lacks a particular law or regulation, we assign 0.  We", strong("assumed"), "if a state does not have laws or regulations, more variability could occur with respect to mobility and, consequently, improvement of socio-economic advancement may be delayed.  For instance, Student Discipline is one of the sub-domains identified within political capital of Education. We included multiple questions for Student Discipline, such as: “Is there a ban on corporal punishment?” According to Brookings, students subject to corporal punishment performed worse than their peers in non-punitive environments. If a state banned corporal punishment, we ranked the state with 1. If they did not ban corporal punishment, we ranked the state with 0."),
                                                      br(),
                                                      p("Some subcategories have multiple questions. In order to standardize, we summed the scores across the policy questions within the subcategory by state, then divided by the number of questions in the subcategory.  A similar approach was used for subdomain scores. Our final index also has a similar approach.   The following table summarizes each domain of political capital and the number of subdomains, subcategories and policy questions.
                                                      ")),
                                                      column(1),
                                               img(height = 390, width = 890, src = "domain_category_counts_table.png", align = "center")
                                     ), #close tab

                                     tabPanel( "View the Data",
                                               width =12,
                                               column(1),
                                               column(10,
                                                      h1(strong("All Data")),
                                                      hr(),
                                                      DT::dataTableOutput("all_data_table"),
                                                      column(1))
                                     ) #close tab

                                   ) #close navlistPanel
                                 ) #close fluidrow







                        ), #Close Summary tab

                        tabPanel("Law Enforcement",

                                 fluidRow(
                                   navlistPanel(
                                     tabPanel( "Background",
                                               width =12,
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
                                                 p("c. Community Policing Practices- Community Policing Practices explores the standards that officers must abide by in policing the community with a focus on the equality of the standards. For example, custodial sexual misconduct policies are analysed, both numerically and qualitatively, to assess how states hold officers accountable for allegations of misconduct against them by an individual in their custody. In addition, body camera usage, demographic information collection and domestic violence- related polices are considered in this subdomain. We also qualitatively assess the nature of officer training programmes in the states, particularly those pertaining to treating individuals with mental health issues.  ")),
                                          column(1)
                                   ),

                                   tabPanel("Data Sources & References",


                                            fluidRow(width =12,
                                                     column(1),
                                                     column(10, h3(strong("Data Sources and References")),

                                                            br(),
                                                            h3("Data Sources"),
                                                            tags$a(href="https://www.ecs.org/research-reports/key-issues/postsecondary-affordability/", "Education Commission of The States: Postsecondary Affordability"),
                                                            br(),
                                                            tags$a(href="https://www.ecs.org/research-reports/key-issues/early-childhood-education/", "Education Commission of The States: Early Childhood Education"),
                                                            br(),
                                                            tags$a(href="https://www.ecs.org/research-reports/key-issues/workforce-development/", "Education Commission of The States: Workforce Development"),
                                                            br(),
                                                            tags$a(href="https://www.ecs.org/research-reports/key-issues/school-climate/", "Education Commission of The States: School Climate"),
                                                            br(),
                                                            tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Oregon%20School%20Discipline%20Laws%20and%20Regulations.pdf/", "Oregon Compilation of School Discipline Laws and Regulations"),
                                                            br(),
                                                            tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Virginia%20School%20Discipline%20Laws%20and%20Regulations.pdf", "Virginia Compilation of School Discipline Laws and Regulations"),
                                                            br(),
                                                            tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Iowa%20School%20Discipline%20Laws%20and%20Regulations.pdf", "Iowa Compilation of School Discipline Laws and Regulations"),
                                                            br(),
                                                            h3("References"),

                                                            tags$a(href="https://www.brookings.edu/research/hitting-kids-american-parenting-and-physical-punishment/", "Brookings Corporal Punishment"),
                                                            br(),
                                                            tags$a(href="https://www.pnas.org/content/116/17/8255", "PNAS Corporal Punishment"),
                                                            br(),
                                                            tags$a(href="https://www.ecs.org/50-state-comparison-postsecondary-education-funding/", "ECS Early Childhood Programs as Economic Development Tool"),
                                                            br(),
                                                            tags$a(href="https://cew.georgetown.edu/cew-reports/recovery-job-growth-and-education-requirements-through-2020/", "Georgetown Job Growth and Education Requirements through 2020"),
                                                            br(),
                                                            tags$a(href="https://www.luminafoundation.org/news-and-views/does-higher-education-really-increase-economic-mobility/", "Lumina Foundation: Does higher education really increase economic mobility?")
                                                     )
                                                     ) #close fluid row
                                            ), # Data Sources & References panel

                                   tabPanel("View the Data",


                                            fluidRow(width =12,
                                                     column(1),
                                                     column(10, h3(strong("Law Enforcement Data Set")),
                                                            DT::dataTableOutput("lawtable")

                                                     )
                                            )#close fluidrow
                                   ) # close Data tab

                                   ) #close navlistPanel
                                   ) #close fluid row
                                 ), # Close Law Enforcement tabPanel



#----Tax ----------------
                      tabPanel("Taxation",

                               fluidRow(
                                 navlistPanel(
                                   tabPanel( "Background",
                                             width =12,
                                             column(1),
                                             column(10, h3(strong( "Taxation")),
                                                    hr(),
                                                    strong("Background"),
                                                    p("In examining how policies affect economic mobility, it is crucial to further explore taxation policy.
                                                   Taxation boosts economic mobility because it influences individuals’ accumulation of wealth and affects absolute or relative mobility,
                                                   whether that be inter or intra generational. Tax revenues are used to fund goods that drive mobility, such as education and health, and tax deduction and credit policies
                                                   lower the cost of mobility enhancing goods. Taxation can also lead to the redistribution of wealth, an important part of combating wealth inequality."),
                                                    p("Since 1979, income inequality in the United States has increased dramatically. In every state, the average income of the top 5% of households is at least 10 times
                                                   that of the bottom 20%. State taxes often work to further this inequity because of their regressive structure. Furthermore, state taxes disproportionately
                                                   affect higher income families and individuals. Thus, it is vital to understand how states can implement more progressive tax policies and re-evaluate regressive
                                                   structures to boost overall economic mobility. Our research identified four main subdomains of tax policy: tax credits, wealth-related taxes, business tax policy, and the Gini index."),
                                                    p(strong("a. Tax credits"), "are negative marginal tax rates, or tax incentives, that reduce tax liability and boost tax refunds, therefore improving economic mobility for low-income individuals.
                                                   They ease low- to moderate-income family burdens by providing appropriate financial support for expenses like childcare, income tax, and property tax.  "),
                                                    p(strong("b: Taxes on inherited wealth"), "such as the estate and inheritance tax, largely only affect the wealthiest taxpayers. These taxes help redistribute income and wealth and thus improve economic mobility for the millions of low- and middle-class Americans.
                                                   Because wealth is concentrated in the hands of few mostly white Americans, wealth-related taxes help upend financial barriers for low-income people, who are predominantly people of color.  "),
                                                    p(strong("c: Businesses"), "create opportunities for employment, thus increasing incomes, and provide access to services that increase future earning potentials.
                                                   States play a significant role in supporting businesses by nullifying corporate tax avoidance strategies to equalize the playing field between multimillion-dollar corporations and small businesses, as well as creating a tax climate that fosters entrepreneurial efforts. "),
                                                    p(strong("d.  The Gini coefficient"), "is a measure of statistical dispersion intended to represent income or wealth inequality in a nation or area. Because the Gini coefficient measures inequality after the effects of taxes, by understanding how Gini indexes change as a result
                                                   of tax policies and financial redistribution, we can better understand how tax policy can support economic mobility.  ")
                                             ),
                                             column(1)),

                                   tabPanel("Data Sources & References",


                                            fluidRow(width =12,
                                                     column(1),
                                                     column(10, h3(strong("Data Sources and References")),
                                                            br(),

                                                            h3("Data Sources"),
                                                            tags$a(href="https://www.ecs.org/research-reports/key-issues/postsecondary-affordability/", "Education Commission of The States: Postsecondary Affordability"),
                                                            br(),
                                                            tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Iowa%20School%20Discipline%20Laws%20and%20Regulations.pdf", "Iowa Compilation of School Discipline Laws and Regulations"),
                                                            br(),

                                                            h3("References"),
                                                            tags$a(href="https://www.brookings.edu/research/hitting-kids-american-parenting-and-physical-punishment/", "Brookings: Corporal Punishment"),
                                                            br(),
                                                            tags$a(href="https://www.luminafoundation.org/news-and-views/does-higher-education-really-increase-economic-mobility/", "Lumina Foundation: Does higher education really increase economic mobility?")
                                                     )
                                            ) #close fluid row
                                   ), # Data Sources & References panel

                                   tabPanel("View the Data",
                                            fluidRow(width =12,
                                                     column(1),
                                                     column(10, h3(strong("Taxation Data Set")),
                                                            DT::dataTableOutput("taxtable")

                                                     )
                                            )#close fluidrow
                                   ) # close Data tab

                                   ) #close navlistPanel
                                 ) #close fluid row
                               ), # Close Taxation tabPanel



#---Housing------------------
                      tabPanel("Housing & Zoning",

                               fluidRow(
                                 navlistPanel(
                                   tabPanel( "Background",
                                             width =12,
                                             column(1),
                                             column(10, h3(strong( "Housing & Zoning")),
                                                    hr(),
                                                    strong("Background"),
                                                    p("When trying to understand how policies affect economic mobility, it’s clear policies that ensure affordable housing access are crucial. Any city that hopes to promote economic mobility must prioritize fundamental needs of all its citizens, including shelter. Many cities are struggling to keep housing prices low and some even intentionally participate in strategies to drive up housing prices and drive out people of lower socioeconomic status. We researched various housing and zoning policies to better understand which work against economic mobility and which promote it."),
                                                    br(),
                                                    p("Our research identified three main subdomains within housing and zoning policy: assistance policies, financial policies, and development policies."),
                                                    br(),
                                                    p(strong("Assistance"), "policies are programs and discounts which aid in reducing the cost of housing for disadvantaged individuals. Loan assistance programs for disabled members and first-time homeowners are examples."),
                                                    br(),
                                                    p("Housing", strong("Financial"), " policy describes policies which aid in covering costs to help provide a fair financial environment when purchasing or renting homes. This includes loan assistance programs, home price discounts and tax exemptions. By understanding housing financial policies and their effects on communities, we can understand which policies cultivate the ideal environment for economic mobility."),
                                                    br(),
                                                    p(strong("Development"), " policies are land use and planning regulations that influence the cost and equity of housing. Restricting the development of multi-unit housing, for example, can drive up the cost of housing. ")


                                                                   ),
                                                                   column(1)),

                                                         tabPanel("Data Sources & References",


                                                                  fluidRow(width =12,
                                                                           column(1),
                                                                           column(10, h3(strong("Data Sources and References")),
                                                                                  br(),

                                                                                  h3("Data Sources"),
                                                                                  tags$a(href="[url]", "_________: \"_________\""),
                                                                                  br(),
                                                                                  tags$a(href="[url]", "_____________namw/article____"),
                                                                                  br(),

                                                                                  h3("References"),
                                                                                  tags$a(href="https://www.urban.org/sites/default/files/alfresco/publication-pdfs/2000428-Housing-Policy-Levers-to-Promote-Economic-Mobility.pdf", "Urban Institute: \"HousingPolicy Levers to Promote Economic Mobility\""),
                                                                                  br(),
                                                                                  tags$a(href="[url]", "_____ Foundation: ________?")
                                                                           )
                                                                  ) #close fluid row
                                                         ), # Data Sources & References panel

                                                         tabPanel("View the Data",
                                                                  fluidRow(width =12,
                                                                           column(1),
                                                                           column(10, h3(strong("Housing & Zoning Data Set")),
                                                                                  DT::dataTableOutput("housetable")

                                                                           )
                                                                  )#close fluidrow
                                                         ) # close Data tab

                                                         ) #close navlistPanel
                                                       ) #close fluid row
                                                     ), # Close Housing/ tabPanel




#---------Education---------------

          tabPanel("Education",

                   fluidRow(
                     navlistPanel(
                       tabPanel( "Background",
                                 width =12,
                                 column(1),
                                 column(10, h3(strong( "Education")),
                                        hr(),
                                        strong("Background"),
                                        p("Education is a key aspect of enabling economic mobility. Education is multi-faceted and different policies can enable or hinder crucial components of economic mobility. For example, Timothy Bartik (Senior Economist at W.E. Upjohn Institute for Employment Research) states that for every one dollar invested in high quality early childhood programs, a state economy will get a two to three dollar return on investment. Furthermore, a finding from Georgetown University’s Center on Education and the Workforce, projected that 64 percent of the job openings in 2020 will require some post-secondary education."),
                                        p("With guidance in our literature review from examples like these, our team identified four main subdomains within education: school climate, early childhood education, post-secondary affordability, and workforce development. Within these four main subdomains, we found 19 subcategories which are derived from 73 policy questions.  "),
                                        p("a. As defined by the National School Climate Center, “School climate refers to the quality and character of school life. School climate is based on patterns of students', parents' and school personnel's experience of school life and reflects norms, goals, values, interpersonal relationships, teaching and learning practices, and organizational structures. A sustainable, positive school climate fosters youth development and learning necessary for a productive, contributing and satisfying life in a democratic society.” School climate policies groups them by disciplinary approaches addressing suspensions, specific infractions and conditions; prevention and non-punitive behavioral interventions; monitoring and accountability; school resources for safety and truant/attendance officers; and state education agency support."),
                                        p("b. Early childhood education includes those school years from pre-kindergarten to the third grade. Early childhood education policies groups them by kindergarten requirements; teacher quality; school readiness and transitions; assessment intervention and retention; family engagement; and social-emotional learning."),
                                        p("c. Post-secondary education is the educational level following the completion of secondary education (high school) Post-secondary education includes non-degree credentials such as certifications, licenses, and work experience programs, as well as college and professional degrees.  Post-secondary affordability policies grouped them by, need and merit based financial aid; financial aid; and free college."),
                                        p("d. The Federal Workforce Innovation and Opportunity Act (WIOA) encourages state policymakers to seek ways to connect education, job seekers, and employers in their states by developing a one-stop delivery system that provides information on career and training services, access to employer programs and activities, and access to real-time labor market information. Workforce development policies grouped them by, statewide apprenticeships; connecting education to work; and post-secondary career and technical education.")
                                 ),
                                 column(1),
                                 fluidRow(width = 12, style = "margin: 20px",
                                plotOutput("word_cloud", height = '700px'))
                                 ),

                       tabPanel("Data Sources & References",
                                fluidRow(width =12,
                                         column(1),
                                         column(10, h3(strong("Data Sources and References")),
                                                br(),

                                                h3("Data Sources"),
                                                tags$a(href="[url]", "_________: \"_________\""),
                                                br(),
                                                tags$a(href="[url]", "_____________namw/article____"),
                                                br(),

                                                h3("References"),
                                                tags$a(href="https://www.urban.org/sites/default/files/alfresco/publication-pdfs/2000428-Housing-Policy-Levers-to-Promote-Economic-Mobility.pdf", "Urban Institute: \"HousingPolicy Levers to Promote Economic Mobility\""),
                                                br(),
                                                tags$a(href="[url]", "_____ Foundation: ________?")
                                         )
                                ) #close fluid row
                       ), # Data Sources & References panel

                       tabPanel("View the Data",
                                fluidRow(width =12,
                                         column(1),
                                         column(10, h3(strong("Education Data Set")),
                                                DT::dataTableOutput("edutable")

                                         )
                                )#close fluidrow
                       ) # close Data tab

                     ) #close navlistPanel
                   ) #close fluid row
          ), # Close Education tabPanel




#-------Voting -----
                tabPanel("Voting",

                         fluidRow(
                           navlistPanel(
                             tabPanel( "Background",
                                       width =12,
                                       column(1),
                                       column(10, h3(strong( "Voting")),
                                              hr(),
                                              strong("Background"),
                                              p(""),
                                              br(),
                                              p("")

                                       ),
                                       column(1)),

                             tabPanel("Data Sources & References",


                                      fluidRow(width =12,
                                               column(1),
                                               column(10, h3(strong("Data Sources and References")),
                                                      br(),

                                                      h3("Data Sources"),
                                                      tags$a(href="[url]", "_________: \"_________\""),
                                                      br(),
                                                      tags$a(href="[url]", "_____________namw/article____"),
                                                      br(),

                                                      h3("References"),
                                                      tags$a(href="https://www.urban.org/sites/default/files/alfresco/publication-pdfs/2000428-Housing-Policy-Levers-to-Promote-Economic-Mobility.pdf", "Urban Institute: \"HousingPolicy Levers to Promote Economic Mobility\""),
                                                      br(),
                                                      tags$a(href="[url]", "_____ Foundation: ________?")
                                               )
                                      ) #close fluid row
                             ), # Data Sources & References panel

                             tabPanel("View the Data",
                                      fluidRow(width =12,
                                               column(1),
                                               column(10, h3(strong("Voting Data Set")),
                                                      DT::dataTableOutput("votetable")

                                               )
                                      )#close fluidrow
                             ) # close Data tab

                           ) #close navlistPanel
                         ) #close fluid row
), # Close Housing/ tabPanel

#-----------Employment
                tabPanel("Employment",

                         fluidRow(
                           navlistPanel(
                             tabPanel( "Background",
                                       width =12,
                                       column(1),
                                       column(10, h3(strong( "Employment")),
                                              hr(),
                                              strong("Background"),
                                              p(""),
                                              br(),
                                              p("")

                                       ),
                                       column(1)),

                             tabPanel("Data Sources & References",


                                      fluidRow(width =12,
                                               column(1),
                                               column(10, h3(strong("Data Sources and References")),
                                                      br(),

                                                      h3("Data Sources"),
                                                      tags$a(href="[url]", "_________: \"_________\""),
                                                      br(),
                                                      tags$a(href="[url]", "_____________namw/article____"),
                                                      br(),

                                                      h3("References"),
                                                      tags$a(href="https://www.urban.org/sites/default/files/alfresco/publication-pdfs/2000428-Housing-Policy-Levers-to-Promote-Economic-Mobility.pdf", "Urban Institute: \"HousingPolicy Levers to Promote Economic Mobility\""),
                                                      br(),
                                                      tags$a(href="[url]", "_____ Foundation: ________?")
                                               )
                                      ) #close fluid row
                             ), # Data Sources & References panel

                             tabPanel("View the Data",
                                      fluidRow(width =12,
                                               column(1),
                                               column(10, h3(strong("Employment Set")),
                                                      DT::dataTableOutput("emptable")

                                               )
                                      )#close fluidrow
                             ) # close Data tab

                           ) #close navlistPanel
                         ) #close fluid row
                ) # Close Employment tabPanel


), #Close NavbarMenu for Data & Methods




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
                                                      width = "90px", height = "100px")
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
                                             "Riya Berry is a rising junior at UC Berkeley studying Data Science and Interdisciplinary Studies with an emphasis on Societal Inequalities ")
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
                                                      width = "85px", height = "105px")),
                                           div(
                                             tags$h5("Tasfia Chowdhury"),
                                             tags$h6( tags$i("Intern"))
                                           ),
                                           div(
                                             "Tasfia Chowdhury is a rising senior at Indiana University Bloomington studying political science and epidemiology.")
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
                                             tags$img(src = "vicki.jpg",
                                                      width = "85px", height = "100px")
                                           ),
                                           div(
                                             tags$h5("Vicki Lancaster"),
                                             tags$h6( tags$i("Principal Investigator"))
                                           ),
                                           div(
                                             "Vicki Lancaster is a Principal Scientist at the Social & Decision Analytics Division (SDAD) of the Biocomplexity Institute & Initiative at the University of Virginia.
                                             Her areas of expertise are experimental design, visualizations, data analysis and interpretation. "
                                           )
                                       )
                                   )
                            ),
                            column(1),


                            # Cesar
                            column(2,
                                   div(class="panel panel-default",
                                       div(class="panel-body", width = "600px",
                                           align = "center",
                                           div(
                                             tags$img(src = "cm.png",
                                                      width = "90px", height = "100px")
                                           ),
                                           div(
                                             tags$h5("Cesar Montalvo"),
                                             tags$h6( tags$i("Postdoctoral Research Associate"))
                                           ),
                                           div(
                                             "Cesar Montalvo is a Postdoctoral Research Associate in the Social and Decision Analytics Division at the Biocomplexity Institute and Initiative from the UVA.  Master’s degree in economics from Iowa State University, Ph.D. in Applied Mathematics
                                             for Life and Social Sciences from Arizona State University.
                                             He works at the interface of economics, statistics, mathematical models and public policy."
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
                      )
                      ),  #close "Project Team panel

             tabPanel(h4("Acknowledgements & Contacts"),
                      fluidRow(width = 12,
                               column(1, align = "center", h3(strong("Approach"))),
                               column(1)))
             )#close NavbarPage for overall tabs on top of page
) # close UI fluid page




# server-----------------------------------------------------------------------------------------


server <- shinyServer(function(input,output){

  #getPage<-function() {
   # return(includeHTML("boxplot.html"))
  #}
  #output$inc<-renderUI({getPage()})

  output$bp <- renderUI({
    includeHTML("boxplot.html")
  })



  # Law Enforcement Plots Rendering
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



  # Taxation Plots Rendering
  output$imgtax <- renderUI({
    if(input$graphlaw == "Domain level"){
      img(height = 300, width = 400, src = "law_domain_plot.png", align = "left")
    }

    else if(input$graphtax == "Tax Credits"){
      img(height = 300, width = 400, src = "law_sub_arrest.png")
    }
    else if(input$graphtax == "Taxes on Wealth"){
      img(height = 300, width = 400, src = "law_sub_incarceration.png")
    }
    else if(input$graphtax == "Taxes Related to Business"){
      img(height = 300, width = 400, src = "law_sub_community.png")
    }
    else if(input$graphtax == "Gini Index"){
      img(height = 300, width = 400, src = "law_sub_community.png")
    }
  })

  output$imgtaxheat <- renderUI({
    if(input$graphtaxheat == "Domain level"){
      img(height = 300, width = 400, src = "taxation_heatmap.png", align = "left")
    }
    else if(input$graphtaxheat == "Tax Credits"){
      img(height = 300, width = 400, src = "tax_health_credits")
    }
    else if(input$graphtaxheat == "Taxes on Wealth"){
      img(height = 300, width = 400, src = "tax_health_wealth.png")
    }
    else if(input$graphtaxheat == "Taxes Related to Business"){
      img(height = 300, width = 400, src = "tax_heat_business.png")
    }
    else if(input$graphtaxheat == "Gini Index"){
      img(height = 300, width = 400, src = "tax_heat_gini.png")
    }
  })

  output$imgtaxwheat <- renderUI({
    if(input$graphtaxheat == "Domain level"){
      img(height = 300, width = 400, src = "arrest_heat.png", align = "left")
    }
    else if(input$graphtaxheat == "Arrest and Court Proceedings"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphtaxheat == "Incarceration Practices"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphtaxheat == "Community Policing"){
      img(height = 300, width = 400, src = "")
    }
  })




  # Housing & Zoning Plots Rendering
  output$imglaw <- renderUI({
    if(input$graphhouse == "Domain level"){
      img(height = 300, width = 400, src = "law_domain_plot.png", align = "left")
    }

    else if(input$graphhouse == "Arrest and Court ProceedingsHousing Assistance Policies"){
      img(height = 300, width = 400, src = "law_sub_arrest.png")
    }
    else if(input$graphhouse == "Housing Development Policies"){
      img(height = 300, width = 400, src = "law_sub_incarceration.png")
    }
    else if(input$graphhouse == "Housing Financial Policies"){
      img(height = 300, width = 400, src = "law_sub_community.png")
    }
  })

  output$imghouseheat <- renderUI({
    if(input$graphhouseheat == "Domain level"){
      img(height = 300, width = 400, src = "arrest_heat.png", align = "left")
    }
    else if(input$graphhouseheat == "Housing Assistance Policies"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphhouseheat == "Housing Development Policies "){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphhouseheat == "Housing Financial Policies"){
      img(height = 300, width = 400, src = "")
    }
  })





  # Employment Plots Rendering
  output$imgemp <- renderUI({
    if(input$graphemp == "Domain level"){
      img(height = 300, width = 400, src = "employment_domain_plot.png")
    }
    else if(input$graphemp == "Worker Organizing Policies"){
      img(height = 300, width = 400, src = "employment_sub_org.png")
    }
    else if(input$graphemp == "Worker Protections"){
      img(height = 300, width = 400, src = "employment_sub_protect.png")
    }
    else if(input$graphemp == "Wage Policies"){
      img(height = 300, width = 400, src = "employment_sub_wage.png")
    }
  })

  output$imgempheat <- renderUI({
    if(input$graphempheat == "Domain level"){
      img(height = 300, width = 400, src = "heat_map_employment.png", align = "left")
    }
    else if(input$graphempheat == "Worker Organizing Policies"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphempheat == "Worker Protections"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphempheat == "Wage Policies"){
      img(height = 300, width = 400, src = "")
    }
  })



  # Voting Plots Rendering
  output$imgvote <- renderUI({
    if(input$graphvote == "Domain level"){
      img(height = 300, width = 400, src = "vote_domain_plot.png")
    }
    else if(input$graphvote == "Voting Accessibility"){
      img(height = 300, width = 400, src = "vote_sub_access.png")
    }
    else if(input$graphvote == "Voting Registration"){
      img(height = 300, width = 400, src = "vote_sub_reg.png")
    }
  })

  output$imgvoteheat <- renderUI({
    if(input$graphvoteheat == "Domain level"){
      img(height = 300, width = 400, src = "Voting HeatMap.png", align = "left")
    }
    else if(input$graphlawheat == "Voting Accessibility"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$graphlawheat == "Voting Registration"){
      img(height = 300, width = 400, src = "")
    }
  })


  # Education Plots Rendering
  output$imgedu <- renderUI({
    if(input$graphedu == "Domain Level"){
      img(height = 300, width = 400, src = "edu_composite.png")
    }
    else if(input$graphedu == "School Climate Policies"){
      img(height = 300, width = 400, src = "edu_schoolclimate.png")
    }
    else if(input$graphedu == "Early Childhood Education Policies"){
      img(height = 300, width = 400, src = "edu_earlychildhood.png")
    }
    else if(input$graphedu == "Post-Secondary Affordability Policies"){
      img(height = 300, width = 400, src = "edu_postsecondaryafford.png")

    }
    else if(input$graphedu == "Workforce Development Policies"){
      img(height = 300, width = 400, src = "education_workforcedev.png")
    }
  })

  output$imgeduheat <- renderUI({
    if(input$grapheduheat == "Domain level"){
      img(height = 300, width = 400, src = "Education_heatmap.png", align = "left")
    }
    else if(input$grapheduheat == "School Climate Policies"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$grapheduheat == "Early Childhood Education Policies"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$grapheduheat == "Post-Secondary Affordability Policies"){
      img(height = 300, width = 400, src = "")
    }
    else if(input$grapheduheat == "Workforce Development Policies"){
      img(height = 300, width = 400, src = "edu_heatmap_workforcedev.png")
    }
  })


  output$all_data_table <- DT::renderDataTable({
    all_data
  })

  output$lawtable = DT::renderDataTable({
    law_dt_data

  })
  output$taxtable = DT::renderDataTable({
    tax_dt_data

  })
  output$housetable = DT::renderDataTable({
    house_dt_data

  })
  output$edutable = DT::renderDataTable({
    edu_dt_data

  })
  output$votetable = DT::renderDataTable({
    vote_dt_data

  })
  output$emptable = DT::renderDataTable({
    emp_dt_data
  })


})


# Run the application
shinyApp(ui = ui, server = server)

