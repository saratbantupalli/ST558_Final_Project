#
# This App takes in data from the Treasury API and looks at trends in fiscal data

library(shiny)
#library(bslib)

# UI for Treasury Data
fluidPage(

    # Application title
    titlePanel(h1(strong("App to Connect to an API, Model Data, and Summarize"))),
    navbarPage("Treasury API App",
               tabPanel("About", 
                        htmlOutput("app_purpose"),
                        htmlOutput("picture"),
                        htmlOutput("note")),
               navbarMenu("Data Exploration",
                          tabPanel("Numerical Summary",
                                   radioButtons("summary_type","Summary type",
                                                choices = c("Mean", "Median",
                                                            "Maximum", "Minimum")),
                                   selectInput("type_of_security_numerical", "Security", selected = "All",
                                               choices = c("All",
                                                 "Treasury Bills",
                                                 "Treasury Notes",
                                                 "Treasury Bonds",
                                                 "Treasury Inflation-Protected Securities (TIPS)",
                                                 "Treasury Floating Rate Notes (FRN)",
                                                 "Federal Financing Bank",
                                                 "Total Marketable")),
                                   DT::dataTableOutput("numerical_summary")),
                          tabPanel("Graphical Summary",
                                   selectInput("type_of_plot", "Plot Type",
                                               choices = c("Trend Plot")),
                                   selectInput("type_of_security_graphical", "Security", selected = "All",
                                               choices = c("All",
                                                           "Treasury Bills",
                                                           "Treasury Notes",
                                                           "Treasury Bonds",
                                                           "Treasury Inflation-Protected Securities (TIPS)",
                                                           "Treasury Floating Rate Notes (FRN)",
                                                           "Federal Financing Bank")),
                                   plotOutput("trend_plot")))
    )
)



# navbarMenu("More",
#            tabPanel("Table",
#                     DT::dataTableOutput("table")
#            ),
#            tabPanel("About",
#                     fluidRow(
#                       column(6,
#                              includeMarkdown("about.md")
#                       ),
#                       column(3,
#                              img(class="img-polaroid",
#                                  src=paste0("http://upload.wikimedia.org/",
#                                             "wikipedia/commons/9/92/",
#                                             "1919_Ford_Model_T_Highboy_Coupe.jpg")),
#                              tags$small(
#                                "Source: Photographed at the Bay State Antique ",
#                                "Automobile Club's July 10, 2005 show at the ",
#                                "Endicott Estate in Dedham, MA by ",
#                                a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
#                                  "User:Sfoskett")
#                              )
#                       )
#                     )
#            )






# mainPanel(
#     tabsetPanel(
#         type = "tabs",
#         tabPanel("Plot", plotOutput("plot")),
#         tabPanel("Summary", tableOutput("summary")),
#         tabPanel("Data", DT::dataTableOutput("data")),
#         tabPanel(
#             "Reference",
#             tags$p(
#                 "There data were obtained from",
#                 tags$a("IMDB", href = "http://www.imdb.com/"), "and",
#                 tags$a("Rotten Tomatoes", href = "https://www.rottentomatoes.com/"), "."
#             ),
#             tags$p(
#                 "The data represent", nrow(movies),
#                 "randomly sampled movies released between 1972 to 2014 in the United States."
#             )
#         )
#     )
# )



# Sidebar with a slider input for number of bins
# sidebarLayout(
#     sidebarPanel(
#         sliderInput("bins",
#                     "Number of bins:",
#                     min = 1,
#                     max = 50,
#                     value = 30)
#     ),

# 
# mainPanel(
#     tabsetPanel(
#         type = "tabs",
#         tabPanel("About",
#                  br(),
#                  h3("Purpose of the App"),
#                  textOutput("about_purpose"),
#                  textOutput("about_data")), # About tab
#         tabPanel("Data Exploration", ),
#         tabPanel("Modeling", tabsetPanel(
#             type = "tabs",
#             tabPanel("Modeling Info", ),
#             tabPanel("Model Fitting", ),
#             tabPanel("Prediction", )
#         ))
#     )
# )
# )


### Works 2024-01-21
# titlePanel(h1(strong("US Treasury Fiscal Data"))),
# navbarPage(title = "",
#            navbarMenu(h3("About"),
#                       tabPanel(h4("Purpose of App"),
#                                htmlOutput("app_purpose")),
#                       tabPanel(h4("About Data"),
#                                htmlOutput("data_source")),
#                       tabPanel(h4("App Navigation"),
#                                htmlOutput("purpose_of_tabs"))
#                       
#            )
#            
#            
# )
# 
# )