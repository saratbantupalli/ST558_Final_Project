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
                        htmlOutput("note"))
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