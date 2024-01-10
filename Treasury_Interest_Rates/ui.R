#
# This App takes in data from the Treasury API and looks at trends in fiscal data

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("US Treasury Fiscal Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("About", ),
                tabPanel("Data Exploration", ),
                tabPanel("Modeling", tabsetPanel(
                    type = "tabs",
                    tabPanel("Modeling Info", ),
                    tabPanel("Model Fitting", ),
                    tabPanel("Prediction", )
                ))
            )
        )
    )
)




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