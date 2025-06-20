
# # Purpose of app- develop a R Shiny app that connects to a data source (here an API),
# generates interactive numerical and graphical summaries, builds prediction models


# # What the App does- This app reads data from US Treasury API, transforms the data into
# usable form, generates interactive numerical and graphical summaries, then builds 
# prediction models


# Required packages for running the UI side of the app
library(shiny)

# Code for UI 

fluidPage(
    # Application title
    # h1 creates a level 1 header, strong makes the text bold
    titlePanel(h1(strong("App to Connect to an API, Model Data, and Summarize"))),
    # Create a page that contains a top level navigation bar
    navbarPage("Treasury API App",
               tabPanel("About", 
                        htmlOutput("app_purpose"),
                        htmlOutput("picture"),
                        htmlOutput("note")),
               navbarMenu("Data Exploration",
                          tabPanel("Numerical Summary",
                                   radioButtons("summary_type", 
                                                strong("Summary type"),
                                                choices = 
                                                  c("Mean", "Median",
                                                    "Maximum", "Minimum")),
                                   selectInput(
                                     "type_of_security_numerical", 
                                     strong("Security"), 
                                     selected = "All",
                                     choices = 
                                       c("All", "Treasury Bills",
                                         "Treasury Notes",
                                         "Treasury Bonds",
                                         "Treasury Inflation-Protected Securities (TIPS)",
                                         "Treasury Floating Rate Notes (FRN)",
                                         "Federal Financing Bank",
                                         "Total Marketable")),
                                   mainPanel(DT::dataTableOutput("numerical_summary"))),
                          tabPanel("Graphical Summary",
                                   selectInput("type_of_plot", 
                                               strong("Plot Type"),
                                               choices = 
                                                 c("Securities Trend Plot", 
                                                   "Securities Histogram",
                                                   "Total Debt and Treasury Security Interest Rates",
                                                   "Correlation Plot")),
                                   conditionalPanel(
                                     condition = 
                                     "input.type_of_plot == 'Securities Trend Plot'|
                                     input.type_of_plot == 'Securities Histogram'|
                                     input.type_of_plot == 'Total Debt and Treasury Security Interest Rates'",
                                     selectInput(
                                       "type_of_security_graphical", 
                                       strong("Security"), 
                                       selected = "All",
                                       choices = 
                                         c("All",
                                           "Treasury Bills",
                                           "Treasury Notes",
                                           "Treasury Bonds",
                                           "Treasury Inflation-Protected Securities (TIPS)",
                                           "Treasury Floating Rate Notes (FRN)",
                                           "Federal Financing Bank")),
                                     conditionalPanel(
                                       condition = 
                                         "input.type_of_plot == 'Securities Histogram'",
                                       checkboxInput(inputId = "density",
                                                     label = 
                                                       strong("Show density estimate"),
                                                     value = FALSE)),
                                     conditionalPanel(
                                       condition = 
                                         "input.type_of_plot == 'Total Debt and Treasury Security Interest Rates'",
                                       numericInput(inputId = "start_year",
                                                    label = 
                                                      strong
                                                    ("Start Year of Interest (2001 or later)"),
                                                    value = 2001)),
                                     conditionalPanel(
                                       condition = 
                                         "input.type_of_plot == 'Total Debt and Treasury Security Interest Rates'",
                                       numericInput(inputId = "end_year",
                                                    label = "End Year of Interest",
                                                    value = 
                                                      as.numeric(format(Sys.Date(),"%Y"))))),
                                   conditionalPanel(
                                     condition = "input.type_of_plot == 'Correlation Plot'",
                                     checkboxGroupInput(inputId = "corr_plot_input",
                                                 label = "Choose Variables for Correlation Plot",
                                                 choices = c("Treasury Bills",
                                                             "Treasury Notes",
                                                             "Treasury Bonds",
                                                             "Treasury Inflation-Protected Securities (TIPS)",
                                                             "Treasury Floating Rate Notes (FRN)",
                                                             "Federal Financing Bank",
                                                             "Total Public Debt Outstanding"),
                                                 selected = "Treasury Bills")),
                                   mainPanel(plotOutput("graphical_summary", height = "800px")),
                                   conditionalPanel(
                                     condition = "input.density == true",
                                     sliderInput(inputId = "bw_adjust",
                                                 label = "Bandwidth adjustment:",
                                                 min = 0.01, max = 0.15, 
                                                 value =0.1, step = 0.05)))),
               navbarMenu("Modeling",
                          tabPanel("Modeling Info",
                                   htmlOutput("model1_mlr1"),
                                   uiOutput("mlr_equation1"),
                                   htmlOutput("model1_mlr2"),
                                   htmlOutput("model2_rf1")),
                          tabPanel("Model Fitting",
                                   numericInput(inputId ="train_percent",
                                                label = "Percent of data to be used for Training Model (0-100)",
                                                value = 80),
                                   checkboxGroupInput(inputId = "model_var_mlr",
                                                      label = strong("Choose Predictor Variables in the MLR Model"),
                                                      choices = c("Treasury Bills",
                                                                  "Treasury Notes",
                                                                  "Treasury Bonds",
                                                                  "Treasury Inflation-Protected Securities (TIPS)",
                                                                  "Treasury Floating Rate Notes (FRN)",
                                                                  "Federal Financing Bank"),
                                                      selected = c("Treasury Bills",
                                                                   "Treasury Notes",
                                                                   "Treasury Bonds")),
                                   checkboxGroupInput(inputId = "model_var_rf",
                                                      label = strong("Choose Predictor Variables in the RF Model"),
                                                      choices = c("Treasury Bills",
                                                                  "Treasury Notes",
                                                                  "Treasury Bonds",
                                                                  "Treasury Inflation-Protected Securities (TIPS)",
                                                                  "Treasury Floating Rate Notes (FRN)",
                                                                  "Federal Financing Bank"),
                                                      selected = c("Treasury Bills",
                                                                   "Treasury Notes",
                                                                   "Treasury Bonds")),
                                   htmlOutput("text_in"),
                                   numericInput(inputId = "tuning_parameter",
                                                label = "Choose Tuning Parameter Grid",
                                                value = 2),
                                   numericInput(inputId = "cv_settings",
                                                label = "Choose CV Settings",
                                                value = 3),
                                   actionButton(inputId = "model_run_button",
                                                label = "Run Model"),
                                   conditionalPanel(
                                     condition = "input.model_run_button",
                                     htmlOutput("mlr_train_output")),
                                   verbatimTextOutput("mlr_model_result"),
                                   conditionalPanel(
                                     condition = "input.model_run_button",
                                     htmlOutput("rf_train_output")),
                                   verbatimTextOutput("rf_model_result"),
                                   plotOutput("rf_plot")),
                          tabPanel("Prediction",
                                   htmlOutput("prediction_text"),
                                   numericInput(inputId = "t_bill",
                                                label = "Treasury Bills",
                                                value = 2),
                                   numericInput(inputId = "t_note",
                                                label = "Treasury Notes",
                                                value = 2),
                                   numericInput(inputId = "t_bnd",
                                                label = "Treasury Bonds",
                                                value = 2),
                                   numericInput(inputId = "tips",
                                                label = "Treasury Inflation-Protected Securities (TIPS)",
                                                value = 2),
                                   numericInput(inputId = "frn",
                                                label = "Treasury Floating Rate Notes (FRN)",
                                                value = 2),
                                   numericInput(inputId = "ffb",
                                                label = "Federal Financing Bank",
                                                value = 2),
                                   htmlOutput("pred_mlr_result"),
                                   verbatimTextOutput("user_pred_mlrresult"),
                                   htmlOutput("pred_rf_result"),
                                   verbatimTextOutput("user_pred_rfresult")))
    )
)



