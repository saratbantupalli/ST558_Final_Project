
# # Purpose of app- develop a R Shiny app that connects to a data source (here an API),
# generates interactive numerical and graphical summaries, builds prediction models


# # What the App does- This app reads data from US Treasury API, transforms the data into
# usable form, generates interactive numerical and graphical summaries, then builds 
# prediction models


# Required packages for running the server side of the app
library(shiny)
library(tidyverse) # tidyverse universe coding style is adopted throughout when possible
library(httr) # for establishing a connection to the API and get data
library(jsonlite) # to parse the data from API into usable form
library(corrplot) # to generate correlation plots
library(caret) # for model development 


# # # Server logic required to connect to US Treasury API, get data on 
# average interest rates and national public debt

# Flow of logic: First connect to the API, gather data, manipulate data based
# on user input (use reactive functions to modify data), and create output

function(input, output, session) {
  # Output for the About tab for the app
  
  
  output$app_purpose <- renderUI(
    HTML( "<b>", "Purpose of the App:", "</b>" , 
          "This R Shiny app connects to the", "<em>","US Treasury API,", "</em>",
          "to retrieve publicly available data on the nation's fiscal health. It 
          then models this data to estimate trends in the national debt 
          finances and key interest rates. Users can leverage this tool to analyze 
          Treasury data, understand trends in average interest rates on 
          Marketable US Treasury Securities, and create models of the National 
          Debt influenced by these interest rates.", 
          
          "<br>", "<br>",
          "<b>", "Data and Source:", "</b>" , 
          "The US Treasury maintains and publishes essential fiscal data 
          concerning the nation's finances, encompassing national debt, deficit, 
          interest rates, and overall health of the economy. This crucial data is 
          publicly available through the Treasury API which can be found", 
          "<a href=https://fiscaldata.treasury.gov>", "here.", "</a>",
          
          "<br>" ,"<br>", 
          "<b>", "Quick Intro on US Treasury Securities :", "</b>",
          "Broadly speaking there are two types of Treasury Securities:",
          
          "<br>","<b>","1. Marketable Securities","</b>",": These are securities
          readily traded on the open market, making them accessible to individual
          investors (like me!!). Examples include Treasury Bills, Treasury Notes, 
          and Treasury Bonds to name a few. Our analysis specifically targets 
          trends within these marketable securities, and therefore, this 
          application exclusively filters their data, trasnforms and models.",
          
          "<br>","<b>","2. Non-Marketable Securities","</b>",": These are non-
          transferable and cannot be traded on the open market, and such, are
          not included in the app's analysis.",
          
          "<br>", "<br>", 
          "<b>", "Navigation of App:", "</b>", 
          "This interactive app empowers users to explore data independently. The" 
          ,"<em>", "About tab", "</em>","explains the app's purporse, the", "<em>",
          "Data Exploration tab", "</em>","allows for creating numerical and 
          graphical summaries of interest, and the", "<em>","Modeling tab", "</em>", 
          "offers covariates choice to predict the national debt using various models.",
          
          "<br>", "<br>", 
          "In the Data Exploration tab, the users can visualize trends for 
          seven distinct marketable securities:", "<em>", 
          "including Treasury Bills, Treasury Notes, 
          Treasury Bonds, Treasury Inflation-Protected Securities (TIPS), 
          Treasury Floating Rate Notes (FRN), 
          Federal Financing Bank, and Total Marketable.", "</em>",
          "Additionally, this section allows users to examine the distribution 
          of various US Treasury Securities, trends in National Public Debt, 
          and the correlation between US Treasury Securities and the National Debt.", 
          "</br>", "<br>", 
          "In the Modeling tab, users can predict the Total Public Debt 
          based on US Treasury Securities interest rates. The app offers a choice 
          between a Multiple Linear Regression Model and a 
          Random Forest Model for predictions.", "</br>", "</br>",
          "This app is based on a project I did as part of ST-558, Data Science 
          for Statistician at NC State University Statistics Department. 
          Information on the project can be", 
          "<a href=https://saratbantupalli.github.io/2023/10/11/ST558_Project2.html>", 
          "here.", "</a>","</br>", "</br>"))
  
  # Create 2 functions to connect to Treasury API. First API connects and gets 
  # interest rates on US Treasury Securities while seconds API connects and gets 
  # data on national public debt
  
  # # # # Function 1: Create a function called avg_interest to connect to the
  # Treasury API and get average interest rate data on US Treasuries
  avg_interest <- function(security_class = "All") {
    # Here we are joining different parts of the API URL we are interested in.
    # The base URL for queries in the API is constant.
    base_url <- 
      "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
    # The Endpoint of the average interest rates API
    api_endpoint <- "v2/accounting/od/avg_interest_rates"
    # Optional Pagination field
    pagination_data <- "?page[number]=1&page[size]=10000"
    # # # Here the complete URL for API was constructed by pasting the base_url 
    # and api_endpoint url
    output_url <- paste0(base_url,api_endpoint,pagination_data)
    # #The GET function from httr package was used to connect to the API and get 
    # the data 
    raw_data <- GET(output_url)
    # The raw data in JSON format is converted to a data frame in these 2 steps
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    parsed_data <- data.frame((parsed_data$data))
    # # # Their are certain observations that have a value "null". The rows with 
    # value "null" are removed then the data is filtered to only save the
    # securities which are Marketable
    parsed_data <- parsed_data %>% filter(avg_interest_rate_amt != "null") %>% 
      filter(security_type_desc == "Marketable") %>%
      select(date = record_date, security = security_desc, 
             value = avg_interest_rate_amt, year = record_calendar_year)
    # # Filtering data based on the type of security chosen by the user
    # # If the user specified security_class is not equal to "all", the function 
    # returns data with the specified security_type
    if(security_class != "All") {
      parsed_data <- parsed_data %>% filter(security == security_class)
    }
    # If user specified security_type is "all", no filtering of data is done
    else {
      
    }
    # Converting the output to a tibble type
    parsed_data <- tibble(parsed_data)
    # Converting the output type for different parameters
    # # Converting the date from character type to date using the 
    # lubridate package
    parsed_data$date <- ymd(parsed_data$date)
    # Converting the avg_interest parameter into a numeric
    parsed_data$value <- as.numeric(parsed_data$value)
    parsed_data$year <- as.numeric(parsed_data$year)
    return(parsed_data)
  }
  
  # # # # Function 2: Create a function called national_debt to connect to the
  # Treasury API and get national public data for the country
  national_debt <- function(security_type = "all", year_from = 2001, 
                            year_to = 2023, debt_type = "all"){
    # The base URL for queries in the API is constant.
    base_url <- 
      "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
    # The Endpoint of the monthly public debt
    debt_api_endpoint <- "v1/debt/mspd/mspd_table_1"	
    # Optional Pagination field
    pagination_data <- "?page[number]=1&page[size]=10000"
    
    # # Here the complete URL for monthly public debt API was constructed by 
    # pasting the base_url and api_endpoint url
    debt_api_url <- paste0(base_url,debt_api_endpoint,pagination_data)
    
    # #The GET function from httr package was used to connect to both the APIs 
    # and get the data Public debt data
    debt_raw_data <- GET(debt_api_url)
    
    # # The raw debt data in JSON format is converted to a data frame in these 
    # 2 steps
    debt_parsed_data <- fromJSON(rawToChar(debt_raw_data$content))
    debt_parsed_data <- data.frame((debt_parsed_data$data))
    
    # # # Here the Public Debt data was manipulated to select the parameters 
    # we are interested in analyzing
    # # The data is then filtered to save the observations for Total Marketable, 
    # Total Nonmarketable, and Total Public Debt Outstanding
    debt_parsed_data <- debt_parsed_data %>%
      select(date = record_date, security = security_type_desc, 
             value = total_mil_amt, 
             year = record_calendar_year) %>% 
      filter(security == "Total Marketable" | 
               security == "Total Nonmarketable" |
               security == "Total Public Debt Outstanding")
    # Filtering data based on user requested range of years
    debt_parsed_data <- debt_parsed_data %>% 
      filter(year >= year_from & year <= year_to) %>% 
      select(date, security, value, year)
    # Converting data into numeric
    debt_parsed_data$date <- ymd(debt_parsed_data$date)
    debt_parsed_data$value <- as.numeric(debt_parsed_data$value)
    debt_parsed_data$year <- as.numeric(debt_parsed_data$year)
    debt_parsed_data <- tibble(debt_parsed_data)
    
    # Filtering data based on the type of security chosen by the user
    # # If the user specified security_class is not equal to "all", the 
    # function returns data with the specified security_type
    if(security_type != "all") {
      debt_parsed_data <- debt_parsed_data %>% 
        filter(security == security_type)
    }
    # If user specified security_type is "all", no filtering of data is done
    else {
      
    }
    
    debt_parsed_data 
  }
  
  
  # ## Gather data for the App- Here the Treasury securities interest rates and
  # national debt (in millions) data is stored
  # Interest data, loads US Treasury Securities average interest rates data
  interest_data <- avg_interest()
  #Wide format
  wider_interest_data <- interest_data %>% 
    pivot_wider(names_from = "security", values_from = "value")
  # Debt data, loads US Treasury Securities average interest rates data
  debt_data <- national_debt()
  #Wide format
  wider_debt_data <- debt_data %>% 
    pivot_wider(names_from = "security", values_from = "value")
  
  
  # Combine interest rate data and debt data
  combined_data <- 
    inner_join(wider_interest_data, wider_debt_data, by = "date") %>%
      select(date = date, year = year.x, `Treasury Notes` = `Treasury Notes`,
             `Treasury Bonds` =  `Treasury Bonds`, 
             `Treasury Bills` = `Treasury Bills`,
             `Federal Financing Bank`  = `Federal Financing Bank`,
             `Treasury Inflation-Protected Securities (TIPS)` = 
               `Treasury Inflation-Protected Securities (TIPS)`,
             `Treasury Floating Rate Notes (FRN)` = 
               `Treasury Floating Rate Notes (FRN)`,
             `Total Public Debt Outstanding` = 
               `Total Public Debt Outstanding`)
  
  
  # # # # For Data Exploration- Numerical Summary, 
  # create a reactive variable so that the data is based on user input
  numerical_interest_data <- reactive({
    if(input$type_of_security_numerical != "All") {
      interest_data %>% 
        filter(security == input$type_of_security_numerical)
    }
    else{
      interest_data
    }
  })
  
  
  # # # # # For Data Exploration - Graphical Summary 
  # (Securities Trend Plot and Securities Histogram), 
  # # create a reactive variable so that the data is based on user input
  graphical_interest_data <- reactive({
    if(input$type_of_security_graphical != "All") {
      interest_data %>% 
        filter(security == input$type_of_security_graphical)
    }
    else{
      interest_data
    }
  })
  
  
  # # # For Data Exploration - Graphical Summary 
  # (Total Debt and Treasury Security Interest Rates plot), 
  # # create a reactive data of combined_data 
  longer_combined_data <- reactive({
    data1 <- combined_data %>% 
      pivot_longer(cols = 3:8, names_to = "security", values_to = "values") %>% 
      filter(year >= input$start_year) %>%
      filter(year <= input$end_year)
    data1
  })
  
  
  # # # # # For graphical summaries(Total Debt and Treasury Security 
  # Interest Rates plot), 
  # # create a reactive variable so that the data is based on user input
  combined_subset_data <- reactive({
    if(input$type_of_security_graphical != "All") {
      longer_combined_data() %>%  
        filter(security == input$type_of_security_graphical)
    }
    else{
      longer_combined_data()
    }
  })
  
  
  # # For Data Exploration - Graphical Summary (Correlation Plot), 
  # create a reactive data of combined_data 
  corr_plot_data <- reactive({
    data2 <- combined_data %>% drop_na() %>% select(input$corr_plot_input)
    Correlation <- cor(data2)
    Correlation
  })
  
  
  # # Reactive function for Output of Data Exploration - Graphical Summary 
  # based on the user input
  graphical_plot <- reactive({
    if(input$type_of_plot == "Securities Trend Plot") {
      g1 <- graphical_interest_data() %>% 
        filter(security == "Treasury Bills" | 
                 security == "Treasury Bonds" | 
                 security == "Treasury Notes" |
                 security == "Treasury Inflation-Protected Securities (TIPS)" |
                 security == "Treasury Floating Rate Notes (FRN)" |
                 security == "Federal Financing Bank") %>% 
        ggplot(aes(x = date, y = value)) + 
        geom_line(aes(color = security)) + 
        labs(x = "year", 
             title = "Trends in US Treasury Securities", 
             y = "Interest rate (%)") +
        theme_bw()
      g1
    }
    else if(input$type_of_plot == "Securities Histogram"){
      g2 <- graphical_interest_data() %>% 
        filter(security == "Treasury Bills" | 
                 security == "Treasury Bonds" | 
                 security == "Treasury Notes" |
                 security == "Treasury Inflation-Protected Securities (TIPS)" |
                 security == "Treasury Floating Rate Notes (FRN)" |
                 security == "Federal Financing Bank") %>% 
        ggplot(aes(x = value)) + 
        geom_histogram(aes(y = ..density.., fill = security), alpha = 0.5) + 
        labs(x = "Interest rate (%)", 
             title = "Histogram of US Treasury Securities", 
             y = "Density") +
        theme_bw()
      if(input$density == TRUE) {
        g2 <- g2 + 
          geom_density(bw = input$bw_adjust, alpha = 0.5, position = "stack")
      }
      else{
        g2
      }
      g2
    }
    else if(input$type_of_plot == 
            "Total Debt and Treasury Security Interest Rates"){
      g3 <- combined_subset_data() %>% 
        ggplot(aes(x = values, 
                   y = `Total Public Debt Outstanding`)) + 
        geom_point(aes(color = security), alpha = 1) + 
        labs(y = "Total Public Debt (Millions USD)") 
      g3 + facet_wrap(~year) + theme_bw()
    }
    else if(input$type_of_plot == "Correlation Plot"){
      g4 <- corrplot(corr_plot_data(), tl.pos = "lt")
      g4
    }
    
  })
  
  
  # # # Reactive function for percentage of training and test data set for 
  # Modeling - Model Fitting based on the user input
  training_percent <- eventReactive(input$model_run_button,{
    p <- as.numeric(input$train_percent)/100
    p
  })
  # # Reactive function for variables of MLR for Modeling - Model Fitting 
  # based on the user input
  mlr_data_reactive <- eventReactive(input$model_run_button,{
      data_mlr <- combined_data %>% drop_na() %>% 
        select(`Total Public Debt Outstanding`, input$model_var_mlr)
      data_mlr
    })
  
  
  # # # Reactive function for variables of RF Model for Modeling - Model Fitting 
  # # based on the user input
   rf_data_reactive <- eventReactive(input$model_run_button,{
     data_rf <- combined_data %>% drop_na() %>% 
       select(`Total Public Debt Outstanding`, input$model_var_rf )
     data_rf
   })
  
  # Reactive function for RF Model tuning parameters
   rf_cv_reactive <- eventReactive(input$model_run_button,{
     cv <- as.numeric(input$cv_settings)
     cv
   })
   
   # Reactive function for RF Model tuning parameters
   rf_grid_reactive <- eventReactive(input$model_run_button,{
     rf_grid <- as.numeric(input$tuning_parameter)
     rf_grid
   })
  

  
  
  
  # Include picture in the about tab
  src = 
    "https://upload.wikimedia.org/wikipedia/en/c/c9/Charging_Bull_statue.jpg"
  output$picture <- renderText({c('<img src="',src,'">')})
  
  
  # Foot note in the about tab 
  output$note <- renderUI(
  HTML("</br>", "<em>", "Note: Since the US Treasury logo is 
  trademarked and cannot be freely used, the Wallstreet Bull image 
       is used here", "</em>"))
  
  
  
  # Output for Data Exploration- Numerical summary based on the user input
  output$numerical_summary <- DT::renderDataTable({
    DT::datatable(numerical_interest_data() %>% group_by(year) %>% 
                    summarise(value = 
                                if_else(input$summary_type == 
                                          "Mean", mean(value), 
                                        if_else(input$summary_type == 
                                                  "Median", median(value),
                                                if_else(input$summary_type == 
                                                          "Maximum", max(value), 
                                                        min(value))))))
    })
  
  
  
  # Output for Data Exploration- Graphical Summary based on the user input
  output$graphical_summary <- renderPlot({
    output_plot <- graphical_plot()
    output_plot
    })
  
  
  
  # Output for Modeling- Modeling Info 
  output$model1_mlr1 <- renderUI(
    HTML( "<b>", "Model 1- Multiple Linear Regression (MLR):", "</b>" , 
          "This type of linear model is useful to model relationship between 
          response variable and several explanatory variables. Complex 
          transformations of the explanatory variables can be used in this
          model. A MLR model with no interaction terms is given below:"))
  
  
  output$mlr_equation1 <- renderUI({
    withMathJax(
      helpText('$$Y_i = \\beta_0 + \\beta_1 * x_1 + \\beta_2 * x_2 + .. + 
      \\beta_p * x_p + E_i$$'))
  })
  
  
  output$model1_mlr2 <- renderUI({
    HTML("where Y", "<sub>","i", "</sub>", "is the Response Variable of interest, 
    \u03B2", "<sub>","0", "</sub>", "is the intercept, \u03B2", "<sub>","1", "</sub>" , 
    "is the slope for explanatory variable x", "<sub>","1", "</sub>",
    "\u03B2", "<sub>","2", "</sub>","is the slope for explanatory variable x", 
    "<sub>","2", "</sub>", ", and \u03B2", "<sub>","p", "</sub>" ,
    "is the slope for explanatory variable x", "<sub>","p", "</sub>" , "and E", 
    "<sub>","i", "</sub>", "is the error term.", "</br>", "</br>",
    "Benefits of MLR Model: The results from a MLR model are easily interpretable, 
    it can model relationship between response variable and complex transformations
    of explanatory variables.","</br>","</br>",
    "Drawbacks of MLR Model: Assumptions regarding distribution of errors (normality,
    and homoscedasticity) needs to hold valid, and achieving complex transformation of
    predictor variables is hard.")
   })
  
  
  output$model2_rf1 <- renderUI(
    HTML( "</br>", "</br>", "<b>", "Model 2- Random Forest Model:", 
          "</b>" , "A well known ensemble method, the Random Forest Model creates 
          a number of bootstrap by resmapling from the sample, then fits a 
          classification tree for each bootstrap sample based on a random 
          subset of predictor variables. The model then averages results across
          all trees.", "</br>", "</br>",
          "Benefits of Random Forest Model: We do not need to make any assumptions
          regarding the distribution of variables, the model can inherently pick-up
          interaction among variables, and we also decrease variance when compared to a single
          classification tree.", "</br>", "</br>",
          "Drawback of Random Forest Model: We lose interpretability, and the model is
          computationally intensive.", "</br>","</br>",
          "Note: For purpose of this app, Total US Debt (in millions) is 
          the response variable of interest and interest rates of different US 
          Treasury Securities are the explanatory variables. For MLR model, 
          Interaction terms are not considered for the purpose of this app. 
          In the Model Fitting section, the user can choose percentage of 
          test/train data split, and predictor/explanatory variables."))
  
  
  
  # Output for Modeling- Model Fitting
  output$text_in <- renderUI(
    HTML("For RF Model, choose tuning parameter grid and CV settings:", "</br>")
  )
  
  # Output for Modeling- Model Fitting
  output$mlr_train_output <- renderUI(
    HTML("</br>","MLR Training Data Output", "</br>")
  )
  # Output for MLR Modeling- Model Fitting
  mlr_model <-  eventReactive(input$model_run_button,{
    # # Split data into test and train set for MLR model and fit the model using the
    # Caret package
    mlr_data <-  mlr_data_reactive()
    mlr_train_index <- createDataPartition(mlr_data$`Total Public Debt Outstanding`, 
                                           p = training_percent(), list = FALSE)
    mlr_data_train <- mlr_data[mlr_train_index, ]
    mlr_data_test <- mlr_data[-mlr_train_index, ]
    # MLR Model
    mlr_model <- train( `Total Public Debt Outstanding` ~ ., 
                        data = mlr_data_train, 
                        method = "lm", 
                        preProcess = c("center", "scale"), 
                        trControl = trainControl(method = "cv", number = 5))
    mlr_model
  })
  
  
  
  output$mlr_model_result <- renderPrint({
    summary(mlr_model())
   })
  
  # Output for Modeling- Model Fitting
  output$rf_train_output <- renderUI(
    HTML("</br>","RF Training Data Output", "</br>")
  )
  # Output for RF Modeling- Model Fitting
  rf_model <- eventReactive(input$model_run_button,{
    # # Split data into test and train set for RF model and fit the model using the
    # Caret package
    rf_data <- rf_data_reactive()
    rf_train_index <- createDataPartition(rf_data$`Total Public Debt Outstanding`, 
                          p = training_percent(), list = FALSE)
    rf_data_train <- rf_data[rf_train_index, ]
    rf_data_test <- rf_data[-rf_train_index, ]
    # RF Model
    rf_model <- train(`Total Public Debt Outstanding` ~ .,
                      data = rf_data_train,
                      method = "rf",
                      trControl = 
                        trainControl(method = "cv", number = rf_cv_reactive()),
                      tuneGrid = data.frame(mtry = seq(1:rf_grid_reactive())))
    rf_model
  })
  # RF Model output
  output$rf_model_result <- renderPrint({
    rf_model()
  })
  
  # RF Model plot
  output$rf_plot <- renderPlot({
    plot(rf_model())
  })
  
  # Output for Modeling- Model Fitting
  output$prediction_text <- renderUI(
    HTML("</br>","Here you can make prediction of National Debt based on
         US Treasury Securities. You can enter values of interest 
         rates on the following securities: Treasury Bills,
         Treasury Notes, Treasury Bonds, 
         Treasury Inflation-Protected Securities (TIPS),
         Treasury Floating Rate Notes (FRN),
         Federal Financing Bank", "</br>")
  )
  
  # Prediction tab
  val1_reactive <- reactive({
    val1 <- as.numeric(input$t_bill)
    val1
  })
  val2_reactive <- reactive({
    val2 <- as.numeric(input$t_note)
    val2
  })
  val3_reactive <- reactive({
    val3 <- as.numeric(input$t_bnd)
    val3
  })
  val4_reactive <- reactive({
    val4 <- as.numeric(input$tips)
    val4
  })
  val5_reactive <- reactive({
    val5 <- as.numeric(input$frn)
    val5
  })
  val6_reactive <- reactive({
    val6 <- as.numeric(input$ffb)
    val6
  })
  
  output$pred_mlr_result <- renderUI(
    HTML("</br>", "Prediction result using MLR model and user defined data")
  )
  
  # User prediction Model output
  output$user_pred_mlrresult <- renderPrint({
    model <- mlr_model()
    df <- data.frame()
    new <- df %>% mutate(`Treasury Bills` = val1_reactive(),
                         `Treasury Notes` = val2_reactive(),
                         `Treasury Bonds` = val3_reactive(),
                         `Treasury Inflation-Protected Securities (TIPS)` = 
                           val4_reactive(),
                         `Treasury Floating Rate Notes (FRN)` = 
                           val5_reactive(),
                         `Federal Financing Bank` = 
                           val6_reactive())
    # new <- data.frame(`Treasury_Bills` = c(val1_reactive()),
    #          `Treasury_Notes` = c(val2_reactive()),
    #          `Treasury Bonds` = c(val3_reactive()),
    #          `Treasury Inflation-Protected Securities (TIPS)` = c(val4_reactive()),
    #           `Treasury Floating Rate Notes (FRN)` = c(val5_reactive()),
    #          `Federal Financing Bank` = c(val6_reactive()))
    result <- predict(model, newdata = new)
    result
  })
  
  output$pred_rf_result <- renderUI(
    HTML("</br>", "Prediction result using RF model and user defined data")
  )
  # User prediction Model output
  output$user_pred_rfresult <- renderPrint({
    model <- rf_model()
    new <-data.frame(val1_reactive(),
                     val2_reactive(),
                     val3_reactive(),
                     val4_reactive(),
                     val5_reactive(),
                     val6_reactive())
    
    # new <- data.frame('Treasury Bills ' = c(val1_reactive()), 
    #          `Treasury Notes` = c(val2_reactive()),
    #          `Treasury Bonds` = c(val3_reactive()), 
    #          `Treasury Inflation-Protected Securities (TIPS)` = c(val4_reactive()),
    #           `Treasury Floating Rate Notes (FRN)` = c(val5_reactive()), 
    #          `Federal Financing Bank` = c(val6_reactive()))
    result <- predict(model, newdata = new)
    result
  })
  
}

