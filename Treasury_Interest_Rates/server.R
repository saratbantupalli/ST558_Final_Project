
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)

#library(bslib)

# # # Define server logic required to connect to US Treasury API, get data on 
# average interest rates and national public debt
function(input, output, session) {
  # Create 2 functions to connect to Treasury API. First API connects and gets 
  # interest rates on US Treasury Securities while seconds API connects and gets 
  # data on national public debt
  
  # # # # Function 1: Create a function called avg_interest to connect to the
  # Treasury API and get average interest rate data on US Treasuries
  avg_interest <- function(security_class = "All") {
    # Here we are joining different parts of the API URL we are interested in.
    # The base URL for queries in the API is constant.
    base_url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
    # The Endpoint of the average interest rates API
    api_endpoint <- "v2/accounting/od/avg_interest_rates"
    # Optional Pagination field
    pagination_data <- "?page[number]=1&page[size]=10000"
    # Here the complete URL for API was constructed by pasting the base_url and api_endpoint url
    output_url <- paste0(base_url,api_endpoint,pagination_data)
    #The GET function from httr package was used to connect to the API and get the data 
    raw_data <- GET(output_url)
    # The raw data in JSON format is converted to a data frame in these 2 steps
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    parsed_data <- data.frame((parsed_data$data))
    # # Their are certain observations that have a value "null". The rows with value "null" are removed then 
    # the data is filtered to only save the securities which are Marketable
    parsed_data <- parsed_data %>% filter(avg_interest_rate_amt != "null") %>% 
      filter(security_type_desc == "Marketable") %>%
      select(date = record_date, security = security_desc, 
             value = avg_interest_rate_amt, year = record_calendar_year)
    # Filtering data based on the type of security chosen by the user
    # If the user specified security_class is not equal to "all", the function returns data with the specified security_type
    if(security_class != "All") {
      parsed_data <- parsed_data %>% filter(security == security_class)
    }
    # If user specified security_type is "all", no filtering of data is done
    else {
      
    }
    # Converting the output to a tibble type
    parsed_data <- tibble(parsed_data)
    # Converting the output type for different parameters
    # Converting the date from character type to date using the lubridate package
    parsed_data$date <- ymd(parsed_data$date)
    # Converting the avg_interest parameter into a numeric
    parsed_data$value <- as.numeric(parsed_data$value)
    parsed_data$year <- as.numeric(parsed_data$year)
    return(parsed_data)
  }
  
  # # # # Function 2: Create a function called national_debt to connect to the
  # Treasury API and get national public data for the country
  national_debt <- function(security_type = "all", year_from = 2001, year_to = 2023, debt_type = "all"){
    # The base URL for queries in the API is constant.
    base_url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
    # The Endpoint of the monthly public debt
    debt_api_endpoint <- "v1/debt/mspd/mspd_table_1"	
    # Optional Pagination field
    pagination_data <- "?page[number]=1&page[size]=10000"
    
    # Here the complete URL for monthly public debt API was constructed by pasting the base_url and api_endpoint url
    debt_api_url <- paste0(base_url,debt_api_endpoint,pagination_data)
    
    #The GET function from httr package was used to connect to both the APIs and get the data 
    # Public debt data
    debt_raw_data <- GET(debt_api_url)
    
    # The raw debt data in JSON format is converted to a data frame in these 2 steps
    debt_parsed_data <- fromJSON(rawToChar(debt_raw_data$content))
    debt_parsed_data <- data.frame((debt_parsed_data$data))
    
    # # Here the Public Debt data was manipulated to select the parameters we are interested in analyzing
    # The data is then filtered to save the observations for Total Marketable, Total Nonmarketable, and Total Public Debt Outstanding
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
    # If the user specified security_class is not equal to "all", the function returns data with the specified security_type
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
  wider_interest_data <- interest_data %>% pivot_wider(names_from = "security", values_from = "value")
  # Debt data, loads US Treasury Securities average interest rates data
  debt_data <- national_debt()
  #Wide format
  wider_debt_data <- debt_data %>% pivot_wider(names_from = "security", values_from = "value")
  
  # Combine interest rate data and debt data
  combined_data <- inner_join(wider_interest_data, wider_debt_data, by = "date") %>%
    select(date = date, year = year.x, `Treasury Notes` = `Treasury Notes`,
           `Treasury Bonds` =  `Treasury Bonds`, `Treasury Bills` = `Treasury Bills`,
           `Federal Financing Bank`  = `Federal Financing Bank`,
           `Treasury Inflation-Protected Securities (TIPS)` = `Treasury Inflation-Protected Securities (TIPS)`,
           `Treasury Floating Rate Notes (FRN)` = `Treasury Floating Rate Notes (FRN)`,
           `Total Public Debt Outstanding` = `Total Public Debt Outstanding`)
  
  # Save the Average Interest rate data based on user input, by default loads all data
  # # For numerical summaries, create a reactive variable so that the data is based on user input
  numerical_interest_data <- reactive({
    if(input$type_of_security_numerical != "All") {
      interest_data %>%  filter(security == input$type_of_security_numerical)
    }
    else{
      interest_data
    }
  })
  
  # Save the Average Interest rate data based on user input, by default loads all data
  # # For graphical summaries, create a reactive variable so that the data is based on user input
  graphical_interest_data <- reactive({
    if(input$type_of_security_graphical != "All") {
      interest_data %>%  filter(security == input$type_of_security_graphical)
    }
    else{
      interest_data
    }
  })
  
  
  
  
  # Output for the About tab for the app
  output$app_purpose <- renderUI(
        HTML( "<b>", "Purpose of the App:", "</b>" , 
        "The purpose of this App is to", "<em>",
        "connect to an API, get the data, and model the data.", "</em>",
        "This app connects to the", "<em>","US Treasury API,", "</em>",
        "gets publicly available data on the nation's fiscal health, and applies modeling techniques 
        to estimate the trends in the nation's finances and some key interest rates. This tool could 
        help anyone look at the Treasury data, estimate, and see trends in average interest rates on 
        Marketable US Treasury Securities.", "<br>", "<br>",
        "<b>", "Data and Source:", "</b>" , 
        "The US Treasury maintains and publishes key fiscal data about the country's finances including 
        national debt, deficit, interest rates, and overall health of the economy. This all important 
        data is available to everyone using the Treasury API which can be found", 
        "<a href=https://fiscaldata.treasury.gov>", "here.", "</a>",
        "<br>", "<br>",
        "<b>", "Navigation of App:", "</b>", 
        "This app is setup to provide the user chance to explore the data by themselves. The About 
        tab provides the purporse of the app, the Data Exploration tab lets the user create numerical 
        and graphical summaries, and the Modeling tab lets the user apply different regression models. 
        In the data exploration tab, the user can choose see trends in average interest rates for 7 different
        marketable securities including Treasury Bills, Treasury Notes, Treasury Bonds, 
        Treasury Inflation-Protected Securities (TIPS), Treasury Floating Rate Notes (FRN), 
        Federal Financing Bank, and Total Marketable", "</br>", "</br>",
        "This app is based on a project I did as part of ST-558, Data Science for Statistician at NC State 
        University Statistics Department. Information on the project can be", 
        "<a href=https://saratbantupalli.github.io/2023/10/11/ST558_Project2.html>", "here.", "</a>", 
        "</br>", "</br>")
        )
    # Include picture in the about tab
    src = "https://upload.wikimedia.org/wikipedia/en/c/c9/Charging_Bull_statue.jpg"
    output$picture <- renderText({c('<img src="',src,'">')})
    # Foot note in the about tab 
    output$note <- renderUI(
        HTML("</br>", "<em>", "Note: Since the US Treasury logo is trademarked and cannot be freely used, 
             the Wallstreet Bull image is used here", "</em>")
    )
    
    
    # Output for Numerical summary based on the user input
    output$numerical_summary <- DT::renderDataTable({
      DT::datatable(numerical_interest_data() %>% group_by(year) %>% 
                      summarise(value = 
                                  if_else(input$summary_type == "Mean", mean(value), 
                                          if_else(input$summary_type == "Median", median(value),
                                                  if_else(input$summary_type == "Maximum", max(value), min(value))))))
    })
    
    # Reactive function for Output of Graphical Summary based on the user input
    graphical_plot <- reactive({
      if(input$type_of_plot == "Securities Trend Plot") {
        g1 <- graphical_interest_data() %>% 
        filter(security == "Treasury Bills" | 
                 security == "Treasury Bonds" | 
                 security == "Treasury Notes" |
                 security == "Treasury Inflation-Protected Securities (TIPS)" |
                 security == "Treasury Floating Rate Notes (FRN)" |
                 security == "Federal Financing Bank") %>% 
            ggplot(aes(x = date, y = value)) + geom_line(aes(color = security)) + 
            labs(x = "year", title = "Trends in US Treasury Securities", y = "Interest rate (%)") +
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
          ggplot(aes(x = value)) + geom_histogram(aes(y = ..density.., fill = security), alpha = 0.5) + 
          labs(x = "Interest rate (%)", title = "Histogram of US Treasury Securities", y = "Density") +
          theme_bw()
        if(input$density == TRUE) {
          g2 <- g2 + geom_density(bw = input$bw_adjust, alpha = 0.5, position = "stack")
        }
        else{
          g2
        }
        g2
      }
      else if(input$type_of_plot == "Total Debt Plot"){
        g3 <- debt_data() %>% 
          ggplot()
      }
      
    })

    # Output for Graphical Summary based on the user input
    output$graphical_summary <- renderPlot({
      output_plot <- graphical_plot()
      output_plot
    })
    
    


}




# output$about_purpose <- renderText( 
#     br(),
#     strong("Purpose of the App:"))
# # output$distPlot <- renderPlot({
# # 
# #     # generate bins based on input$bins from ui.R
# #     x    <- faithful[, 2]
# #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# # 
# #     # draw the histogram with the specified number of bins
# #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
# #          xlab = 'Waiting time to next eruption (in mins)',
# #          main = 'Histogram of waiting times')
# 
# })

#Estimations and Modeling using this all important information could give some significant insights into
#https://fiscaldata.treasury.gov


# function(input, output, session) {
#   output$app_purpose <- renderUI(
#     HTML("The purpose of this App is to", "<b>","connect to an API, get the data, and 
#              model the data.", "</b>", "<br>", "<br>",
#          "This app connects to the", "<em>","US Treasury API", "</em>","gets publicly available data on the nation's fiscal
#              health, and applies modeling techniques to estimate the trends in the nation's finances and 
#              some key interest rates. This tool could help anyone look at the Treasury data, estimate, 
#              and make a more informed decision about investments in the US Treasuries."))
#   
#   output$data_source <- renderUI(
#     HTML("The US Treasury maintains and publishes key fiscal data about the country's finances including national 
#            debt, deficit, interest rates, and overall health of the economy. This all important data is available 
#            to everyone using the Treasury API which can be found", "<a href=https://fiscaldata.treasury.gov>", "here.", "</a>"))
#   
#   output$purpose_of_tabs <- renderUI(
#     HTML("This app is setup to provide the user chance to explore the data by themselve. The About tab provides the purporse 
#            of the app, the Data Exploration tab lets the user create numerical and graphical summaries, the Modeling tab lets 
#            the user different regression models."))
#   
#   
#   
# }