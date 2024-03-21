#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.


library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)

#library(bslib)

# Define server logic required to connect to US Treasury API, get data on average interest rates,
function(input, output, session) {
  # Create a function to connect to the Treasury API and get interest rate data
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
             avg_interest = avg_interest_rate_amt, year = record_calendar_year)
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
    parsed_data$avg_interest <- as.numeric(parsed_data$avg_interest)
    parsed_data$year <- as.numeric(parsed_data$year)
    return(parsed_data)
  }
  
  # Interest data, loads all data
  interest_data <- avg_interest()
  
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
    
    # Numerical summary output based on the user input
    output$numerical_summary <- DT::renderDataTable({
      DT::datatable(numerical_interest_data() %>% group_by(year) %>% 
                      summarise(value = 
                                  if_else(input$summary_type == "Mean", mean(avg_interest), 
                                          if_else(input$summary_type == "Median", median(avg_interest),
                                                  if_else(input$summary_type == "Maximum", max(avg_interest), min(avg_interest))))))
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
    # Graphical Summary- trend plot
    output$trend_plot <- renderPlot({
      g1 <- graphical_interest_data() %>% 
        filter(security == "Treasury Bills" | 
                 security == "Treasury Bonds" | 
                 security == "Treasury Notes" |
                 security == "Treasury Inflation-Protected Securities (TIPS)" |
                 security == "Treasury Floating Rate Notes (FRN)" |
                 security == "Federal Financing Bank") %>% 
        ggplot(aes(x = date, y = avg_interest)) + geom_line(aes(color = security)) + 
        labs(x = "year", title = "Trends in US Treasury Securities", y = "interest_rate") +
        theme_bw()
      g1
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