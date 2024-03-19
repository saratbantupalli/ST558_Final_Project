#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.


library(shiny)
#library(bslib)

# Define server logic required to draw a histogram
function(input, output, session) {
    output$app_purpose <- renderUI(
        HTML( "<b>", "Purpose of the App:", "</b>" , 
        "The purpose of this App is to", "<em>",
        "connect to an API, get the data, and model the data.", "</em>",
        "This app connects to the", "<em>","US Treasury API,", "</em>",
        "gets publicly available data on the nation's fiscal health, and applies modeling techniques 
        to estimate the trends in the nation's finances and some key interest rates. This tool could 
        help anyone look at the Treasury data, estimate, and make a more informed decision about investments 
        in the US Treasuries.", "<br>", "<br>",
        "<b>", "Data and Source:", "</b>" , 
        "The US Treasury maintains and publishes key fiscal data about the country's finances including national 
        debt, deficit, interest rates, and overall health of the economy. This all important data is available 
        to everyone using the Treasury API which can be found", "<a href=https://fiscaldata.treasury.gov>", "here.", "</a>",
        "<br>", "<br>",
        "<b>", "Navigation of App:", "</b>", 
        "This app is setup to provide the user chance to explore the data by themselves. The About tab provides the purporse 
        of the app, the Data Exploration tab lets the user create numerical and graphical summaries, and the Modeling tab lets 
        the user apply different regression models.", "</br>", "</br>")
        )
    
    #src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Seal_of_the_United_States_Department_of_the_Treasury.svg"
    # output$picture <- renderImage({
    #     list(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Seal_of_the_United_States_Department_of_the_Treasury.svg",
    #          width = 224,
    #          height = 136,
    #          alt = "Atlt text")
    # })
    #output$picture <- renderImage("https://www.imf.org/assets/imf/images/footer/IMF_seal.svg")
    src = "https://upload.wikimedia.org/wikipedia/en/c/c9/Charging_Bull_statue.jpg"
    output$picture <- renderText({c('<img src="',src,'">')})

    output$note <- renderUI(
        HTML("</br>", "<em>", "Note: Since the US Treasury logo is trademarked and cannot be freely used, the Wallstreet Bull image is used here", "</em>")
    )

}



      # src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Seal_of_the_United_States_Department_of_the_Treasury.svg"
      # output$picture <- renderText({c('<img src=" ',src,'">')})
      # #output$picture <- renderImage(https://www.imf.org/assets/imf/images/footer/IMF_seal.svg)
      

# – Describe the purpose of the app
# – Briefly discuss the data and its source - providing a link to more information about the data
# – Tell the user the purpose of each tab (page) of the app
# – Include a picture related to the data (for instance, if the data was about the world wildlife fund,
#                                          you might include a picture of their logo)



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