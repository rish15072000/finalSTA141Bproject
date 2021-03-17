library(ggplot2)
library(quantmod)
library(httr)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(keyring)
library(RJSONIO)
library(lubridate)
library(rvest)
library(shiny)
library(shinydashboard)
library(dygraphs)
library("styler")
library(shiny.router)


################## APP LINK :- https://rishabgupta.shinyapps.io/finalSTA141Bproject/      ################

#### APP INFO :-
# This app will allow users to analyze Bitcoin as an investment opportunity for themselves. App will liberate users
#to fetch the current price using API, see historic trends from the past ten years and distinguish the growth rates.
# The App has the main dashboard coupled with 3 other pages.
####

# fetching the live prices of Bitcoin (in $). Updated every minute on dashboard

final_result <- fromJSON("https://api.coindesk.com/v1/bpi/currentprice.json")
# current date for dashboard
cdate <- Sys.Date()
# end date for calculating the data for one day
edate <- ymd(cdate) - 1
rdate <- ymd(cdate) - 2
# end date for calculating the data for more than 31 days
mdate <- ymd(cdate) - 32
# end date for calculating the data for 6 months
mdate_6 <- ymd(cdate) - 180
# end date for calculating the data for 1 year
mdate_1y <- ymd(cdate) - 365
# end date for calculating the data for 5 years
mdate_5y <- ymd(cdate) - 1825
chdate <- Sys.Date()
rhdate <- ymd(chdate) - 31
cadate <- Sys.Date()
radate <- ymd(cadate) - 31

# API for extracting values relating to starting and ending date
hours_high_low <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                                    st = rdate, en = edate), flatten = TRUE)
# API for extracting values relating to 31 days
last_days <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                               st = mdate, en = edate), flatten = TRUE)
# API for extracting values relating to 6 months
last_days_6 <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                                 st = mdate_6, en = edate), flatten = TRUE)
# API for extracting values relating to 1 year
last_days_1y <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                                  st = mdate_1y, en = edate), flatten = TRUE)
# API for extracting values relating to 5 years
last_days_5y <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                                  st = mdate_5y, en = edate), flatten = TRUE)

# API for extracting old values of bitcoin to see the trend
allhigh_result <- fromJSON("https://api.coindesk.com/v1/bpi/historical/close.json")

# Calculating the growth rate
g <- ((max(hours_high_low$bpi) - min(hours_high_low$bpi))/(min(hours_high_low$bpi) *
                                                             10000))
# Creating the dashboard
ui <- dashboardPage(dashboardHeader(title = "Bitcoin Analysis"),
                    dashboardSidebar(sidebarMenu(menuItem("Dashboard",
                                                          tabName = "dashboard", icon = icon("dashboard")),
                                                 menuItem("Currency check for bitcoin",
                                                          tabName = "widgets", icon = icon("money")),
                                                 menuItem("Price analysis", tabName = "widgets1",
                                                          icon = icon("calendar")),
                                                 menuItem("Growth analysis", tabName = "widgets2",
                                                          icon = icon("bar-chart-o")))),
                    dashboardBody(fluidRow(h2("Key Metrics"),
                                           infoBoxOutput("usdprice"), infoBoxOutput("marketcap"),
                                           infoBoxOutput("usdhigh")), fluidRow(infoBoxOutput("usdlow"),
                                                                               infoBoxOutput("alltimehigh"),
                                                                               infoBoxOutput("hoursgrowth")),
                                  tabItems(tabItem(tabName = "dashboard",
                                                   
                                                   fluidRow(dygraphOutput("plot2",
                                                                          height = 250, width = 500),dygraphOutput("plot3",
                                                                                                                   height = 250, width = 500)),
                                                   fluidRow(dygraphOutput("plot4",
                                                                          height = 250, width = 500),dygraphOutput("plot5",
                                                                                                                   height = 250, width = 500)),
                                  ),
                                  
                                  # creating page for displaying bitcoin values for different currencies
                                  tabItem(tabName = "widgets",
                                          fluidRow(titlePanel("Choose a currency"),
                                                   sidebarLayout(sidebarPanel(selectInput("dest",
                                                                                          "Currency", choices = c("-",
                                                                                                                  "United States Dollar",
                                                                                                                  "British Pound Sterling",
                                                                                                                  "Euro"), multiple = FALSE),
                                                   ), mainPanel(h3("Price Of Bitcoin In Different Currencies"),
                                                                h5("Price Of One Bitcoin"),
                                                                span(textOutput("minprice"), style="color:green"))))),
                                  
                                  #Page 2
                                  # creating a page for analyzing historical trends of Bitcoin when a user inputs a date
                                  tabItem(tabName = "widgets1",
                                          fluidPage(titlePanel("Select a date"),
                                                    sidebarLayout(sidebarPanel(dateInput("date1",
                                                                                         "From:", value = rhdate),
                                                                               dateInput("date2",
                                                                                         "To:", value = chdate)),
                                                                  mainPanel(h3("Analyze Bitcoin History"),
                                                                            dygraphOutput("btcprice"),
                                                                            textOutput("head_summary"),
                                                                            verbatimTextOutput("description"))))),
                                  #Page 3
                                  # creating a page for analyzing growth rates of Bitcoin in various time frames
                                  tabItem(tabName = "widgets2",
                                          fluidPage(titlePanel("Select a date"),
                                                    sidebarLayout(sidebarPanel(selectInput("origin",
                                                                                           "Select time period",
                                                                                           choices = c("-",
                                                                                                       "31 days", "6 months",
                                                                                                       "1 year", "3 year",
                                                                                                       "5 year", "10 year"),
                                                                                           multiple = FALSE)),
                                                                  mainPanel(h3("Bitcoin Growth"),
                                                                            h4("Growth"),
                                                                            
                                                                            span(textOutput("gprice"), style="color:blue"),
                                                                            dygraphOutput("btcprice1"),
                                                                            textOutput("head_summary1"),
                                                                            verbatimTextOutput("description1"),
                                                                            textOutput("head_monthly_detail1"),
                                                                            tableOutput("monthly_description1"))))))))



server <- function(input, output) {
  # For page "currency check for bitcoin"
  # fetching API for bitcoin prices in different currencies dynamically
  
  monthly_result <- reactive({
    # API for current prices of Bitcoin (updated every 1 min)
    final_result <- fromJSON("https://api.coindesk.com/v1/bpi/currentprice.json")
  })
  output$minprice <- renderPrint({
    rsl = 0
    pricel <- monthly_result()
    if (input$dest == "United States Dollar") {
      rsl <- pricel$bpi$USD$rate
    } else if (input$dest == "British Pound Sterling") {
      rsl <- pricel$bpi$GBP$rate
    } else if (input$dest == "Euro") {
      rsl <- pricel$bpi$EUR$rate
    }
    cat(rsl)
  })
  history_result <- reactive({
    # For page "Price analysis"
    fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                      st = input$date1, en = input$date2),
             flatten = TRUE)
  })
  output$head_summary <- renderText({
    "Summary Statistics"
  })
  output$description <- renderPrint({
    summary(history_result()$bpi)
  })
  output$btcprice <- renderDygraph(dygraph(data = history_result()$bpi,
                                           main = "Bitcoin Analysis") %>%
                                     dyHighlight(highlightCircleSize = 5,
                                                 highlightSeriesBackgroundAlpha = 0.2,
                                                 hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                     dyRangeSelector())
  # Specifying conditions for various time periods
  date_result <- reactive({
    sdate <- cadate
    fdate <- radate
    if (input$origin == "6 months") {
      sdate <- cadate
      fdate <- ymd(cadate) - 180
    } else if (input$origin == "1 year") {
      sdate <- cadate
      fdate <- ymd(cadate) - 365
    } else if (input$origin == "3 year") {
      sdate <- cadate
      fdate <- ymd(cadate) - 1095
    } else if (input$origin == "5 year") {
      sdate <- cadate
      fdate <- ymd(cadate) - 1825
    } else if (input$origin == "10 year") {
      sdate <- cadate
      fdate <- ymd(cadate) - 3650
    }
    r <- fromJSON(str_glue("https://api.coindesk.com/v1/bpi/historical/close.json?start={st}&end={en}",
                           st = fdate, en = sdate),
                  flatten = TRUE)
  })
  output$head_summary1 <- renderText({
    "Summary Statistics"
  })
  output$description1 <- renderPrint({
    summary(date_result()$bpi)
  })
  
  #Calculating the growth rate
  output$gprice <- renderPrint({
    pricel <- date_result()$bpi
    l <- length(pricel)
    nw <- pricel[[l]]
    ol <- pricel[[1]]
    rs <- ((nw - ol)/(ol)) * 100
    rsv<-paste(rs,"%")
    cat(rsv)
  })
  
  #Displaying the graph
  output$btcprice1 <- renderDygraph(dygraph(data = date_result()$bpi,
                                            main = "Price of Bitcoin") %>%
                                      dyHighlight(highlightCircleSize = 5,
                                                  highlightSeriesBackgroundAlpha = 0.2,
                                                  hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                      dyRangeSelector() %>% dyOptions(colors = "green",
                                                                      stackedGraph = TRUE, fillGraph = TRUE,
                                                                      fillAlpha = 0.4))
  
  #Showcasing Key Metrics in the dashboard
  output$usdprice <- renderInfoBox({
    infoBox("Price in $", final_result$bpi$USD$rate,
            color = "purple")
  })
  output$marketcap <- renderInfoBox({
    infoBox("Market Cap", "$ 1.04T",
            icon = icon("credit-card"),
            color = "yellow")
  })
  output$usdhigh <- renderInfoBox({
    infoBox("24 hour High ", max(hours_high_low$bpi),
            icon = icon("thumbs-up"),
            color = "green")
  })
  output$usdlow <- renderInfoBox({
    infoBox("24 hour LOW ", min(hours_high_low$bpi),
            icon = icon("thumbs-down"),
            color = "red")
  })
  output$alltimehigh <- renderInfoBox({
    infoBox("ALL TIME HIGH ", max(allhigh_result$bpi),
            icon = icon("list"), color = "blue")
  })
  output$hoursgrowth <- renderInfoBox({
    infoBox("growth ", paste0(g,
                              "%"), icon = icon("list"),
            color = "aqua")
  })
  price <- last_days$bpi
  
  
  # Outputting different graphs for distinguishing between different time periods
  output$plot2 <- renderDygraph(dygraph(data = last_days$bpi,
                                        main = "Trend of Bitcoin in Last 31 days") %>%
                                  dyHighlight(highlightCircleSize = 5,
                                              highlightSeriesBackgroundAlpha = 0.2,
                                              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                  dyRangeSelector()%>% dyOptions(colors = "green",
                                                                 stackedGraph = TRUE, fillGraph = TRUE,
                                                                 fillAlpha = 0.4))
  output$plot3 <- renderDygraph(dygraph(data = last_days_6$bpi,
                                        main = "Trend of Bitcoin in Last 6 months") %>%
                                  dyHighlight(highlightCircleSize = 5,
                                              highlightSeriesBackgroundAlpha = 0.2,
                                              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                  dyRangeSelector()%>% dyOptions(colors = "orange",
                                                                 stackedGraph = TRUE, fillGraph = TRUE,
                                                                 fillAlpha = 0.4))
  output$plot4 <- renderDygraph(dygraph(data = last_days_1y$bpi,
                                        main = "Trend of Bitcoin in Last 1 year") %>%
                                  dyHighlight(highlightCircleSize = 5,
                                              highlightSeriesBackgroundAlpha = 0.2,
                                              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                  dyRangeSelector()%>% dyOptions(colors = "yellow",
                                                                 stackedGraph = TRUE, fillGraph = TRUE,
                                                                 fillAlpha = 0.4))
  output$plot5 <- renderDygraph(dygraph(data = last_days_5y$bpi,
                                        main = "Trend of Bitcoin in Last 5 years") %>%
                                  dyHighlight(highlightCircleSize = 5,
                                              highlightSeriesBackgroundAlpha = 0.2,
                                              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                  dyRangeSelector()%>% dyOptions(colors = "olive",
                                                                 stackedGraph = TRUE, fillGraph = TRUE,
                                                                 fillAlpha = 0.4))
}
shinyApp(ui, server)