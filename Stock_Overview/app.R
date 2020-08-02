#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#data cleanup
library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
library(textdata)
#library(lubridate)

#API and json
library(httr)
library(jsonlite)

#Web Scraping
library(rvest)

#Visualization
library(plotly)
library(ggplot2)
library(DT)
library(gridExtra)

#Data
library(devtools)
library(gtrendsR)

#Text Analysis
library(tidytext)
library(wordcloud)
library(RColorBrewer)

#Forecasting
library(quantmod)
library(forecast)
library(tseries)
library(prophet)

#Shiny
library(shinycssloaders)
library(shinydashboard)
library(shiny)
library(shinyjs)
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabItems(
    tabItem(tabName = "historical",
            h3("Stock Performance"),
            h4("A visualization of the price and volume time series for the selected stock."),
            sidebarLayout(position = "right",
                          sidebarPanel(h4("Directions"),
                                       tags$ul(
                                         tags$li("Type in your stock ticker of interest into the text input box on the left panel. The default stock ticker used is AMZN (Amazon)."),
                                         tags$li("Wait for the charts to generate - this may take up to 15 seconds."),
                                         tags$li("Navigate accross the dashboard pages using the menu items on the left panel.")),
              h4("Information"),
                    "This Shiny application uses trading data API to obtain historical stock price data to report on stock metrics and performace. 
The steps taken to create this dashboard include the following:",
                    tags$ul(
                      tags$li("Import and tidy the stock trading API data for a stock of choice"),
                      tags$li("Visualize trends and historical prices of the selected stock"),
                      tags$li("Investigate google trends interest over time in the selected stock"),
                      tags$li("Perform sentiment analysis on recent news articles about the selected stock"),
                      tags$li("Create a time series forecasting model that uses historical stock prices to predict future trends"))),
              mainPanel(
                verticalLayout(
                     box(
                       title = "Historical Price and Volume Trends",
                       width = 12,
                       height = 425,
                       plotlyOutput("plot1") %>% withSpinner(color="#0dc5c1"),
                       status="primary"
                     ),
                     box(
                       title = "Underlying Data",
                       width = 12,
                       height = 375,
                       dataTableOutput("table1") %>% withSpinner(color="#0dc5c1"),
                       status="primary"
                     )
                   )
                )
              )
            ),
    
    tabItem(tabName = "interest",
            h3("Interest Over Time Dashboard"),
            h4("An analysis of Google Trends interest over time in the selected stock."),
                verticalLayout( 
                  box(title = "Google Trends Over Time", width = 8, height = 360, plotlyOutput("plot2") %>% withSpinner(color="#0dc5c1"), status="primary"),
                  box(title = "Google Trends vs Close Stock Price", width = 8, height = 450, plotlyOutput("plot3") %>% withSpinner(color="#0dc5c1"), status="primary")
              )
    ),
    
    tabItem(tabName = "news",
            h3("Recent News Article Dashboard"),
            h4("Sentiment analysis of recent news articles pertaining to the selected company."),
                verticalLayout(
                  box(title = "Related News Articles", width = 8, height = 350, dataTableOutput("table2") %>% withSpinner(color="#0dc5c1"), status="primary"),
                  box(title = "Sentiment Analysis", width = 8, height = 450, plotlyOutput("plot5") %>% withSpinner(color="#0dc5c1"), status="primary"))),
    
    tabItem(tabName = "forecast",
            h3("Time Series Forecasting Dashboard"),
            h4("A forecast of closing prices for the selected stock one year into the future."),
                verticalLayout(
                  box(width = 8, height = 400, plotlyOutput("plot6") %>% withSpinner(color="#0dc5c1"), status="primary"),
                  box(width = 8, height = 400, plotlyOutput("plot7") %>% withSpinner(color="#0dc5c1"), status="primary"))
              )

  )
)

sidebar <- dashboardSidebar(
  width = 350,
    sidebarMenu(id = "sbmenu",
      menuItem("Inputs", icon = icon("font"), startExpanded = TRUE,
               textInput("symbol", "Enter a Stock Ticker", value = "AMZN"),
               actionButton("calcbtn", "Calculate"),
               lapply(seq(10), function(i) uiOutput(paste0("ui", i)))),
      menuItem("Stock Performance", tabName = "historical", icon = icon("chart-line")),
      menuItem("Interest Over Time", tabName = "interest", icon = icon("hashtag")),
      menuItem("News Analysis", tabName = "news", icon = icon("newspaper")),
      menuItem("Forecast", tabName = "forecast", icon = icon("hourglass")),
      menuItem("Report Page", href = "http://rpubs.com/mkivenson/stocks", icon = icon("mouse-pointer")),
      menuItem("Repo", href = "https://github.com/mkivenson/Data-Acquisition-and-Management/tree/master/Stock%20Project", icon = icon("github"))
    )
  )

header <- dashboardHeader(title = "Stock Overview", titleWidth = 350)
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
ui <- dashboardPage(header, sidebar, body, skin = "blue", useShinyjs())



# Define server logic required to draw a histogram
server <- function(input, output) {
  #stockRVal = reactiveVal()
  
  stock <- NULL
  symbol <- NULL
  key <- fromJSON(txt = "www/config.json")
  stock_apikey <- key$stock_apikey
  news_apikey <- key$news_apikey
  date_from <- Sys.Date() - 1825
  
  
  #output$table1 <- renderDataTable({
  #  datatable(stockRVal(), options = list(pageLength = 5), rownames = FALSE)
  #})
  
  observeEvent(input$calcbtn, {
    output$plot1 <- renderText({"Loading"})
    output$plot2 <- renderText({"Loading"})
    output$plot3 <- renderText({"Loading"})
    output$plot5 <- renderText({"Loading"})
    output$plot6 <- renderText({"Loading"})
    output$plot7 <- renderText({"Loading"})
    output$table1 <- renderText({"Loading"})
    output$table2 <- renderText({"Loading"})
    symbol <<- input$symbol    

    URL <- paste0("https://api.marketstack.com/v1/eod?access_key=", stock_apikey,
                  "&symbols=",symbol,
                  "&sort=DESC&date_from=",date_from,
                  "&limit=1000")
    results <- GET(url = URL)
    content <- content(results, "text")
    content %<>%
      fromJSON(flatten = TRUE) %>% #Flatten
      as.data.frame() #Make dataframe
    content <- content[5:ncol(content)]
    
    #extract the date and metric into a new field
    content$name <- content$data.symbol
    content$date <- as.Date(content$data.date)
    content$open <- content$data.open
    content$close <- content$data.close
    content$high <- content$data.high
    content$low <- content$data.low
    content$volume <- content$data.volume
    stock <<- content[14:20]
    

  
  observeEvent(input$sbmenu, {  #Not triggering
    print(input$sbmenu)
    if(input$sbmenu=="historical"){
      output$plot1 <- renderPlotly({
        p1 <- stock %>%
          plot_ly(x = ~date,
                  type = "candlestick", 
                  open = ~open, 
                  close = ~close, 
                  high = ~high,
                  low = ~low,
                  name = "price") %>%
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 1,
                    label = "1 mo",
                    step = "week",
                    stepmode = "backward"),
                  list(
                    count = 6,
                    label = "6 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "1 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 3,
                    label = "3 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(step = "all",
                       label = "max"))),
              rangeslider = list(visible = FALSE)),
            yaxis = list(title = "Price ($)",
                         showgrid = TRUE,
                         showticklabels = TRUE))
        p2 <- stock %>%
          plot_ly(x=~date, y=~volume, type='bar', name = "Volume", height = 350) %>%
          layout(yaxis = list(title = "Volume"))
        
        plot1 <- subplot(p1, p2, heights = c(0.6,0.4), nrows=2,
                         shareX = TRUE, titleY = TRUE, titleX = FALSE) %>%
          layout(title = paste0(symbol), legend = list(orientation = 'h', xanchor = "center", x = 0.5))
        plot1
      })
      
      
      output$table1 <- renderDataTable({
        datatable(stock, options = list(pageLength = 5), rownames = FALSE)
      })
    }
    
    else if(input$sbmenu=="interest"){
      
      #GET GOOGLE TRENDS DATA
      trends <- gtrends(keyword = symbol, geo = "US", onlyInterest = TRUE)
      trends <- trends$interest_over_time %>%
        as_data_frame() %>%
        select(c(date, hits, keyword))
      trends$date <- as.Date(cut(as.Date(trends$date), "week"))
      
      output$plot2 <- renderPlotly({
        plot2 <- trends %>%  
          plot_ly(x=~date, y=~hits, mode = 'lines', name = "Google Search Trends", height = 300) %>%
          layout(title = paste0(symbol, ": Interest over Time"), yaxis = list(title = "Google Trends Hits"))
        plot2
      })

      output$plot3 <- renderPlotly({
        plot3 <- trends %>%
          left_join(stock, by = "date") %>%
          select(one_of(c("date", "hits", "close"))) %>%
          drop_na() %>%
          ggplot(aes(hits, close)) + geom_point(color="blue") + geom_smooth(model=lm, color = "black") +
          labs(title = paste0(symbol,": Relationship between Hits and Close Stock Price"), 
               x = "Google Trends Hits", 
               y = "Close Price")
        plot3
      })
      
      #output$plot4 <- renderPlot({
      #  plot4 <- wordcloud(words = words_only$word, freq = words_only$n, scale=c(3,.3), max.words=50, colors=brewer.pal(8, "Dark2"))
      #  plot4
      #})
    }
    
    else if(input$sbmenu=="news"){
      ##get company name using web-scraping
      url_overview = paste0("https://www.marketwatch.com/investing/stock/",symbol,"/profile")
      var_overview = read_html(url_overview)
      company <-  var_overview %>% 
        html_nodes('#instrumentname') %>%
        html_text() %>%
        as.character()
      
      #news API Query
      url_news = paste0("https://newsapi.org/v2/everything?q=",
                        str_replace_all(company,pattern = " ", replacement = "%20"),
                        "&from=",Sys.Date() - 30, #last 30 days
                        "&sortBy=relevance&pageSize=100&language=en&apiKey=",news_apikey)
      #API json to datatable
      results <- GET(url = url_news)
      news <- content(results, "text")
      news %<>%
        fromJSON(flatten = TRUE) %>% #flatten
        as.data.frame() %>% #make dataframe
        unique()
      news$date <- as.Date(news$articles.publishedAt)
      news <- select(news, c("articles.title", "articles.description", "articles.content", "date"))
      
      
      output$table2 <- renderDataTable({
        datatable(select(news, c("articles.title", "articles.description","date")), 
                  options = list(pageLength = 100), 
                  fillContainer = TRUE,
                  rownames = FALSE)
      })
      

      news_words <- news %>%
        select(c("articles.title","articles.description", "articles.content", "date")) %>%
        unnest_tokens(word, articles.description) %>%
        filter(!word %in% append(stop_words$word, values = "chars"), str_detect(word, "^[a-z']+$"))
      
      #words_only <- news_words %>%
      #  count(word, sort =TRUE)
      afinn <- read_csv("./www/afinn.csv")
      
      sentiment_summary <- news_words %>%
        left_join(afinn) %>%
        filter(!is.na(value)) %>%
        group_by(articles.title, date) %>%
        summarise(score = mean(value)) %>%
        mutate(sentiment = ifelse(score>0, "positive","negative")) 
      
      
      output$plot5 <- renderPlotly({
        plot5 <- ggplot(sentiment_summary, aes(date, score)) + 
          geom_bar(stat = "identity", aes(fill=sentiment))  + 
          ggtitle(paste0(symbol, ": News Sentiment Over Time")) +
          theme(legend.position = "bottom")
        plot5
      })
    }
    else if(input$sbmenu=="forecast"){
      #pre-processing
      df <- stock %>%
        select(c("date","close")) %>%
        rename(ds = date, y = close)
      
      #predictions
      m <- prophet(df, uncertainty.samples = 100)
      future <- make_future_dataframe(m, periods = 365) %>% filter(!wday(ds) %in% c(1,7)) #account for regular gaps on weekends
      forecast <- predict(m, future) 
      
      #plot predictions 1 
      output$plot6 <- renderPlotly({
        plot6 <- plot(m, forecast, xlabel = "date", ylabel = "stock close price ($)") + ggtitle(paste0(symbol, ": Prophet Stock Price Prediction"))
        plot6
      })
      
      #plot predictions 2
      output$plot7 <- renderPlotly({
        forecast$ds <- as.Date(forecast$ds)
        plot7 <- df %>% 
          left_join(forecast[c('ds','yhat','yhat_lower','yhat_upper')], by = "ds") %>%
          filter(ds < Sys.Date()) %>%
          mutate(res = (y-yhat)) %>%
          ggplot(aes(ds, res)) + geom_point() + geom_hline(yintercept =0, color = "red") + labs(title =paste0(symbol, ": Prophet Forecasting Residuals"), x = "date", y = "residual") 
        plot7
      })
    }
  })
  
})
  

click("calcbtn")

  
}

# Run the application 
shinyApp(ui = ui, server = server)