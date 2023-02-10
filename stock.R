library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyquant)
library(remotes)
library(shinycssloaders)
library(highcharter)
library(readxl)
library(tidyr)
library(colourpicker)
library(httr)
library(dplyr)
library(DT)
library(reshape2)
library(scales)
options(scipen = 999)

# Хувьцаа 
tickers <- c("AAPL","MSFT","AMZN","TSLA","GOOGL","NVDA","NFLX", "META")
benchmarks <- c("^NDX","^GSPC")

prices <- tq_get(tickers,
                 get  = "stock.prices",
                 from = today()-months(120),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,close)

bench <- tq_get(benchmarks,
                get  = "stock.prices",
                from = today()-months(120),
                to   = today()) %>%
  select(symbol,date,close)

# Define UI for application that draws a histogram
ui <- fluidPage(
  h2("Хувьцаа 2023",align="center") ,
  fluidRow(
    box(width = 12,
        fixedRow(
          column(5,
                 pickerInput(
                   inputId = "stocks",
                   label = h4("Хувьцаанууд"),
                   choices = c(
                     "Apple Inc"       = tickers[1], 
                     "Microsoft Corp"   = tickers[2],
                     "Amazon.com"      = tickers[3],
                     "Tesla"         = tickers[4],
                     "Alphabet Inc. Class A /Google/"    = tickers[5],
                     "NIVIDA Corp"     = tickers[6],
                     "Netflix"       = tickers[7],
                     "Meta Platform"       = tickers[8]),
                   selected = tickers,   
                   options = list(`actions-box` = TRUE), 
                   multiple = T
                 ),
          ),
          # Pick time period
          column(4, 
                 radioButtons("period", label = h4("Хугацаа"),
                              choices = list("3 сар" = 1, "6 сар" = 2, "1 жил" = 3, "5 жил" = 4, "10 жил" = 5), 
                              selected = 4
                 ),
          ),
          # Pick benchmark
          column(3,
                 radioButtons("benchmark", label = h4("Бенчмарк"),
                              choices = list("SP500" = 1, "Nasdaq100" = 2,"None" = 3),
                              selected = 3)
          ),
        ),
    ),
    box(width = 12,
        title = "Хувьцааны график дүрслэл",
        plotlyOutput("plot",height=800),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # server logic based on user input
  observeEvent(c(input$period,input$stocks,input$benchmark), {
    
    prices <- prices %>% dplyr::filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(3)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(12)) }
    
    if (input$period == 4) {
      prices <- prices %>%
        filter(date >= today()-months(60)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(date >= today()-months(120)) }
    
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^GSPC",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^NDX",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    # Create plot
    output$plot <- renderPlotly({
      print(
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   ungroup() %>%
                   ggplot(aes(date, value,colour = symbol)) +
                   geom_line(size = 1, alpha = .9) +
                   # uncomment the line below to show area under curves
                   #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "white"),
                         panel.background = element_rect(fill="white"),
                         panel.grid = element_blank(),
                         legend.text = element_text(colour="black"))
        )
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
