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

    # "1. эдийн засаг"
country<-c("Mongolia","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

inflation <- read_excel("inflation.xls")

year<-c(1980:2022) #making a vector consisting of all years
year<-as.character(year)#converting to character type to use in gather()

# Inflation --
inf<-inflation %>% gather(all_of(year),key = "Year",value="InflationRate")
inf<-na.omit(inf) #omitting NA values

names(inf)<-c("region","year","inflation")

inf$year<-as.integer(inf$year)

Mongolia<-filter(inf,region=="Mongolia")
Mongolia$inflation<- as.numeric(Mongolia$inflation)
Mongolia$year<-as.numeric(Mongolia$year)

China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
EU<-filter(inf,region=="European Union")
UK<-filter(inf,region=="United Kingdom")
Fr<-filter(inf,region=="France")
India<-filter(inf,region== "India")

    # "3. Хувьцаа" 
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
#UI=======================================================================================================================================
  ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "Chinguun Finance"),
    dashboardSidebar(
      hr(),
      sidebarMenu( 
        HTML(paste0(
          "<br>",
          "<a href='https://ecnclub.com/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='THE ECONOMIST LOGO_drew (1).png' width = '120'></a>",
          "<br>"
        )),
                  menuItem("Эдийн засаг", tabName = "ecn", icon = icon("globe")), 
                  menuItem("Тооцоолуур", tabName = "тооцоолуур", icon = icon("calculator")), 
                  menuItem("Хувьцаа - үзүүлэлт", tabName = "хувьцаа", icon = icon("money-bill"), selected = TRUE), 
                  menuItem("Ипотекийн зээл", tabName = "mortgage", icon = icon("house")), 
                  #menuItem("Кластер шинжилгээ", tabName = "кластер шинжилгээ", icon = icon("circle-nodes")), 
        menuItem("Онол", icon=icon("book"),  
                 menuSubItem("stock_price_predict.R", tabName = "nvidia", icon = icon("book")),
                 menuSubItem("Fincal.R", tabName = "fincal", icon = icon("book")),
                 menuSubItem("quantmod.R", tabName = "quantmod", icon = icon("book"))
        ),
        menuItem("Эх код", icon=icon("code"),
                 menuSubItem("Economic.R", tabName = "ecoR", icon = icon("angle-right")),
                 menuSubItem("Calculator.R", tabName = "calcR", icon = icon("angle-right")),
                 menuSubItem("Stock.R", tabName = "stokR", icon = icon("angle-right")),
                 menuSubItem("Mortgage.R", tabName = "mortR", icon = icon("angle-right"))
        ),
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
          "<tr>",
          "<td style='padding: 5px;'><a href='https://www.facebook.com/chinguun.ganbaatar.90' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
          "<td style='padding: 5px;'><a href='https://twitter.com/chingvvn_senor' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
          "<td style='padding: 5px;'><a href='https://www.instagram.com/chingvvn_senor/' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
          "<td style='padding: 5px;'><a href='https://github.com/Chingvvnsenor' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
          "</tr>",
          "</table>",
          "<br>"),
          HTML(paste0(
            "<script>",
            "var today = new Date();",
            "var yyyy = today.getFullYear();",
            "</script>",
            "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.instagram.com/chingvvn_senor/' target='_blank'>Chinguun.G</a> - <script>document.write(yyyy);</script></small></p>")
          ))
      )
    ),
    # DASHBOARD-ийн их бие
    dashboardBody(
      
      tabItems(
        #1 Эдийн засаг- UI  -------------------------------------------------------
        tabItem(tabName = "ecn",
                h2("Монголын эдийн засаг 2022",align="center") ,
                h5("Эх сурвалж : Үндэсний Статистикийн Хороо",align="center") ,
                
                fluidRow(
                  valueBoxOutput("ecnbox1", width = 3),
                  valueBoxOutput("ecnbox2", width = 3),
                  valueBoxOutput("ecnbox3", width = 3),
                  valueBoxOutput("ecnbox4", width = 3),
                ),
                fluidRow(
                  box(width = 6, height="320px",
                      plotlyOutput("mgplot1", width = "100%", height = "300px")),
                  box(width = 6, height="320px",
                      plotlyOutput("mgplot2", width = "100%", height = "300px")),
                ),
                fluidRow(
                  box(width = 6, height="353px",
                      plotlyOutput("pie1")),
                  box(width = 6, height="353px",
                      plotlyOutput("pie2")),
                ),
                h2("Олон улсын эдийн засаг",align="center") ,
                h5("Эх сурвалж : Дэлхийн банк",align="center") ,
                fluidRow(
                  
                  column(12,
                         
                         box(selectInput("country",label="Улсаа сонгоно уу",choices=country),width = 12) 
                         
                  ),#end column
                  
                  #box for plotting the time series plot
                  column(12,
                         
                         box(
                           
                           highchartOutput("hcontainer"),
                           
                           width="12") #end box2
                  ), #end column
                  hr(),
                  h4("Харьцуулсан инфляцийн түвшин",align="center"),
                  br(),
                  column(12,
                         box(
                           highchartOutput("hc2"),width=12
                         ) )
                ),#end row
        ),
        #2 Тооцоолуур -------------------------------------------------------
        tabItem(
          tabName = "тооцоолуур",
          h2("Хадгаламж, Зээлийн тооцоолуур",align="center") ,
          fluidRow(
            box(width = 4, height = "623px",
                #title = "Хадгаламжийн тооцоолуур",
                status = "primary",
                solidHeader = TRUE,
                title = HTML("Хадгаламжийн тооцоолуур"),
                numericInput("principal", "Эхний орлого (₮)", value = 100000),
                numericInput("rate", "Жилийн хүү (%)", value = 0.12),
                numericInput("years", "Хугацаа (жил)", value = 15),
                numericInput("monthly_income", "Cар бүр хийх орлого (₮)", value = 50000),
                actionButton("calculate", "Тооцоолоорой"),
                hr(),
                #HTML('<p>Орон сууцны ипотекийн зээл гэдэг нь бүрэн баригдаж дууссан, Улсын комисс хүлээж ашиглалтад хүлээж авсан, улсын бүртгэлд бүртгэгдсэн, үл хөдлөх эд хөрөнгийн гэрчилгээгээр баталгаажсан, орон сууцны зориулалттай үл хөдлөх эд хөрөнгө барьцаалсан зээлийг хэлнэ.</p>'),
                tags$div(style = "text-align: justify;", "Хуримтлал гэдэг нь богино, дунд, урт хугацааны хэрэгцээгээ хангах зорилгоор хуримтлуулж буй мөнгө үнэт цаас болон бусад хөрөнгийг хэлнэ."),
                hr(),
                img(src = "saving.jpg", height = "90px", width = "100%")
                ),
            box(width = 8, height = "623px",
                status = "primary",
                solidHeader = TRUE,
                h3("Нийт мөнгөн дүн: ₮"),
                verbatimTextOutput("future_value"),
                h3("Цуглуулах хүү дангаараа: ₮"),
                verbatimTextOutput("cumulative_interest"),
                plotlyOutput("bar_chart")
                ),
          ),
          fluidRow(
            box(width = 4, height = "560px",
                #title = "Зээлийн тооцоолуур",
                status = "primary",
                solidHeader = TRUE,
                title = HTML("Зээлийн тооцоолуур"),
                numericInput("loan_amount", "Зээлийн хэмжээ ₮", value = 100000, min = 0),
                numericInput("interest_rate", "Хүүний түвшин (%)", value = 8, min = 0),
                numericInput("loan_term", "Хугацаа (жил)", value = 10, min = 0),
                actionButton("calculate", "Тооцоол"),
                hr(),
                tags$div(style = "text-align: justify;", "Иргэн, аж ахуйн нэгж, байгууллага ирээдүйн орлогоосоо эргүүлэн төлөх нөхцөлтэйгээр эрх бүхий байгууллагаас баталсан эрх зүйн зохицуулалтын хүрээнд гэрээ байгуулан олгох санхүүжилтийг зээл гэнэ."),
                hr(),
                tags$div(style = "text-align: justify;", "Банкууд зээл олгох хоёр гол шалгууртай. Нэгдүгээрт, тухайн иргэн, ААН, байгууллагад зээлээ эргүүлэн төлөх чадвар. Хоёрдугаарт, барьцаа хөрөнгө. Барьцаа хөрөнгө нь зээлийн эрсдэлийг бүрэн хаадаг.")
                ),
            box(width = 8, height = "560px",
                status = "primary",
                solidHeader = TRUE,
                h3(textOutput("monthly_payment")),
                h3(textOutput("total_payment")),
                h3(textOutput("interest_paid")),
                highchartOutput("loan_breakdown")
                )
          )
        ),
        #3 Хувьцаа үзүүлэлт ----------------------------------------------------
        tabItem(
          tabName = "хувьцаа",
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
          
        ),
        #4 ипотекийн зээл -------------------------------------------------------------
        tabItem(
          tabName = "mortgage",
          h2("Ипотекийн зээл 2022",align="center") ,
          fluidRow(
              box(width = 4, height = "610px",
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Ипотекийн зээлийн тооцоолуур",align="center") ,
                # Input: Simple integer interval ----
                numericInput("principals", "Эхлэх дүн (Зээлийн хэмжээ ₮)", 50000000, min = 1000000, step = 100000),
                hr(),
                numericInput("interest", "Жилийн хүүгийн түвшин (%)", 6, min = 0, max = 100, step = 0.1),
                hr(),
                setSliderColor(c("#9A32CD", "#FF4500", "", "Teal"), c(1, 2, 4)),
                sliderInput("length", "Зээлийн хугацаа (Жилээр)", 
                            min = 0,
                            max = 30,
                            value = 20,
                            step = 1
                ),
                hr(),
                  checkboxInput("plot", "График үзүүлэх?", TRUE),
                hr(),
                #HTML('<p>Орон сууцны ипотекийн зээл гэдэг нь бүрэн баригдаж дууссан, Улсын комисс хүлээж ашиглалтад хүлээж авсан, улсын бүртгэлд бүртгэгдсэн, үл хөдлөх эд хөрөнгийн гэрчилгээгээр баталгаажсан, орон сууцны зориулалттай үл хөдлөх эд хөрөнгө барьцаалсан зээлийг хэлнэ.</p>'),
                tags$div(style = "text-align: justify;", "Орон сууцны ипотекийн зээл гэдэг нь бүрэн баригдаж дууссан, Улсын комисс хүлээж ашиглалтад хүлээж авсан, улсын бүртгэлд бүртгэгдсэн, үл хөдлөх эд хөрөнгийн гэрчилгээгээр баталгаажсан, орон сууцны зориулалттай үл хөдлөх эд хөрөнгө барьцаалсан зээлийг хэлнэ.")
              ),
              box(width = 8, height = "295px",
                  status = "primary",
                  solidHeader = TRUE,
                  #h3("Нийт зээлдэгчийн тоо",align="center"),
                  highchartOutput("barPlot", height = "294px"),
                  tags$style(type="text/css", "#barPlot .highcharts-container { width: 100% !important; }")
                  # h3("Нийт зээлийн үлдэгдэл",align="center"),
                  # highchartOutput("barPlot2")
              ),
              box(width = 8, height = "295px",
                  status = "primary",
                  solidHeader = TRUE,
                  #h3("Нийт зээлийн үлдэгдэл",align="center"),
                  highchartOutput("barPlot2", height = "294px"),
                  tags$style(type="text/css", "#barPlot2 .highcharts-container { width: 100% !important; }")
              ),
              box(width = 12,
                  
                  status = "primary",
                  solidHeader = TRUE,
                # Output: Table summarizing the values entered ----
                uiOutput("number"),
                br(),
                plotOutput("distPlot"),
                br(),
                DT::dataTableOutput("tbl"),
                br(),
                p(em("Тодруулга: Энэхүү зээлийн тооцоолуурт ямар нэгэн санхүүгийн зөвөлгөө болон шинжилгээ багтаагүй болно. Энэхүү тооцоолуур нь зөвхөн таны зээлэнд хөрөнгө оруулж буй эрсдлийг тооцоолж харуулах зориулалттай юм. Энэхүү тооцоолуурт агуулагдсан мэдээлэлд үндэслэн гаргасан шийдвэр, түүнийг гуравдагч этгээд ашигласан тохиолдолд хариуцлага хүлээхгүй болно.")),
                p(em("Энэхүү R Shiny программ дээр бичигдсэн тооцоолуур код нь профессор Томас Гиркийн R код дээр суурилагдав.")),
                br(),
                br()
              ),
              box(width = 12, status = "primary",
                  h3("ИПОТЕКИЙН ЗЭЭЛ ОЛГОХ СХЕМ",align="center") ,
                  img(src = "ipootek.jpg", height = "100%", width = "100%"))
          )
        ),
        #5 Онол ойлголт -----------------------------------------------------------
        tabItem(
          tabName = "nvidia",
          fluidPage(
            tags$iframe(src = './stock_price_prediction_NVDA.html', 
                        width = '100%', height = '950px', 
                        frameborder = 0, scrolling = 'auto'
            )
          )
        ),
        tabItem(
          tabName = "fincal",
                fluidPage(
                  tags$iframe(src = './fincal-package.html', 
                              width = '100%', height = '950px', 
                              frameborder = 0, scrolling = 'auto'
                  )
                )
        ),
        tabItem(
          tabName = "quantmod",
                fluidPage(
                  tags$iframe(src = './quantmod-API-stock.html', 
                              width = '100%', height = '950px', 
                              frameborder = 0, scrolling = 'auto'
            )
          )
        ),
        #6 Эх код ----------------------------------------------------
        tabItem(tabName = "ecoR",
                box( width = NULL, status = "primary", solidHeader = TRUE, title="Эдийн засаг -> economic.R",
                     downloadButton('downloadData1', 'Download'),
                     br(),br(),
                     pre(includeText("economic.R"))
                )
        ),
        tabItem(tabName = "calcR",
                box( width = NULL, status = "primary", solidHeader = TRUE, title="Тооцоолуур -> calculator.R",
                     downloadButton('downloadData2', 'Download'),
                     br(),br(),
                     pre(includeText("calculator.R"))
                )
        ),
        tabItem(tabName = "stokR",
                box( width = NULL, status = "primary", solidHeader = TRUE, title="Хувьцаа үзүүлэлт -> stock.R",
                     downloadButton('downloadData3', 'Download'),
                     br(),br(),
                     pre(includeText("stock.R"))
                )
        ),
        tabItem(tabName = "mortR",
                box( width = NULL, status = "primary", solidHeader = TRUE, title="Ипотекийн зээл -> mortgage.R",
                     downloadButton('downloadData4', 'Download'),
                     br(),br(),
                     pre(includeText("mortgage.R"))
                )
        )
      )
    )
  )
  # ============================================================================================================================
  server <- function(input, output, session) {
    
    #1. эдийн засаг-server-------------------------------------------------------------
    output$ecnbox1 <- renderValueBox({
      valueBox(
        "3.7%", "Эдийн засгийн өсөлт", icon = icon("money-bill"),
        color = "navy"
      )
    })
    output$ecnbox2 <- renderValueBox({
      valueBox(
        "13.2%", "Инфляцийн түвшин", icon = icon("chart-bar"),
        color = "blue"
      )
    })
    output$ecnbox3 <- renderValueBox({
      valueBox(
        "5.4%", "Ажилгүйдлийн түвшин", icon = icon("briefcase", lib = "glyphicon"),
        color = "olive"
      )
    })
    output$ecnbox4 <- renderValueBox({
      valueBox(
        "3'409'939", "Хүн амын тоо", icon = icon("user", lib = "glyphicon"),
        color = "purple"
      )
    })
    
    datamgplot1 <- read_excel("tusuv.xlsx")
    output$mgplot1 <- renderPlotly({
    mgplot1 <- plot_ly(datamgplot1, x = ~жил, y = ~Нийт_орлого, type = 'bar', color = I("darkgreen"))
    mgplot1 <- mgplot1 %>% layout(title = "МОНГОЛ УЛСЫН НЭГДСЭН ТӨСВИЙН ОРЛОГО",
                          xaxis = list(title = "Жил"),
                          yaxis = list(title = "Мөнгөн дүн"),
                          annotations = list(
                            text = "Хэмжих нэгж - сая.төг",
                            x = 0.5,
                            y = 0.95,
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE
                          ))
    })
    #МОНГОЛ УЛСАД ОРСОН ГАДААДЫН ШУУД ХӨРӨНГӨ ОРУУЛАЛТЫН ОРОХ УРСГАЛ
    datamgplot2 <- read_excel("investment.xlsx")
    output$mgplot2 <- renderPlotly({
      mgplot2 <- plot_ly(datamgplot2, x = ~жил, y = ~Нийт_хөрөнгө_оруулалт, type = 'scatter', mode = 'lines', color = I("darkgreen"))
      mgplot2 <- mgplot2 %>% layout(title = "ГАДААДЫН ШУУД ХӨРӨНГӨ ОРУУЛАЛТЫН ОРОХ УРСГАЛ",
                                    xaxis = list(title = "Жил"),
                                    yaxis = list(title = "Мөнгөн дүн"),
                                    annotations = list(
                                      text = "Хэмжих нэгж - сая.ам.доллар",
                                      x = 0.5,
                                      y = 0.95,
                                      xref = "paper",
                                      yref = "paper",
                                      showarrow = FALSE
                                    ))
    })
    datapie1 <- c(15.04, 504.36, 11661.27, 359.12, 0.59)
    output$pie1 <- renderPlotly({
      plot_ly(labels = c("Газар тариалан", "Мал аж, ахуй", "Уул уурхай", "Аж үйлдвэрлэл", "Бусад"), values = datapie1, type = "pie", marker = list(colors = c("#833D98", "#804AA4", "#7065B3", "#6771B8", "#4F87BF"))) %>%
        layout(title = "Экспорт-боловсруулалтын түвшингээр", width = 530, height = 340)
    })
    datapie2 <- c(2686.05, 965.63, 3307.92, 1723.55, 21.26)
    output$pie2 <- renderPlotly({
      plot_ly(labels = c("Хэрэглээний", "Аж үйлдвэрлэл", "Хөрөнгө оруулалт", "Нефтийн", "Бусад"), values = datapie2, type = "pie", marker = list(colors = c("#833D98", "#2EA4C2", "#7065B3", "#6771B8", "#4F87BF"))) %>%
        layout(title = "Импорт-хэрэглээний зориулалтаар", width = 540, height = 340)
    })
    
    output$hcontainer <- renderHighchart ({
 
      
      df<-inf %>% filter(region==input$country)#making is the dataframe of the country
      
      df$inflation<-as.numeric(df$inflation)
      df$year<-as.numeric(df$year)
      #plotting the data
      hchart(df, name = "инфляцийн түвшин", "line",color="navy",hcaes(x=year,y=inflation))  %>%
        
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "white",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Инфляцийн түвшин 1980-2022",align="center") %>%
        hc_subtitle(text="Эх сурвалж: IMF",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
    })
    
    output$hc2<-renderHighchart({
      
      highchart() %>% 
        hc_xAxis(categories=inf$year) %>% 
        hc_add_series(name = "Germany", data = Ger$inflation) %>% 
        hc_add_series(name = "USA", data = US$inflation) %>%
        hc_add_series(name = "UK", data = UK$inflation) %>%
        hc_add_series(name = "China", data = China$inflation) %>%
        hc_add_series(name = "EU", data = EU$inflation) %>%
        hc_add_series(name="Japan",data=Japan$inflation) %>%
        hc_add_series(name="France",data=Fr$inflation) %>%
        hc_add_series(name="India",data=India$inflation) %>%
        #to add colors
        hc_colors(c("olive","blue","navy","purple","brown", "orange", "green", "black")) %>%
        hc_add_theme(hc_theme_elementary())
      
    })
    
    #2 Тооцоолуур ================================================================
    
    # Хадгаламжийн тооцоолуур
    observeEvent(input$calculate, {
      principal <- input$principal
      rate <- input$rate
      years <- input$years
      monthly_income <- input$monthly_income
      
      future_value <- principal * (1 + rate)^years + monthly_income * 12 * years
      cumulative_interest <- future_value - principal
      output$future_value <- renderText(future_value)
      output$cumulative_interest <- renderText(cumulative_interest)
      
      data <- data.frame(year = 1:years, value = principal)
      for (i in 2:years) {
        data[i, "value"] <- data[i-1, "value"] * (1 + rate) + monthly_income * 12
      }
      
      output$bar_chart <- renderPlotly({
        plot_ly(data, x = ~year, y = ~value, type = 'bar') %>%
          add_bars(marker = list(color = 'purple')) %>%
          layout(xaxis = list(title = 'Жил'),
                 yaxis = list(title = 'Мөнгөн дүн'),
                 title = 'Хуримтлагдсан хүүтэй хадгаламжийн өсөлт')
      })
    })
    # Зээлийн тооцоолуур =================
    loan_data <- reactiveValues(monthly_payment = 0, total_payment = 0, interest_paid = 0)
    
    observeEvent(input$calculate, {
      loan_amount <- input$loan_amount
      interest_rate <- input$interest_rate/100/12
      loan_term <- input$loan_term * 12
      monthly_payment <- loan_amount * interest_rate / (1 - (1 + interest_rate)^(-loan_term))
      total_payment <- monthly_payment * loan_term
      interest_paid <- total_payment - loan_amount
      
      loan_data$monthly_payment <- round(monthly_payment, 2)
      loan_data$total_payment <- round(total_payment, 2)
      loan_data$interest_paid <- round(interest_paid, 2)
    })
    
    output$monthly_payment <- renderText({
      paste0("Сар бүрийн тогтмол төлөлт: ₮ ", loan_data$monthly_payment)
    })
    
    output$total_payment <- renderText({
      paste0("Нийт буцаан төлөлт: ₮ ", loan_data$total_payment)
    })
    
    output$interest_paid <- renderText({
      paste0("Хүүний төлбөр: ₮ ", loan_data$interest_paid)
    })
    
    output$loan_breakdown <- renderHighchart({
      highchart() %>%
        hc_title(text = "Зээлийн муруй") %>%
        hc_xAxis(categories = c("Зээлийн хэмжээ", "Хүүний төлбөр", "Нийт буцаан төлөлт")) %>%
        hc_add_series(name = "Хэмжээ: ₮", data = c(input$loan_amount, loan_data$interest_paid, loan_data$total_payment), color = "purple")
    })
    
    #3 Хувьцааны график дүрслэл -------------------------------------------------
    
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
    #4 Ипотек зээлийн тооцоолуур, server----------------------------------------------------------
    
    # ипотек зээлдэгчдийн тоо & Зээлийн үлдэгдэл /өгөгдөл/
    df1 <- data.frame(жил=c("2017", "2018", "2019", "2020", "2021", "2022"), Нийт_зээлдэгчдийн_тоо=c(93147, 93865, 96671, 100496, 104364, 108633))
    df2 <- data.frame(жил=c("2017", "2018", "2019", "2020", "2021", "2022"), Нийт_зээлийн_үлдэгдэл=c(4253222160000, 4430220130000, 4628957990000, 4919173920000, 5654482000000, 6487800470000))
    output$barPlot <- renderHighchart({
      hc <- df1 %>%
        hchart('column', hcaes(x = жил, y = Нийт_зээлдэгчдийн_тоо),
               plotOptions = list(column = list(borderWidth = 0)), 
               chart = list(margin = c(50, 50, 100, 50)), color = "purple") %>%
        hc_subtitle(text = "Эх сурвалж: Монголбанк") 
    })
    output$barPlot2 <- renderHighchart({
      hc <- df2 %>%
        hchart('column', hcaes(x = жил, y = Нийт_зээлийн_үлдэгдэл),color = "#68228B") %>%
        hc_subtitle(text = "Эх сурвалж: Монголбанк, ₮") 
    })
    mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
      J <- I / (12 * 100)
      N <- 12 * L
      M <- P * J / (1 - (1 + J)^(-N))
      monthPay <<- M
      # Calculate Amortization for each Month
      if (amort == TRUE) {
        Pt <- P # current principals or amount of the loan
        currP <- NULL
        while (Pt >= 0) {
          H <- Pt * J # this is the current monthly interest
          C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
          Q <- Pt - C # this is the new balance of your principal of your loan
          Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
          currP <- c(currP, Pt)
        }
        monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
        aDFmonth <<- data.frame(
          Сар = 1:length(currP),
          Жил = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
          Баланс = c(currP[1:(length(currP))]),
          Төлөлт = monthP + c((monthPay - monthP)[1:(length(monthP))]),
          Үндсэн_зээл = monthP,
          Хүү = c((monthPay - monthP)[1:(length(monthP))])
        )
        aDFmonth <<- subset(aDFmonth, Жил <= L * 12)
        aDFyear <- data.frame(
          Amortization = tapply(aDFmonth$Баланс, aDFmonth$Жил, max),
          Annual_Payment = tapply(aDFmonth$Төлөлт, aDFmonth$Жил, sum),
          Annual_Principals = tapply(aDFmonth$Үндсэн_зээл, aDFmonth$Жил, sum),
          Annual_Interest = tapply(aDFmonth$Хүү, aDFmonth$Жил, sum),
          Жил = as.factor(na.omit(unique(aDFmonth$Жил)))
        )
        #Хүү
        aDFyear <<- aDFyear
      }
      if (plotData == TRUE) {
        aDFyear2 <- aDFyear %>%
          rename(
            Хүү = Annual_Interest,
            Төлөлт = Annual_Payment,
            Үндсэн_зээл = Annual_Principals
          )
        aDFyear2$Жил <- as.factor(aDFyear2$Жил)
        aDFyear2 <- melt(aDFyear2[, c("Хүү", "Үндсэн_зээл", "Жил")], id.vars = "Жил")
        
        ggplot(aDFyear2, aes(x = Жил, y = value, fill = variable)) +
          geom_bar(position = "fill", stat = "identity") +
          labs(y = "Төлөлт") +
          scale_y_continuous(labels = percent) +
          theme_minimal() +
          theme(legend.title = element_blank(), legend.position = "top")
      }
    }
    
    output$number <- renderUI({
      mortgage(P = input$principals, I = input$interest, L = input$length, plotData = TRUE)
      HTML(paste0(
        "<h3>", "Үр дүн", "</h3>",
        "Зээлэх мөнгөн дүн ₮: ", format(round(input$principals, 2), big.mark = ","),
        "<br>",
        "Жилийн хүүгийн түвшин: ", input$interest, "%",
        "<br>",
        "Хугацаа: ", input$length, " жил (", input$length * 12, " сар)",
        "<br>",
        "<b>", "Сар бүрийн төлөлт ₮: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
        "<br>",
        "<b>", "Нийт буцаан төлөлт ₮: ", "</b>", format(round(input$principals, 2), big.mark = ","), " (зээлийн хэмжээ) + ", format(round(monthPay * 12 * input$length - input$principals, 2), big.mark = ","), " (хүү) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
      ))
    })
    output$distPlot <- renderPlot({
      mortgage(P = input$principals, I = input$interest, L = input$length, plotData = input$plot)
    })
    # Data output
    output$tbl <- DT::renderDataTable({
      mortgage(P = input$principals, I = input$interest, L = input$length, plotData = FALSE)
      df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                                extensions = "Buttons",
                                options = list(
                                  lengthChange = TRUE,
                                  dom = "Blrtip",
                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                  
                                  lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                                ),
                                rownames = FALSE
      ) %>%
        formatCurrency(c("Баланс", "Төлөлт", "Үндсэн_зээл", "Хүү"), currency = "₮", interval = 3, mark = ",")
    })
    #Эх код--------
    
    #download file
    output$download1 <- downloadHandler(
      filename = function() {paste("economic.R", 'economicR.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "pdf")
      }
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)
  