library(shinydashboard)
require(shiny)
require(highcharter)
require(shinydashboard)
require(ggplot2)
require(dplyr)
require(highcharter) #to plot amazing time series plots
library(readxl)
require(tidyr)
library(plotly)

# Run this R code firstly 
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

# Define UI for application that draws a histogram
ui <- fluidPage(
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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
}

# Run the application 
shinyApp(ui = ui, server = server)
