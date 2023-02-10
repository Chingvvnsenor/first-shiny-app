library(shiny)
library(highcharter)
library(plotly)
library(ggplot2)
library(shinydashboard)

ui <- fluidPage(
  h2("Хадгаламж, Зээлийн тооцоолуур",align="center") ,
  fluidRow(
    box(width = 4, height = "623px",
        #title = "Хадгаламжийн тооцоолуур",
        status = "primary",
        solidHeader = TRUE,
        title = HTML("Хадгаламжийн тооцоолуур"),
        numericInput("principal", "Эхний орлого (₮)", value = 1000),
        numericInput("rate", "Жилийн хүү (%)", value = 0.05),
        numericInput("years", "Хугацаа (жил)", value = 10),
        numericInput("monthly_income", "Cар бүр хийх орлого (₮)", value = 0),
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
)

server <- function(input, output) {
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
}

shinyApp(ui, server)