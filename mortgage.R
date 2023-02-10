library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Ипотекийн зээлийн тооцоолуур"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("principal", "Эхлэх дүн (Зээлийн хэмжээ ₮)", 200000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Жилийн хүүгийн хэмжээ (%)", 2, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Зээлийн хугацаа (Жилээр)",
                  min = 0,
                  max = 30,
                  value = 25,
                  step = 1
      ),
       hr(),
       checkboxInput("plot", "График гаргах?", TRUE),
      hr(),
      HTML('<p>Орон сууцны ипотекийн зээл гэдэг нь бүрэн баригдаж дууссан, Улсын комисс хүлээж ашиглалтад хүлээж авсан, улсын бүртгэлд бүртгэгдсэн, үл хөдлөх эд хөрөнгийн гэрчилгээгээр баталгаажсан, орон сууцны зориулалттай үл хөдлөх эд хөрөнгө барьцаалсан зээлийг хэлнэ.</p>')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      DT::dataTableOutput("tbl"),
      br(),
      p(em("Тодруулга: Энэхүү зээлийн тооцоолуурт ямар нэгэн санхүүгийн зөвөлгөө болон шинжилгээ багтаагүй болно. Энэхүү тооцоолуур нь зөвхөн таны зээлэнд хөрөнгө оруулж буй эрсдлийг тооцоолж харуулах зориулалттай юм. Энэхүү тооцоолуурт агуулагдсан мэдээлэлд үндэслэн гаргасан шийдвэр, түүнийг гуравдагч этгээд ашигласан тохиолдолд хариуцлага хүлээхгүй болно.")),
      p(em("Энэхүү R Shiny программ дээр бичигдсэн тооцоолуур код нь профессор Томас Гиркийн R код дээр суурилдаг.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
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
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      
      ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  }
  
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Үр дүн", "</h3>",
      "Зээлэх мөнгөн дүн ₮: ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Жилийн хүүгийн түвшин: ", input$interest, "%",
      "<br>",
      "Хугацаа: ", input$length, " жил (", input$length * 12, " сар)",
      "<br>",
      "<b>", "Сар бүрийн төлөлт ₮: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Нийт буцаан төлөлт ₮: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (зээлийн хэмжээ) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (хүү) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
  output$distPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })
  
  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
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
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
  })
}

# Run the application
shinyApp(ui = ui, server = server)