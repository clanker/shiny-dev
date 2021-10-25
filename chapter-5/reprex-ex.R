# reprex-ex.R

# A simple reprex ================================
library(shiny)
ui <- fluidPage(
  selectInput("n", "N", 1:10),
  plotOutput("plot")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    n <- input$n
    message(glue::glue("n = {n}"))
    plot(head(cars, n))
  })
}
shinyApp(ui, server)


# dput ===========================================

mydata <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"))
mydata  # lists data.frame
dput(mydata)  # gives structure of the code making mydata

# copy output of dput(mydata) and turn that into mydata
mydata <- structure(
  list(
    x = 1:5, 
    y = structure(1:5, .Label = c("a", "b", "c", "d", "e"), class = "factor")
  ),
  class = "data.frame", row.names = c(NA, -5L)
)
# confirm they are equal
mydata


# Problem exists, try to reprex ======================
library(xts)
library(lubridate)
library(shiny)

ui <- fluidPage(
  uiOutput("interaction_slider"),
  verbatimTextOutput("breaks")
)
server <- function(input, output, session) {
  df <- data.frame(
    dateTime = c(
      "2019-08-20 16:00:00",
      "2019-08-20 16:00:01",
      "2019-08-20 16:00:02",
      "2019-08-20 16:00:03",
      "2019-08-20 16:00:04",
      "2019-08-20 16:00:05"
    ),
    var1 = c(9, 8, 11, 14, 16, 1),
    var2 = c(3, 4, 15, 12, 11, 19),
    var3 = c(2, 11, 9, 7, 14, 1)
  )
  
  timeSeries <- as.xts(df[, 2:4], 
                       order.by = strptime(df[, 1], format = "%Y-%m-%d %H:%M:%S")
  )
  print(paste(min(time(timeSeries)), is.POSIXt(min(time(timeSeries))), sep = " "))
  print(paste(max(time(timeSeries)), is.POSIXt(max(time(timeSeries))), sep = " "))
  
  output$interaction_slider <- renderUI({
    sliderInput(
      "slider",
      "Select Range:",
      min = min(time(timeSeries)),
      max = max(time(timeSeries)),
      value = c(min, max)
    )
  })
  
  brks <- reactive({
    req(input$slider)
    seq(input$slider[1], input$slider[2], length.out = 10)
  })
  
  output$breaks <- brks
}
shinyApp(ui, server)


# Simpler reprex ---------------------------------------------------
library(xts)
library(shiny)

ui <- fluidPage(
  uiOutput("interaction_slider"),
  verbatimTextOutput("breaks")
)

datetime <- Sys.time() + (86400 * 0:10)

server <- function(input, output, session) {
  output$interaction_slider <- renderUI({
    sliderInput(
      "slider",
      "Select Range:",
      min   = min(datetime),
      max   = max(datetime),
      value = c(min, max)
    )
  })
  
  brks <- reactive({
    req(input$slider)
    seq(input$slider[1], input$slider[2], length.out = 10)
  })
  
  output$breaks <- brks
}
shinyApp(ui, server)


# moving output$interaction_slider to ui
library(xts)
library(shiny)

datetime <- Sys.time() + (86400 * 0:10)

ui <- fluidPage(
  sliderInput("slider",
              "Select Range:",
              min   = min(datetime),
              max   = max(datetime),
#              value = c(min, max)
              value = range(datetime)
  ),
  verbatimTextOutput("breaks")
)

server <- function(input, output, session) {
  brks <- reactive({
    req(input$slider)
    seq(input$slider[1], input$slider[2], length.out = 10)
  })
  
  output$breaks <- brks
}
shinyApp(ui, server)
