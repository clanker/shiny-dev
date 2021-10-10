#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## k = 1:  Chap. 3 Exercises, p. 35 
k <- 1.1

if (k == 1.1) {  ## Exercise 1, p. 35
    ui <- fluidPage(
        textInput("name", "What's your name?"),
        textOutput("greeting")
    )

    server1 <- function(input, output, server) {
        output$greeting <- renderText(paste0("Hello ", input$name))
    }
    
    server2 <- function(input, output, server) {
        greeting <- reactive(paste0("Hello ", input$name))
        output$greeting <- renderText(greeting())
    }
    
    server <- function(input, output, server) {
        output$greeting <- renderText(paste0("Hello ", input$name))
    }
}

if (k == 1.2) {
    
    server1 <- function(input, output, session) {
        c <- reactive(input$a + input$b)
        e <- reactive(c() + input$d)
        output$f <- renderText(e())
    }
#   (a OR b) -> c
#   (c OR d) -> e -> f 
    
    server2 <- function(input, output, session) {
        x <- reactive(input$x1 + input$x2 + input$x3)
        y <- reactive(input$y1 + input$y2)
        output$z <- renderText(x() / y())
    }
#   (x1 OR x2 OR x3) -> x
#   (y1 OR y2) -> y
#   (x OR y) -> z
    
    server3 <- function(input, output, session) {
        d <- reactive(c() ^ input$d)
        a <- reactive(input$a * 10)
        c <- reactive(b() / input$c) 
        b <- reactive(a() + input$b)
    }
#   input$a -> a
#   (a OR input$b) -> b
#   (b OR input$c) -> c
#   (c OR input$d) -> d
#   ... except none of these are calculated as there is no HTML output
}


# Run the application 
shinyApp(ui = ui, server = server)
