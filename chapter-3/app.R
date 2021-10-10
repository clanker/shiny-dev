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
k <- 2.1

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

if (k == 2.1) {  ## Chapter 3, Reactive Expressions
    library(ggplot2)
    
    freqpoly <- function(x1, x2) {
        df <- data.frame(
            x = c(x1, x2),
            g = c(rep("x1", length(x1)), rep("x2", length(x2)))
        )
        
        ggplot(df, aes(x, after_stat(density), colour = g)) +
            geom_freqpoly(binwidth = diff(range(df$x))/24, size = 1) +
            coord_cartesian(xlim = c(min(df$x), max(df$x)))
    }
    
    t_test <- function(x1, x2) {
        test <- t.test(x1, x2)
        
        # use sprintf() to format t.test() results compactly
        sprintf(
            "p value: %0.3f\n[%0.2f, %0.2f]",
            test$p.value, test$conf.int[1], test$conf.int[2]
        )
    }

    x1 <- rnorm(100, mean = 0, sd = 0.5)
    x2 <- rnorm(200, mean = 0.15, sd = 0.9)

    freqpoly(x1, x2)
    cat(t_test(x1, x2))
    #> p value: 0.457 [-0.22, 0.10]
    
}

# Run the application 
shinyApp(ui = ui, server = server)
