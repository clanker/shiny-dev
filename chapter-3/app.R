#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## k = 1.1-1.2:  Chap. 3 Exercises, p. 35 
## k = 2.1-2.4:  Chap. 3, pp. 36-48
## k = 3:  Chap. 3, p. 49, Observers
k <- 3

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

if (dplyr::between(k, 2.1, 2.4)) {  ## Chapter 3, Reactive Expressions
    library(ggplot2)
    
    freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
        df <- data.frame(
            x = c(x1, x2),
            g = c(rep("x1", length(x1)), rep("x2", length(x2)))
        )
        
        ggplot(df, aes(x, after_stat(density), colour = g)) +
            geom_freqpoly(binwidth = binwidth, size = 1) +
            coord_cartesian(xlim = xlim)
    }
    
    t_test <- function(x1, x2) {
        test <- t.test(x1, x2)
        
        # use sprintf() to format t.test() results compactly
        sprintf(
            "p value: %0.3f\n[%0.2f, %0.2f]",
            test$p.value, test$conf.int[1], test$conf.int[2]
        )
    }
    
    # x1 <- rnorm(100, mean = 0, sd = 0.5)
    # x2 <- rnorm(200, mean = 0.15, sd = 0.9)
    # 
    # freqpoly(x1, x2)
    # cat(t_test(x1, x2))
    # #> p value: 0.457 [-0.22, 0.10]
    
    ui <- fluidPage(
        fluidRow(
            column(4, 
                   "Distribution 1",
                   numericInput("n1", label = "n", value = 1000, min = 1),
                   numericInput("mean1", label = "µ", value = 0, step = 0.1),
                   numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
            ),
            column(4, 
                   "Distribution 2",
                   numericInput("n2", label = "n", value = 1000, min = 1),
                   numericInput("mean2", label = "µ", value = 0, step = 0.1),
                   numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
            ),
            column(4,
                   "Random seed",
                   numericInput("seed", label = "Seed:", min = 0, max = 2^30-1, value = 0),
                   "Frequency polygon",
                   numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
                   sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5),
                   if (k >= 2.3)  actionButton("simulate", "Simulate!")
            )
        ),
        fluidRow(
            column(9, plotOutput("hist")),
            column(3, verbatimTextOutput("ttest"))
        )
    )
    
    server <- function(input, output, session) {
        if (k == 2.1) {
            x1 <- reactive({
                set.seed(input$seed)
                rnorm(input$n1, input$mean1, input$sd1)
            })
            x2 <- reactive({
                set.seed(input$seed+2^30)
                rnorm(input$n2, input$mean2, input$sd2)
            })
        } else if (k == 2.2) {
            timer <- reactiveTimer(500)
            
            x1 <- reactive({
                timer()
                rnorm(input$n1, input$mean1, input$sd1)
            })
            x2 <- reactive({
                timer()
                rnorm(input$n2, input$mean2, input$sd2)
            })
        } else if (k == 2.3) {
            x1 <- reactive({
                input$simulate
                rnorm(input$n1, input$mean1, input$sd1)
            })
            x2 <- reactive({
                input$simulate
                rnorm(input$n2, input$mean2, input$sd2)
            })
        } else if (k == 2.4) {
            x1 <- eventReactive(input$simulate, {
                rnorm(input$n1, input$mean1, input$sd1)
            })
            x2 <- eventReactive(input$simulate, {
                rnorm(input$n2, input$mean2, input$sd2)
            })
        }
        
        output$hist <- renderPlot({
            freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
        })
        
        output$ttest <- renderText({
            t_test(x1(), x2())
        })
    }
}

if (k == 3) {  ## Chap. 3, p. 49, Observers
    ui <- fluidPage(
        textInput("name", "What's your name?"),
        textOutput("greeting")
    )
    
    server <- function(input, output, session) {
        string <- reactive(paste0("Hello ", input$name, "!"))
        
        output$greeting <- renderText(string())
        observeEvent(input$name, {
            message("Greeting performed")
        })
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
