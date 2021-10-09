#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Designate the example Shiny app to run:
# k:
#   1, 2;  in-chapter examples
#   3-7;  Exercise 1-5
k <- 7

if (k == 1) {  # First example
    
    ui <- fluidPage(
        "Hello, World!"
    )
    server <- function(input, output, session) {
    }
    
}

if (k == 2) {  # Second example
    
    ui <- fluidPage(
        selectInput("dataset", 
                    label = "Dataset", 
                    choices = ls("package:datasets")),
        verbatimTextOutput("summary"),
        tableOutput("table")
    )
    
    server <- function(input, output, session) {
        dataset <- reactive({
            get(input$dataset, "package:datasets")
        })
        
        output$summary <- renderPrint({
            summary(dataset())
        })
        
        output$table <- renderTable({
            dataset()
        })
    }
    
}

if (k == 3) {  ## Question 1-1.
    
    ui <- fluidPage(
        textInput("name", "What's your name?"),
        textOutput("greeting")
    )
    
    server <- function(input, output, session) {
        output$greeting <- renderText({
            paste0("Hello ", input$name)
        })
    }
    
}

if (k == 4) {  ## Question 1-2.
    
    ui <- fluidPage(
        sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
        "then 5x is ",
        textOutput("product")
    )
    
    server <- function(input, output, session) {
        output$product <- renderText({
            5 * input$x
        })
    }
    
}

if (k == 5) {  ## Question 1-3.
    
    ui <- fluidPage(
        sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
        sliderInput("y", label = "and y is", min = 1, max = 50, value = 30),
        "then x times y is ",
        textOutput("product")
    )
    
    server <- function(input, output, session) {
        output$product <- renderText({
            input$x * input$y
        })
    }
    
}

if (k == 6) {  ## Question 1-4.
    
    ui <- fluidPage(
        sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
        sliderInput("y", label = "and y is", min = 1, max = 50, value = 30),
        "then x times y is ", textOutput("product"),
        "and (x * y) + 5 is ", textOutput("product_plus5"),
        "and (x * y) + 10 is ", textOutput("product_plus10"),
    )
    
    server <- function(input, output, session) {
        product <- reactive(input$x * input$y)
        output$product <- renderText({ product() })
        output$product_plus5 <- renderText({ product() + 5 })
        output$product_plus10 <- renderText({ product() + 10 })
    }
    
}

if (k == 7) {  ## Question 1-5.
    
    library(ggplot2)
    datasets <- c("economics", "faithfuld", "seals")
    ui <- fluidPage(
        selectInput("dataset", label = "Dataset", choices = datasets),
        verbatimTextOutput("summary"),
        plotOutput("plot")
    )
    
    server <- function(input, output, session) {
        dataset <- reactive({
            get(input$dataset, "package:ggplot2")
        })
        output$summary <- renderPrint({
            summary(dataset())
        })
        output$plot <- renderPlot({
            plot(dataset(), pch=3)
        }, res = 96)
    }
    
}

# Run the application
shinyApp(ui = ui, server = server)
