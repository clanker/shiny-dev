#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# k = 1.1-1.4:  Exercises pp. 21-22
k <- 1.2

if (k == 1.1) {  ## Exercise 1, p. 21
    ui <- fluidPage(
        textInput("name", "", value = "", width = 200, 
                  placeholder = "Your name"),
        textOutput("greeting")
    )
    
    server <- function(input, output, session) {
        output$greeting <- renderText({
            paste0("Hello ", input$name)
        })
    }
}

if (k == 1.2) {  ## Exercise 2, p. 21
    ui <- fluidPage(
        sliderInput("delivery", "When should we deliver?",
                    min = strptime("2020-09-16", format = "%Y-%m-%d"),
                    max = strptime("2020-09-23", format = "%Y-%m-%d"),
                    value = strptime("2020-09-17", format = "%Y-%m-%d"),
                    step = as.difftime(1, format = "%d", units = "days")),
        textOutput("announce_days_to_delivery")
    )

    server <- function(input, output, session) {
        output$announce_days_to_delivery <- renderText({
            paste0("The delivery is scheduled in ",
                   input$delivery - strptime("2020-09-16", format = "%Y-%m-%d"),
                   " days")
        })
    }
}


# Run the application 
shinyApp(ui, server)
