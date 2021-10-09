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
k <- 1.1

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


# Run the application 
shinyApp(ui, server)
