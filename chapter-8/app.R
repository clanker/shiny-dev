#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    numericInput("n", "n", value = 10),
    textOutput("half")
)

server <- function(input, output, session) {
    half <- reactive({
        even <- input$n %% 2 == 0
        shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
        req(even)
        input$n / 2    
    })
    
    output$half <- renderText(half())
}

# Run the application 
shinyApp(ui = ui, server = server)
