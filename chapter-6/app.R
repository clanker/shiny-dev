library(shiny)

code_part <- 1

if (code_part == 1) {

    # fluidPage() = sets up all the HTML, CSS, and JavaScript to make Shiny 
    #     window. 
    #     Also fixedPage() = fixed width, fillPage() = full height of browser
    ui <- fluidPage(
        # titlePanel(), sidebarLayout() for 2-column layout, with sidebarPanel()
        #     and mainPanel()
        titlePanel("Title = full width of window"),  # app title/description
        sidebarLayout(
            sidebarPanel(  # inputs, left column
            ),
            mainPanel(   # outputs, right column
            )
        )
    )
    
    ui <- fluidPage(
        titlePanel("Central limit theorem"),
        sidebarLayout(
            sidebarPanel(
                numericInput("m", "Number of samples:", 2, min = 1, max = 100)
            ),
            mainPanel(
                plotOutput("hist")
            )
        )
    )
    server <- function(input, output, session) {
        output$hist <- renderPlot({
            means <- replicate(1e4, mean(runif(input$m)))
            hist(means, breaks = 20)
        }, res = 96)
    }
}

# UI:
#   fluidPage(
#       fluidRow(
#       column(4, ...),
#       column(4, ...), ...
#       ),
#       fluidRow(
#       column(4, ...),
#       column(4, ...), ...
#       ),
#   )

# Run the application 
shinyApp(ui = ui, server = server)
