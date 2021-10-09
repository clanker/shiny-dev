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
k <- 1.4

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

if (k == 1.3) {  ## Exercise 3, p. 22
    ui <- fluidPage(
        sliderInput("delivery_units", "How many units should we deliver?",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 5,
                    animate = animationOptions(
                        interval = 100,
                        loop = TRUE,
                        playButton = "Start animation",
                        pauseButton = "Stop animation"
                    )
        ),
        textOutput("announce_units_to_deliver")
    )
    
    server <- function(input, output, session) {
        output$announce_units_to_deliver <- renderText({
            paste0("The delivery will have ", input$delivery_units, " units")
        })
    }
}

if (k == 1.4) {  ## Exercise 4, p. 22
    selection_list <- list()
    selection_list[[1]] <- c('a', 'b', 'c')
    selection_list[[2]] <- c('x', 'y', 'z')
    names(selection_list) <- c("Group 1", "Group N")
    ui <- fluidPage(
        selectInput("selection", "What is your selection?", selection_list),
        textOutput("announce_selection")
    )
    
    server <- function(input, output, session) {
        output$announce_selection <- renderText({
            paste0("You have selected ", input$selection)
        })
    }
}

# Run the application 
shinyApp(ui, server)
