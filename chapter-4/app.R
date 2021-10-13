#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vroom)
library(tidyverse)

prod_codes <- setNames(products$prod_code, products$title)

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

ui <- fluidPage(
    fluidRow(
        column(6, selectInput("code", "Product", choices = prod_codes))
    ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    fluidRow(
        column(6, plotOutput("age_sex")),
        column(6, plotOutput("age_sex_rate"))
    )
)

server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    output$diag <- renderTable(
        selected() %>% count(diag, wt = weight, sort = TRUE)
    )
    output$body_part <- renderTable(
        selected() %>% count(body_part, wt = weight, sort = TRUE)
    )
    output$location <- renderTable(
        selected() %>% count(location, wt = weight, sort = TRUE)
    )
    
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    output$age_sex <- renderPlot({
        summary() %>%
            ggplot(aes(age, n, colour = sex)) +
            geom_line() +
            labs(y = "Estimated number of injuries")
    }, res = 96)

    output$age_sex_rate <- renderPlot({
        summary() %>% 
            ggplot(aes(age, rate, color = sex)) + 
            geom_line() + 
            stat_smooth() +
            labs(y = "Estimated number of injuries per 10k")
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
