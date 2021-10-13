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

count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
    fluidRow(
        column(8, selectInput("code", "Product", choices = prod_codes)),
        column(4, radioButtons("yaxis", "Choose y-axis variable:", 
                               c("n", "rate"), selected = "rate"))
    ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    fluidRow(
        column(12, div(style = "height:360px", plotOutput("age_sex")))
        # column(6, plotOutput("age_sex_rate"))
    ),
    fluidRow(
        column(3, actionButton("story", "Give me crazy examples!")),
        column(9, textOutput("story"))
    )
)

server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    output$diag <- renderTable(count_top(selected(), diag), width = "100%")

    output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")

    output$location <- renderTable(count_top(selected(), location), width = "100%")
    
    # output$diag <- renderTable(
    #     selected() %>% count(diag, wt = weight, sort = TRUE)
    # )
    # output$body_part <- renderTable(
    #     selected() %>% count(body_part, wt = weight, sort = TRUE)
    # )
    # output$location <- renderTable(
    #     selected() %>% count(location, wt = weight, sort = TRUE)
    # )
    
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    output$age_sex <- renderPlot({
        if (input$yaxis == "n") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                stat_smooth(se = F) +
                labs(y = "Estimated number of injuries")
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line() +
            stat_smooth(se = F) +
            labs(y = "Estimated number of injuries per 10k")
        }
    }, res = 96, height = 300, width = 900)

    injury_example <- eventReactive(
        list(input$story, selected()),
        selected() %>% pull(narrative) %>% sample(5)
    )
    
    output$story <- renderPrint(injury_example())
}

# Run the application 
shinyApp(ui = ui, server = server)
