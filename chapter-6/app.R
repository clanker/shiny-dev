library(shiny)

# Chapter 6.1 - 6.3
code_part <- 3

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
} else {
    ui <- fluidPage(
        tabsetPanel(
            tabPanel("Import data", 
                     fileInput("file", "Data", buttonLabel = "Upload..."),
                     textInput("delim", "Delimiter (leave blank to guess)", ""),
                     numericInput("skip", "Rows to skip", 0, min = 0),
                     numericInput("rows", "Rows to preview", 10, min = 1)
            ),
            tabPanel("Set parameters"),
            tabPanel("Visualise results")
        )
    )
    
    server <- function(input, output, session) {
        output$hist <- renderPlot({
            means <- replicate(1e4, mean(runif(input$m)))
            hist(means, breaks = 20)
        }, res = 96)
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
    
    
    # MULTIPAGE
    # fluidPage(
    #   tabsetPanel(
    #     tabPanel("title",
    #       ... 
    #       ),
    #     tabPanel("title",
    #      ... 
    #      ),
    #      ...
    #   )
    # )
    # 
    
    # ALSO: see this as an example:
    # 
    # ui <- fluidPage(
    #     sidebarLayout(
    #         sidebarPanel(
    #             textOutput("panel")
    #         ),
    #         mainPanel(
    #             tabsetPanel(
    #                 id = "tabset",   # id --> input$tabset for use in server()
    #                 tabPanel("panel 1", "one"),
    #                 tabPanel("panel 2", "two"),
    #                 tabPanel("panel 3", "three")
    #             )
    #         )
    #     )
    # )
    # server <- function(input, output, session) {
    #     output$panel <- renderText({
    #         paste("Current panel: ", input$tabset)   # --> "panel" for use in ui()
    #     })
    # } 
    
    # ALSO:
    # Navigation lists:  navlistPanel()
    ui <- fluidPage(
        navlistPanel(
            id = "tabset",
            "Heading 1",
            tabPanel("panel 1", "Panel one contents"),
            "Heading 2",
            tabPanel("panel 2", "Panel two contents"),
            tabPanel("panel 3", "Panel three contents")
        )
    )
    
    # Navigation bar:   navbarPage()   <- at top, across page
    ui <- navbarPage(
        "Page title",   
        tabPanel("panel 1", "one"),
        tabPanel("panel 2", "two"),
        tabPanel("panel 3", "three"),
        navbarMenu("subpanels", 
                   tabPanel("panel 4a", "four-a"),
                   tabPanel("panel 4b", "four-b"),
                   tabPanel("panel 4c", "four-c")
        )
    )
}

# Chapter 6.4 - 6.6
if (code_part == 3) {

    ui <- fluidPage(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        sidebarLayout(
            sidebarPanel(
                textInput("txt", "Text input:", "text here"),
                sliderInput("slider", "Slider input:", 1, 100, 30)
            ),
            mainPanel(
                h1(paste0("Theme: darkly")),
                h2("Header 2"),
                p("Some text")
            )
        )
    )    
    
    # ... or create your own theme ...
    # 
    theme <- bslib::bs_theme(
    bg = "#0b3d91", 
    fg = "white", 
    base_font = "Source Sans Pro"
    )
    ui <- fluidPage(
        theme = theme,
        sidebarLayout(
            sidebarPanel(
                textInput("txt", "Text input:", "text here"),
                sliderInput("slider", "Slider input:", 1, 100, 30)
            ),
            mainPanel(
                h1(paste0("Theme: darkly")),
                h2("Header 2"),
                p("Some text")
            )
        )
    )    
    
    # to quickly preview the theme:
    # 
    bslib::bs_theme_preview(theme)
    # > bslib::bs_theme_preview
    # function (theme = bs_theme(), ..., with_themer = TRUE) 
    # {
    #     assert_bs_theme(theme)
    #     old_theme <- bs_global_get()
    #     on.exit(bs_global_set(old_theme), add = TRUE)
    #     bs_global_set(theme)
    #     app <- system_file("themer-demo", package = "bslib")
    #     if (with_themer) {
    #         run_with_themer(app, ...)
    #     }
    #     else {
    #         shiny::runApp(app, ...)
    #     }
    # }

    server <- function(input, output, session) {
        output$hist <- renderPlot({
            means <- replicate(1e4, mean(runif(input$m)))
            hist(means, breaks = 20)
        }, res = 96)
    }

}

# Run the application 
shinyApp(ui = ui, server = server)
