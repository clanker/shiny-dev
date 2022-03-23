library(shiny)
library(ggplot2)

# Chapter 7
code_part <- 6

if (code_part == 1) {
  # about "click"
  
  ui <- fluidPage(
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("info"),
    tableOutput("data")
  )
  
  server <- function(input, output) {
    output$plot <- renderPlot({
      plot(mtcars$wt, mtcars$mpg)
    }, res = 96)
    
    output$info <- renderPrint({
      #req(input$plot_click)
      x <- round(input$plot_click$x, 2)
      y <- round(input$plot_click$y, 2)
      cat("[", x, ", ", y, "]", sep = "")
    })
    
    output$data <- renderTable({
      nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
    })
  }
}

if (code_part == 2) {
  # using ggplot makes things easier
  
  ui <- fluidPage(
    plotOutput("plot", 
#               click = "plot_click"
#               dblclick = "plot_click"
               hover = hoverOpts("plot_click", delay = 1)
               ),
    verbatimTextOutput("info"),
    tableOutput("data")
  )
  
  server <- function(input, output) {
    output$plot <- renderPlot({
      ggplot(mtcars, aes(wt, mpg)) +
        geom_point()
    }, res = 96)
    
    output$info <- renderPrint({
      req(input$plot_click)
      x <- round(input$plot_click$x, 2)
      y <- round(input$plot_click$y, 2)
      cat("[", x, ", ", y, "]", sep = "")
    })
    
    output$data <- renderTable({
      req(input$plot_click)
      #browser()
      nearPoints(mtcars, input$plot_click)
                 #allRows = TRUE, addDist = TRUE)
    })
  }
}

if (code_part == 3) {
  # about "brush"
  
  ui <- fluidPage(
    plotOutput("plot", 
               # click = "plot_click"
               # dblclick = "plot_click"
               hover = hoverOpts("plot_hover", delay = 10),
               brush = brushOpts("plot_brush", delay = 50,
                                 direction = dir_val())
    ),
    radioButtons("dir", "Restrict brushing to a single dimension",
                 c("x", "y", "both")),
    verbatimTextOutput("info"),
    tableOutput("data"),
  )
  
  server <- function(input, output) {
    output$dir_val <- reactiveVal("xy")
    
    observeEvent(input$dir, {
      if (input$dir == "x")  output$dir_val("x")
      if (input$dir == "y")  output$dir_val("x")
      if (input$dir == "both")  output$dir_val("xy")
    })
    
    output$plot <- renderPlot({
      ggplot(mtcars, aes(wt, mpg)) +
        geom_point()
    }, res = 96)
    
    output$info <- renderPrint({
      req(input$plot_hover)
      x <- round(input$plot_hover$x, 2)
      y <- round(input$plot_hover$y, 2)
      cat("[", x, ", ", y, "]", sep = "")
    })
    
    output$data <- renderTable({
      req(input$plot_brush)
      #browser()
      brushedPoints(mtcars, input$plot_brush)
      #allRows = TRUE, addDist = TRUE)
    })
  }
}

if (code_part == 4) {
  set.seed(1014)
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  
  ui <- fluidPage(
    plotOutput("plot", click = "plot_click", )
  )
  
  server <- function(input, output, session) {
    dist <- reactiveVal(rep(1, nrow(df)))
    
    # an observeEvent will run calculations when event occurs
    observeEvent(input$plot_click,
                 dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
    )
    
    output$plot <- renderPlot({
      df$dist <- dist() |> sqrt()
      ggplot(df, aes(x, y, size = dist)) + 
        geom_point() + 
        scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)
    }, res = 96)
  }
}

if (code_part == 5) {
  ui <- fluidPage(
    sliderInput("height", "height", min = 100, max = 500, value = 250),
    sliderInput("width", "width", min = 100, max = 500, value = 250),
    plotOutput("plot", width = 200, height = 200)
  )
  server <- function(input, output, session) {
    output$plot <- renderPlot(
      width = function() input$width,
      height = function() input$height,
      res = 96,
      {
        #browser()
        plot(rnorm(20), rnorm(20))
      }
    )
  }
}


if (code_part == 6) {
  puppies <- tibble::tribble(
    ~breed, ~ id, ~author, 
    "corgi", "eoqnr8ikwFE","alvannee",
    "labrador", "KCdYn0xu2fU", "shaneguymon",
    "spaniel", "TzjMd7i5WQI", "_redo_"
  )
  
  ui <- fluidPage(
    selectInput("id", "Pick a breed", choices = setNames(puppies$id, puppies$breed)),
    htmlOutput("source"),
    imageOutput("photo")
  )
  server <- function(input, output, session) {
    output$photo <- renderImage({
      list(
        src = file.path("puppy-photos", paste0(input$id, ".jpg")),
        contentType = "image/jpeg",
        width = 500,
        height = 650
      )
    }, deleteFile = FALSE)
    
    output$source <- renderUI({
      info <- puppies[puppies$id == input$id, , drop = FALSE]
      HTML(glue::glue("<p>
      <a href='https://unsplash.com/photos/{info$id}'>original</a> by
      <a href='https://unsplash.com/@{info$author}'>{info$author}</a>
    </p>"))
    })
  }
}
# df <- data.frame(x = rep(0, 4))
# ui <- fluidPage(tableOutput("disp"))
# server <- function(input, output) {
#   temp <- reactiveVal(c(1, 4, 9, 16))
#   output$disp <- renderTable({
#     df$y <- sqrt(temp())
#     df
#   })   
# }

# Run the application 
shinyApp(ui, server)
