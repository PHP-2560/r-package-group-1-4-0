library(shiny)

ui <- fluidPage(
  titlePanel("Group 1.0 version WEEK12"),
  sidebarLayout(
    sidebarPanel( br(), br(), br(),
      "This is where are the INPUTS will be" ,
      br(), br(), 
      sliderInput( 
        "num", "Choose a number",
        min = 0, max = 100,
        value = 20),
      plotOutput("myplot")
      ),
    mainPanel(br(), br(), br(),
      "This is where all the OUTPUT and Beautiful Visuals will go"
    )
  )
)

server <- function(input, output) {
  
  output$myplot <- renderPlot({
    plot(rnorm(input$num))
  }) 
  
}


shinyApp(ui = ui, server = server)

