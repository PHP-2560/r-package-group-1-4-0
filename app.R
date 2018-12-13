library(shiny)



#Shiny app College and Universities Project



ui <- fluidPage(
  titlePanel("Group 1.0 v.13 Shiny App Project: United States Colleges and Universities"),
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
      "This is where all the OUTPUT and Beautiful Interactice Visuals will go"
    )
  )
)

server <- function(input, output) {
  
  output$myplot <- renderPlot({
    plot(rnorm(input$num))
  }) 
  
}


shinyApp(ui = ui, server = server)

