#install.packages("shiny")

library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Endowment", "Median_Start_Sal", "Acc_Rate", "Score", "Tuition"), 
                  selected = "Score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Endowment", "Median_Start_Sal", "Acc_Rate", "Score", "Tuition"), 
                  selected = "Acc_Rate"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("School_Type", "Religion"),
                  selected = "School_Type")
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200),
      verbatimTextOutput(outputId = "lmoutput") # regression output
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = df, aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      geom_point()
    
  
  })
  #creating density plot
  output$densityplot <- renderPlot({
    ggplot(data = df, aes_string(x = input$x)) +
      geom_density()
  })
  
  
  # Create regression output
  output$lmoutput <- renderPrint({
    x <- df %>% pull(input$x)
    y <- df %>% pull(input$y)
    summ <- summary(lm(y ~ x, data = df)) 
    print(summ, digits = 3, signif.stars = FALSE)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

