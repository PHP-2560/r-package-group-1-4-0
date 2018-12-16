#install.packages("shiny")

library(shiny)
library(ggplot2)
library(dplyr)
library(tools)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("PHP2560/1560 Shiny App"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      h3("Plotting"),      # Third level header: Plotting
      
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
                  selected = "School_Type"),
      
      # Enter text for plot title
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text for plot title"),
      
      h3("Selecting"),
      
      # Select which types of school
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select school type:",
                         choices = c("Public", "Private", "Proprietary"),
                         selected = "Private"
    ),
    
    # Outputs
    mainPanel(
      
      h3("Scatterplot"),
      plotOutput(outputId = "scatterplot"),
      br(),   
      plotOutput(outputId = "densityplot", height = 200),
      verbatimTextOutput(outputId = "lmoutput") # regression output
    )
  )
)
)
# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types
  schools_subset <- reactive({
    req(input$selected_type)
    filter(df, School_Type %in% input$selected_type)
  })
  # Convert plot_title toTitleCase
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = schools_subset(), 
           aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point() +
      labs(title = pretty_plot_title())
  })  
  
  
  # Create the scatterplot object the plotOutput function is expecting
 # output$scatterplot <- renderPlot({
  #  ggplot(data = df, aes_string(x = input$x, y = input$y,
   #                                  color = input$z)) +
    #  geom_point()
    
    # Create descriptive text
    output$description <- renderText({
      paste0("The plot above titled '", pretty_plot_title(), "' visualizes the relationship between ", 
             input$x, " and ", input$y, ", conditional on ", input$z, ".")
  
  
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

