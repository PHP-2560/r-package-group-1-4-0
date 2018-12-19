#install.packages("shiny")
#install.packages("leaflet")
library(shiny)
library(dplyr)
library(tools)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)
library(shinythemes)

ui = navbarPage("UniversityRankings", id="nav",
                
                # tab for data explorer                  
                tabPanel("Data explorer",
                         # option to filter by public schools, private schools, or both
                         fluidRow(
                           column(3,
                                  selectInput("schools", "Schools", c("Public", "Private"), multiple=TRUE)
                           )
                         ),
                         # option to filter by a minimum and maximum score (as rated by US News)
                         # defaults to show all schools
                         fluidRow(
                           column(1,
                                  numericInput("minScore", "Min Score", value = 0, min=0, max=100)
                           ),
                           column(1,
                                  numericInput("maxScore", "Max Score", value = 100,min=0, max=101)
                           )
                         ),
                         hr(),
                         DT::dataTableOutput("df_out")
                ),
                tabPanel("Statistical Analysis",
                         theme = shinytheme("cerulean"),
                         
                         
                         # Application title
                         titlePanel("PHP2560/1560 Shiny App"),
                         
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
                                         selected = "School_Type"),
                             
                             # Enter text for plot title
                             textInput(inputId = "plot_title", 
                                       label = "Plot title", 
                                       placeholder = "Enter text to be used as plot title"),
                             
                             # Select which types of school
                             checkboxGroupInput(inputId = "selected_type",
                                                label = "Select school type:",
                                                choices = c("Public", "Private", "Proprietary"),
                                                selected = "Private")
                           ),
                           
                           # Outputs
                           mainPanel(
                             plotOutput(outputId = "scatterplot"),
                             plotOutput(outputId = "densityplot", height = 200),
                             verbatimTextOutput(outputId = "lmoutput") # regression output
                           )
                         )
                ),
                conditionalPanel("true", icon("crosshair")),
                singleton(shiny::tags$head(
                  shiny::tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
                  includeScript('voice.js')
                ))
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  # filter function based on given minimum/maximum scores & school type(s)
  change_df = function(min, max, sch_typ) {
    df %>%
      dplyr::filter(Score >= min,
                    Score <= max,
                    is.null(sch_typ) | School_Type %in% sch_typ
      )
  }
  # updates data table based on given inputs
  output$df_out = DT::renderDataTable({
    change_df(input$minScore, input$maxScore, input$schools)
  })
  
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

