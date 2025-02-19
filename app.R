library(shiny)
library(popbio)

# Load matrix data
matrix_data <- readRDS("allOrg25.rds")  # Assuming matrices are stored as a named list

default_matrix <- names(matrix_data)[1]  # Default to the first matrix

ui <- fluidPage(
  titlePanel("Matrix Population Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("matrix_choice", "Choose a Matrix:", choices = names(matrix_data), selected = default_matrix)
    ),
    mainPanel(
      h3("Selected Matrix"),
      tableOutput("selected_matrix"),
      h3("Lambda"),
      verbatimTextOutput("lambda"),
      h3("Stable Stage Distribution"),
      tableOutput("ssd"),
      h3("Sensitivity Matrix"),
      tableOutput("sensitivity"),
      h3("Elasticity Matrix"),
      tableOutput("elasticity"),
      h3("Population Projection"),
      plotOutput("projection_plot")
    )
  )
)

server <- function(input, output, session) {
  selected_matrix <- reactive({
    req(input$matrix_choice)
    matrix_data[[input$matrix_choice]]
  })
  
  output$selected_matrix <- renderTable({
    selected_matrix()
  }, rownames = TRUE)
  
  output$lambda <- renderPrint({
    lambda(selected_matrix())
  })
  
  output$ssd <- renderTable({
    stable.stage(selected_matrix())
  }, rownames = TRUE)
  
  output$sensitivity <- renderTable({
    sensitivity(selected_matrix())
  }, rownames = TRUE)
  
  output$elasticity <- renderTable({
    elasticity(selected_matrix())
  }, rownames = TRUE)
  
  output$projection_plot <- renderPlot({
    projection <- pop.projection(selected_matrix(), c(2,2,2), 20)
    matplot(t(projection$stage.vectors), type = "l", lty = 1, col = 1:3,
            xlab = "Time Steps", ylab = "Population Size", main = "Population Projection")
    legend("topright", legend = paste("Stage", 1:3), col = 1:3, lty = 1)
  })
}

shinyApp(ui, server)


