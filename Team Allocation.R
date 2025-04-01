library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Team Allocation Plan"),
    sidebarLayout(
        sidebarPanel(
            numericInput("working_days", "Number of Working Days:", value = 20, min = 1),
            selectInput("project", "Select Project:", choices = c("Project A", "Project B", "Project C")),
            selectInput("team_member", "Select Team Member:", choices = c("Alice", "Bob", "Charlie")),
            numericInput("allocation_percentage", "Allocation Percentage:", value = 50, min = 0, max = 100),
            selectInput("allocation_type", "Allocation Type:", choices = c("Billable", "Non-Billable")),
            actionButton("allocate", "Allocate"),
            hr(),
            downloadButton("download_data", "Download Allocation Data")
        ),
        mainPanel(
            tableOutput("allocation_table")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    allocation <- reactiveVal(data.frame(
        Project = character(), 
        TeamMember = character(), 
        Percentage = numeric(), 
        Hours = numeric(), 
        AllocationType = character(),
        stringsAsFactors = FALSE
    ))

    observeEvent(input$allocate, {
        total_hours <- input$working_days * 8
        allocated_hours <- (input$allocation_percentage / 100) * total_hours
        new_allocation <- data.frame(
            Project = input$project, 
            TeamMember = input$team_member, 
            Percentage = input$allocation_percentage, 
            Hours = allocated_hours, 
            AllocationType = input$allocation_type,
            stringsAsFactors = FALSE
        )
        allocation(rbind(allocation(), new_allocation))
    })

    output$allocation_table <- renderTable({
        allocation()
    })
    
    # Download handler for allocation data
    output$download_data <- downloadHandler(
        filename = function() {
            paste("team_allocation_", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(allocation(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
