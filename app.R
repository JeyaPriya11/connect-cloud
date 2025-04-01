library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Team Allocation Plan"),
  sidebarLayout(
    sidebarPanel(
      numericInput("working_days", "Number of Working Days:", value = 20, min = 1),
      selectInput("project", "Select Project:", choices = c("Project R", "Internal", "Project P","Internal-FG/Training)),
      selectInput("team_member", "Select Team Member:", choices = c("Dom", "Muku", "Shara","Ayya","San","jeps")),
      numericInput("allocation_percentage", "Allocation Percentage:", value = 50, min = 0, max = 100),
      selectInput("allocation_type", "Allocation Type:", choices = c("Billable", "Non-Billable")),
      actionButton("allocate", "Allocate"),
      hr(),
      downloadButton("download_data", "Download Allocation Data")
    ),
    mainPanel(
      h4("Team Allocation Table"),
      tableOutput("allocation_table"),
      hr(),
      h4("Team Member Availability"),
      tableOutput("availability_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize allocation data frame
  allocation <- reactiveVal(data.frame(
    Project = character(), 
    TeamMember = character(), 
    Percentage = numeric(), 
    Hours = numeric(), 
    AllocationType = character(),
    stringsAsFactors = FALSE
  ))
  
  # Initialize team members and their total available hours
  team_members <- c("Alice", "Bob", "Charlie")
  
  # Calculate total hours available per person
  total_hours_per_person <- reactive({
    input$working_days * 8
  })
  
  # Calculate hours already allocated per person
  allocated_hours_per_person <- reactive({
    alloc_data <- allocation()
    if(nrow(alloc_data) == 0) {
      return(setNames(rep(0, length(team_members)), team_members))
    }
    
    hours_per_person <- tapply(alloc_data$Hours, alloc_data$TeamMember, sum)
    result <- setNames(rep(0, length(team_members)), team_members)
    result[names(hours_per_person)] <- hours_per_person
    return(result)
  })
  
  # Calculate remaining hours per person
  available_hours_per_person <- reactive({
    total <- total_hours_per_person()
    allocated <- allocated_hours_per_person()
    result <- setNames(rep(total, length(team_members)), team_members)
    result <- result - allocated
    return(result)
  })
  
  # Add new allocation
  observeEvent(input$allocate, {
    # Check if selected team member has enough available hours
    team_member <- input$team_member
    available_hours <- available_hours_per_person()[team_member]
    requested_hours <- (input$allocation_percentage / 100) * total_hours_per_person()
    
    if(requested_hours > available_hours) {
      showNotification(
        paste0("Cannot allocate ", round(requested_hours, 1), " hours to ", team_member, 
               ". Only ", round(available_hours, 1), " hours available."),
        type = "error"
      )
      return()
    }
    
    # Add allocation if there are enough hours
    new_allocation <- data.frame(
      Project = input$project, 
      TeamMember = team_member, 
      Percentage = input$allocation_percentage, 
      Hours = requested_hours, 
      AllocationType = input$allocation_type,
      stringsAsFactors = FALSE
    )
    allocation(rbind(allocation(), new_allocation))
    
    # Show success message
    showNotification(
      paste0("Successfully allocated ", round(requested_hours, 1), " hours to ", team_member, 
             " for ", input$project),
      type = "message"
    )
  })
  
  # Display allocation table
  output$allocation_table <- renderTable({
    allocation()
  })
  
  # Display availability table
  output$availability_table <- renderTable({
    total <- total_hours_per_person()
    allocated <- allocated_hours_per_person()
    available <- available_hours_per_person()
    
    data.frame(
      TeamMember = team_members,
      TotalHours = rep(total, length(team_members)),
      AllocatedHours = allocated,
      AvailableHours = available,
      PercentAllocated = round((allocated / total) * 100, 1)
    )
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

