

library(shiny)
library(dplyr)
library(plotly)
library(DT)

# Load the data
data <- read.csv("Holocaust-Data-3.csv", stringsAsFactors = FALSE)

# Remove rows with empty Nationality.Category
data <- data %>%
  filter(Nationality.Category != "")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Murdered People by Nationality"),
  
  # Sidebar with a checkbox group for selecting nationalities
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("groups", "Select nationalities:",
                         choices = unique(data$Nationality.Category)),
      br(),
      actionButton("update", "Update")
    ),
    
    # Show a plot of the selected nationalities
    mainPanel(
      tabsetPanel(
        tabPanel("Graph", plotlyOutput("plot")),
        tabPanel("Table", DTOutput("table"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    filter(data, Nationality.Category %in% input$groups) %>%
      arrange(Number.of.victims)  # Arrange the data by Number.of.victims
  })
  
  output$plot <- renderPlotly({
    filtered_data_df <- filtered_data()
    filtered_data_df$Nationality.Category <- factor(filtered_data_df$Nationality.Category, 
                                                    levels = unique(data$Nationality.Category))
    
    p <- ggplot(filtered_data_df, aes(x = Nationality.Category, y = Number.of.victims)) +
      geom_bar(stat = "identity") +
      labs(x = "Nationality/Category", y = "Number of victims") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Set plot width and adjust x-axis labels
    p <- plotly::ggplotly(p) %>% layout(width = 1000, xaxis = list(tickangle = -45))
    
    p
  })
  
  output$table <- renderDT({
    filtered_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
