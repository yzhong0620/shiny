library(tidyverse)
library(shiny)
library(bslib)

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

catties <- nominees %>% 
  distinct(category) %>% 
  arrange(category) %>% 
  pull(category)

ui <- fluidPage(
  theme = bs_theme(primary = "#ADD8E6", 
                   secondary = "#FFEBCD", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "spacelab"),
  
  # Application title
  titlePanel("Update the distributor within a category"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cat", # to use in code
                  label = "Category:", # how it looks in UI
                  choices = catties, 
                  selected = "Outstanding Character Voice-Over Performance - 2021"
      ),
      selectInput(inputId = "distributor", # to use in code
                  label = "Distributor:", # how it looks in UI
                  multiple = TRUE, # can choose more than one
                  choices = NULL
      )
      
    ),
    # Show a barplot of chosen distributors within chosen category
    mainPanel(
      plotOutput(outputId = "total_count")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  category <- reactive({
    nominees %>% 
      filter(category == input$cat)
  })
  
  observeEvent(category(), {
    choices <- category() %>% 
      distinct(distributor) %>% 
      arrange(distributor) %>% 
      pull(distributor)
    updateSelectInput(inputId = "distributor", choices = choices) 
  })
  
  cat_distributor <- reactive({
    category() %>% 
      filter(distributor %in% input$distributor)
  })
  
  output$total_count <- renderPlot({
    req(input$distributor)
    cat_distributor() %>% 
      group_by(distributor) %>% 
      summarize(tot_count = n(),
                winner = ifelse(sum(type == "Winner") == 0, "No", "Yes")) %>% 
      ggplot(aes(y = fct_reorder(distributor, tot_count),
                 x = tot_count,
                 fill = winner)) +
      geom_col() +
      labs(title = paste("Total count of nominees under", input$cat, "for each distributor"),
           x = "",
           y = "")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)