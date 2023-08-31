library(shiny)

df_hdi <- read.csv('data/hdi.csv')

min_year <- min(df_hdi$year)
max_year <- max(df_hdi$year)
indicators <- c(
    'University Score' = 'uni_score',
    'Human Development Index' = 'hdi',
    'Life Expectancy at Birth' = 'le',
    'Expected Years of Schooling' = 'eys',
    'Mean Years of Schooling' = 'mys',
    'Gross National Income Per Capita' = 'gnipc'
)

# UI
ui <- fluidPage(
    titlePanel("University Scores and Human Development Index"),
    sidebarLayout(
        sidebarPanel(
            selectInput("xAxis", 'x-Axis', indicators, selected = 'hdi'),
            selectInput("yAxis", 'y-Axis', indicators, selected = 'uni_score')
        ),
        mainPanel(
            plotOutput("plot"),
            sliderInput(
                "year", label = "Year",
                min = min_year, value = min_year, max = max_year, 
                step = 1, ticks = FALSE, sep = ""
            ),
            textOutput("xOutput")
        )
    )
)

## Set up back-end logic (server)
server <- function(input, output, session) {
  ## The following would work
  output$xOutput <- renderText({
    paste("x-Axis selected:", input$xAxis)
  })
}

## Put both together. You can also click on RStudio's "Run App" button.
shinyApp(ui, server)
