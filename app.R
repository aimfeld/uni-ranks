library(shiny)

df_hdi <- read.csv('data/hdi.csv')

min_year <- min(df_hdi$year)
max_year <- max(df_hdi$year)

indicators <- c(
    'uni_score' = 'University Score',
    'hdi' = 'Human Development Index',
    'le' = 'Life Expectancy at Birth',
    'eys' = 'Expected Years of Schooling',
    'mys' = 'Mean Years of Schooling',
    'gnipc' = 'Gross National Income Per Capita'
)
# Flip values and names for input choices
indicators_input <- setNames(names(indicators), indicators)

# UI
ui <- fluidPage(
    titlePanel("University Scores and Human Development Index"),
    sidebarLayout(
        sidebarPanel(
            selectInput("xAxis", 'x-Axis', indicators_input, selected = 'hdi'),
            selectInput("yAxis", 'y-Axis', indicators_input, selected = 'uni_score')
        ),
        mainPanel(
            plotOutput("plot"),
            sliderInput(
                "year", label = "Year",
                min = min_year, value = min_year, max = max_year, 
                step = 1, ticks = FALSE, sep = ""
            )
        )
    )
)

## Server
server <- function(input, output, session) {
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        df_hdi[df_hdi$year == input$year, c(input$xAxis, input$yAxis)]
    })
    # Plot
    output$plot <- renderPlot({
        plot(selectedData(), 
             xlab = indicators[input$xAxis],
             ylab = indicators[input$yAxis])
    })
}

## Put both together. You can also click on RStudio's "Run App" button.
shinyApp(ui, server)
