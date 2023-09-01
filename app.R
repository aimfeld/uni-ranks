library(shiny)
library(plotly)
library(dplyr)

df_hdi <- read.csv('data/hdi.csv')

# Round all numeric columns to 2 digits
df_hdi <- df_hdi %>% mutate(across(is.numeric, round, digits=2))

kpis <- c(
    'University Score' = 'uni_score',
    'Human Development Index' = 'hdi',
    'Life Expectancy at Birth (years)' = 'le',
    'Expected Years of Schooling (years)' = 'eys',
    'Mean Years of Schooling (years)' = 'mys',
    'Gross National Income Per Capita (2017 PPP$)' = 'gnipc'
)
kpi_labels = setNames(names(kpis), kpis)

# Calculate limits
min_year <- min(df_hdi$year)
max_year <- max(df_hdi$year)
kpi_limits <- ceiling(apply(df_hdi[, kpis], 2, function(x) max(x, na.rm = TRUE)))

# UI
ui <- fluidPage(
    titlePanel("University Scores and Human Development Index"),
    sidebarLayout(
        sidebarPanel(
            selectInput("xAxis", 'x-Axis', kpis, selected = 'hdi'),
            selectInput("yAxis", 'y-Axis', kpis, selected = 'uni_score')
        ),
        mainPanel(
            plotlyOutput("plot"),
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
    output$plot <- renderPlotly({
        df <- selectedData()
        plot_ly(
            df,
            x = ~get(input$xAxis),
            y = ~get(input$yAxis), 
            type = 'scatter', 
            mode = 'markers',
            hovertemplate = paste(
                '<b>', kpi_labels[input$yAxis], '</b>: %{y}<br>',
                '<b>', kpi_labels[input$xAxis], '</b>: %{x}',
                '<extra></extra>'
            )
        ) %>% layout(
            showlegend = FALSE, 
            xaxis = list(
                title = kpi_labels[input$xAxis],
                range = c(0, kpi_limits[input$xAxis])
            ),
            yaxis = list(
                title = kpi_labels[input$yAxis],
                range = c(0, kpi_limits[input$yAxis])
            )
        )
    })
}

## Put both together. You can also click on RStudio's "Run App" button.
shinyApp(ui, server)
