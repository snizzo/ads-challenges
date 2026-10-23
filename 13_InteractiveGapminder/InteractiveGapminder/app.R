#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library("gapminder")
library(tidyverse)

continents <- sort(as.character(unique(gapminder$continent)))

vars <- c("Life expectancy" = "lifeExp",
          "Population" = "pop",
          "GDP per capita" = "gdpPercap")
label_of <- function(v) names(vars)[vars == v]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny Gapminder"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            sliderInput("s_year",
                        "Year:",
                        min = 1952,
                        max = 2007,
                        value = 1952,
                        step = 5),
            
            selectInput(
              "s_continent",
              "Continents",
              choices = c(setNames(continents, continents), "All" = "all"),
              selected = c("All" = "all"),
              multiple = FALSE
            ),
            
            radioButtons( 
              inputId = "x_value", 
              label = "X", 
              choices = vars,
              selected = "gdpPercap"
            ),
            
            radioButtons( 
              inputId = "y_value", 
              label = "Y", 
              choices = vars,
              selected = "lifeExp"
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$plot <- renderPlot({
      p <- gapminder |>
        filter(year == input$s_year) |>
        filter("all" %in% input$s_continent | continent %in% input$s_continent) |>
        ggplot(aes(x = .data[[input$x_value]],
                   y = .data[[input$y_value]],
                   size = pop,
                   alpha = pop,
                   colour = continent
                   ) ) +
        geom_point() +
        scale_alpha(range = c(0, 0.7), transform = "log10") +
        scale_size(range = c(2, 20)) +
        scale_colour_manual(values = c(
          Asia = "darkorchid4",
          Africa = "darkorange3",
          Europe = "darkgreen",
          Americas = "darkred",
          Oceania = "darkblue"
        )) +
        labs(x = label_of(input$x_value), y = label_of(input$y_value)) +
        theme_minimal()+
        theme(legend.position="none")
      
      if (input$x_value != "lifeExp") p <- p + scale_x_log10()
      if (input$y_value != "lifeExp") p <- p + scale_y_log10()
      p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
