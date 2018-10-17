library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(lme4)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(rlang)
library(lme4)
library(scales)
library(ggplot2)
library(grDevices)
library(bit)

#setwd("C:/Users/ethompson/Desktop/baseball/team_strength")

model_coef_comparable <- read.csv("game_logs/model_app.csv", stringsAsFactors = TRUE)
nms <- names(model_app)

ui <- fluidPage(
  
  headerPanel("MLB's Modern Era Explorer"),
  sidebarPanel(
    selectInput('x', 'X', choices = nms, selected = "year"),
    selectInput('y', 'Y', choices = nms, selected = "team"),
    selectInput('color', 'Color', choices = nms, selected = "overall_score"),
    #checkboxInput("team", "Team", choices = model_coef_comparable$team),
    
    #selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "team"),
    #selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 250, max = 1500, value = 500)
  ),
  mainPanel(
    
    h5("Negative binomial hierarchical regression model on 1961-2017 MLB data:"),
    h5("Runs/Game ~ Offense + Defense + Homefield Advantage + Stadium Effect"),
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  dataset <- reactive({
    model_coef_comparable %>%
      filter(input$color == input$color) %>%
      select(input$x, input$y, input$color)
  })
  
  output$trendPlot <- renderPlotly({
    
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point() + scale_color_gradient(low="red", high="green")
    
    #if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)



