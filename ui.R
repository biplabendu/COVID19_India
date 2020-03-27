# Code borrowed from: https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d
library(shiny)
library(plotly)
library(shinythemes)

shinyUI(fluidPage(
  theme=shinytheme("slate"),
  tags$style(
    type='text/css', 
    ".selectize-input { font-family: Courier New, monospace; } .selectize-dropdown { font-family: Courier New, monospace; }"
  ),
  tags$style(HTML(
    "body { font-family: Courier New, monospace; line-height: 1.1; }"
  )),
  
  titlePanel("Tracking Coronavirus (COVID-19) in India*"),
  sidebarLayout(
    sidebarPanel("*Indian nationals only",
                 helpText(paste0("Last update: ", datetime))),
    mainPanel(
      em(paste0("Note: Data curated from Ministry of Health and Family Welfare [India] and JHU Github repository [global]. Please report data errors at biplabendu.knights@gmail.com")))
    ),
  fluidRow(
    column(
      4, 
      selectizeInput("country", label=h5("Country"), choices=NULL, width="100%")
    ),
    column(
      4, 
      selectizeInput("state", label=h5("State / Province"), choices=NULL, width="100%")
    ),
    column(
      4, 
      checkboxGroupInput(
        "metrics", label=h5("Selected Metrics"), 
        choices=c("Confirmed", "Deaths", "Recovered"), 
        selected=c("Confirmed", "Deaths", "Recovered"), width="100%")
    )
  ),
  fluidRow(
    plotlyOutput("cumulatedMetrics")
  ),
  fluidRow(
    plotlyOutput("dailyMetrics")
  )
))
