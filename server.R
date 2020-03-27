# Code borrowed from: https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d
# Data from:
# World: Johns Hopkins University Center for System Science and Engineering (JHU CCSE)
# India: https://www.kaggle.com/sudalairajkumar/covid19-in-india#covid_19_india.csv

library(dplyr)
library(tidyr)
f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

# Add the server logic ----------------------------------------------------
function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(Country == input$country)
    if(input$state != "<all>") {
      d = d %>% 
        filter(State == input$state) 
    } else {
      d = d %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm=TRUE)
    }
    
    d %>%
      mutate(
        dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  observeEvent(input$country, {
    states = allData %>%
      filter(Country == input$country) %>% 
      pull(State)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, selected=states[1])
  })
  
  countries = sort(unique(allData$Country))
  
  updateSelectInput(session, "country", choices=countries, selected="India")
  
  renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    renderPlotly({
      data = data()
      plt = data %>% 
        plot_ly() %>%
        config(displayModeBar=FALSE) %>%
        layout(
          barmode='group', 
          xaxis=list(
            title="", tickangle=-90, type='category', ticktext=as.list(data$dateStr), 
            tickvals=as.list(data$date), gridwidth=1), 
          yaxis=list(
            title=yaxisTitle
          ),
          legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)'),
          font=f1
        )
      for(metric in input$metrics) 
        plt = plt %>%
        add_trace(
          x= ~date, y=data[[paste0(varPrefix, metric)]], type='bar', 
          name=paste(legendPrefix, metric, "Cases"),
          marker=list(
            color=switch(metric, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
            line=list(color='rgb(8,48,107)', width=1.0)
          )
        )
      plt
    })
  }
  
  output$cumulatedMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
  output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
}
