# Code borrowed from: https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d
# Data from:
# World: Johns Hopkins University Center for System Science and Engineering (JHU CCSE)
# India: https://www.kaggle.com/sudalairajkumar/covid19-in-india#covid_19_india.csv

library(dplyr)
library(tidyr)
f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

# Read the curated dataset from my GitHub account

urlfile <- "https://github.com/biplabendu/COVID19_India/raw/master/curated_data/curated_data_BD.csv"


daysSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime))/60/60/24
}

loadData = function(fileName) {
  if(!file.exists(fileName) || daysSinceLastUpdate(fileName) > 0.5) {
    data = read.table(url(urlfile), sep = ",",
                    header = T, stringsAsFactors=FALSE) %>% 
      mutate(date=as.Date(date))
      # select(-Lat, -Long) %>%
      # pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
      # mutate(
      #   date=as.Date(date, format="%m/%d/%y"),
      #   `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
      #   `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`))
      
    save(data, file=fileName)
  } else {
    load(file=fileName)
  }
  return(data)
}

# allData =
#   loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
#   inner_join(loadData("time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
#   inner_join(loadData("time_series_covid19_recovered_global.csv", "CumRecovered"))
# 
# df.curated.global <- allData
# 

df <- loadData("curated_data_BD.csv")

# df <- read.csv(url(urlfile),
#                   header = T, stringsAsFactors = F)

df <- df[-1]
names(df)[1] <- "State"
names(df)[2] <- "Country"

allData <- df

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
