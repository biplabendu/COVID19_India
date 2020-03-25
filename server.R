# Code borrowed from: https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d
# Data from:
# World: Johns Hopkins University Center for System Science and Engineering (JHU CCSE)
# India: https://www.kaggle.com/sudalairajkumar/covid19-in-india#covid_19_india.csv

library(dplyr)
library(tidyr)  

# Read data from JHU ------------------------------------------------------

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")


# Run it for every update cycle [1] -------------------------------------------

# minutesSinceLastUpdate = function(fileName) {
#   (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
# }
# 
# loadData = function(fileName, columnName) {
#   if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
#     data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
#       select(-Lat, -Long) %>%
#       pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
#       mutate(
#         date=as.Date(date, format="%m/%d/%y"),
#         `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
#         `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
#       )
#     save(data, file=fileName)
#   } else {
#     load(file=fileName)
#   }
#   return(data)
# }

loadData = function(fileName, columnName) {
    load(file=fileName)
    return(data)
}

allData =
  loadData("time_series_19-covid-Confirmed.csv", "CumConfirmed") %>%
  inner_join(loadData("time_series_19-covid-Deaths.csv", "CumDeaths")) %>%
  inner_join(loadData("time_series_19-covid-Recovered.csv", "CumRecovered"))


# names(allData)[1] <- "State_or_Province"
# names(allData)[2] <- "Country"
# 
# allData %>% glimpse()

# Read dataset from Github ------------------------------------------------

# ## Data available until
# current.date <- "25/03/20"
# 
# # Data downloaded from: https://www.kaggle.com/sudalairajkumar/covid19-in-india/version/7
# # Download timestamp: 0225 hrs, 20th March 2020
# urlfile <- "https://github.com/biplabendu/homepage/raw/master/covid19_data_india/covid_19_india_24Mar20.csv"
# 
# india <- read.csv(url(urlfile),
#                      header = T, stringsAsFactors = F)
# save(india, file="india_data.csv")
# load(file="india_data.csv")
# 
# # india %>% glimpse()
# 
# 
# india$Date <- as.Date(india$Date, "%d/%m/%y")
# 
# # Incorrect data entry | Fix it
# india[india$State.UnionTerritory == "Maharashtra" & 
#         india$Date == "2020-03-11",]$ConfirmedIndianNational <- 5
# 
# ## Different names for the same state | Fix this
# # 1. Pondicherry = Puducherry
# # 2. Union Territory of Chandigarh = Chandigarh
# # 3. Chattisgarh = Chhattisgarh
# # 4. Union Territory of Jammu and Kashmir = Jammu and Kashmir
# # 5. Union Territory of Ladakh = Ladakh
# 
# india[india$State.UnionTerritory == "Pondicherry",]$State.UnionTerritory <- "Puducherry"
# india[india$State.UnionTerritory == "Union Territory of Chandigarh",]$State.UnionTerritory <- "Chandigarh"
# india[india$State.UnionTerritory == "Chattisgarh",]$State.UnionTerritory <- "Chhattisgarh"
# india[india$State.UnionTerritory == "Union Territory of Jammu and Kashmir",]$State.UnionTerritory <- "Jammu and Kashmir"
# india[india$State.UnionTerritory == "Union Territory of Ladakh",]$State.UnionTerritory <- "Ladakh"
# 
# 
# allData_india <-
#   india %>%
#   rename("State_or_Province" = State.UnionTerritory, date = Date) %>%
#   mutate("Country" = "India") %>%
#   rename(Confirmed = ConfirmedIndianNational) %>%
#   arrange(State_or_Province, date) %>% 
#   ## need to fix this - filter to keep the right copy since there are multiple entries
#   distinct(State_or_Province, Country, date, .keep_all = T) %>%   
#   rename(CumConfirmed = Confirmed,
#          CumDeaths = Deaths,
#          CumRecovered = Cured) %>%
#   select(State_or_Province, Country, date, CumConfirmed, CumRecovered, CumDeaths) %>% 
#   as_tibble()
# 
# # allData_india %>% 
# #   filter(date == "2020-03-13") %>% 
# #   View()
# # 
# # allData_india %>% 
# #   group_by(State_or_Province,date) %>% 
# #   summarise_if(is.numeric, sum, na.rm=TRUE) %>% 
# #   View()
# 
# # head(allData)
# 
# 
# # Format the data for dates 22 Jan 2020 to current --------------------------------------------------------
# 
# ## Code borrowed from: https://biostats.w.uib.no/7-building-a-dataframe-from-a-bunch-of-vectorsseries/
# indian_states <- levels(factor(allData_india$State_or_Province)) %>% as.character()
# 
# dates <- rep(seq(as.Date("22/01/20", format = "%d/%m/%y"), 
#              as.Date(current.date, format = "%d/%m/%y"), 
#              by="days"), length(indian_states))
# country = rep("India", length(dates))
# states = rep(indian_states, each=length(dates)/length(indian_states))
# confirmed = as.integer(rep(0, length(dates)))
# recovered = as.integer(rep(0, length(dates)))
# deaths = as.integer(rep(0, length(dates)))
# 
# df <- data.frame(State_or_Province = states, 
#               Country = country,
#               date = dates,
#               CumConfirmed = confirmed,
#               CumRecovered = recovered,
#               CumDeaths = deaths,
#               stringsAsFactors = F)
# 
# df <- df %>%
#   select(State_or_Province:date) %>% 
#   left_join(allData_india, by = c("State_or_Province", "Country", "date"))
#   
# df[is.na(df)] <- 0  
# 
# df <- df %>% 
#   filter(!(State_or_Province == ""))
# 
# ## Update the github curated file here -------
# 
# df.curated <- df
# names(df.curated) <- names(allData)
# write.csv(df, file="./curated_data_India/india_curated_data_BD.csv")


# Let's fill in the data for each state, for all the dates until available date

india.df <- read.csv("./curated_data_India/india_curated_data_BD.csv", stringsAsFactors = F, header = T)
india.df <- india.df[-1]
names(india.df) <- names(allData)

allData <-  rbind(allData %>% filter(!(`Country/Region` == "India")), india.df)


# allData %>% 
#   filter(date %in% allData_india$date) %>% 
#   summary()


# Add the server logic ----------------------------------------------------
function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
    if(input$state != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state) 
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
      filter(`Country/Region` == input$country) %>% 
      pull(`Province/State`)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, selected=states[1])
  })
  
  countries = sort(unique(allData$`Country/Region`))
  
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
