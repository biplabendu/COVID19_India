library(dplyr)
library(tidyr)
f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

# Read the curated dataset from my GitHub account

urlfile <- "https://github.com/biplabendu/homepage/raw/master/covid19_data_india/curated_data/curated_data_BD.csv"


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

datetime <- paste0(file.info("curated_data_BD.csv")$ctime %>% as.character()," EDT")
# df <- read.csv(url(urlfile),
#                   header = T, stringsAsFactors = F)

df <- df[-1]
names(df)[1] <- "State"
names(df)[2] <- "Country"

allData <- df