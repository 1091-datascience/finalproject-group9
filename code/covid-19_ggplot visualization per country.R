library(tidyverse)
library(plotly)
library(grid)
library(RCurl)


Main <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

confirmed_Path <-  file.path(Main,"time_series_covid19_confirmed_global.csv")
Deaths_Path <- file.path(Main,"time_series_covid19_deaths_global.csv")
Recovered_Path <- file.path(Main,"time_series_covid19_recovered_global.csv")

#Read data from stored links:
ConfirmedData <- read.csv(confirmed_Path,stringsAsFactors = FALSE)
DeathData     <- read.csv(Deaths_Path,stringsAsFactors = FALSE)
RecoveredData <- read.csv(Recovered_Path,stringsAsFactors = FALSE)

# region; prompt user?
Region <- "Taiwan*"

#Accumulate data
accum <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat) 
  lvls <- plotly:::getLevels(var) 
  dats <- lapply(seq_along(lvls), 
                 function(x) {
                   cbind.data.frame(dat[var %in% lvls[seq(1, x)], ],
                                    frame = lvls[[x]])}) 
  dplyr::bind_rows(dats)}

# Data clean/ filter by country/region
DataClean <- function(data, region, CaseType) {
  CleanedData <- data %>% 
    pivot_longer(cols = starts_with("X"), 
                 names_to = "Date", 
                 names_prefix = "X", 
                 names_ptypes = list(week = integer()), 
                 values_to = CaseType, 
                 values_drop_na = TRUE)   %>% 
    mutate(Province.State = ifelse(Province.State %in% "", Country.Region, Province.State)) %>% 
    mutate(Date = as.Date(Date, "%m.%d.%y")) %>% 
    filter(Province.State == region) %>% 
    arrange(Date) %>% 
    mutate(ID = row_number()) 
  return(CleanedData)
}

# Clean Data
ConData <- DataClean(ConfirmedData,Region,"Confirmed") 
RecData <- DataClean(RecoveredData,Region,"Recovered") %>% select(ID,Recovered) 
DeData <-DataClean(DeathData,Region,"Deaths")          %>% select(ID,Deaths)

# Merge cleaned data with ID column
AllData <- list(ConData, RecData, DeData)              %>% reduce(left_join, by = "ID")


#  vector generation (in time)
vector <- AllData %>% accum(~ID)


# use ggplot to combine frames into vector for every country in time series

plot <- ggplot(data=vector,aes(x=Date, frame = frame)) + 
  geom_line(aes(y=Confirmed, colour="Confirmed"),size=1) + 
  geom_line(aes(y= Recovered, colour = "Recovered"),size=1) + 
  geom_line(aes(y= Deaths, colour = "Deaths"),size=1) + 
  labs(colour="Cases") + 
  scale_color_manual(values=c('#0751B8','#B82607', '#0BB807')) + 
  theme_bw()+ 
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(6, 6, 6, 6)) + 
  labs(title = paste("Numeber of cases (since first report) in:",Region),
       x = "Date",
       y="Cases") 

plot
