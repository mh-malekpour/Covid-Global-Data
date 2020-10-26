
plotTimeSeries <- function(Country, startDate, endDate){

  library(scales)
  library(patchwork)
  library(ggpmisc)
  library(dplyr)

  Country = 'US'
  startDate = '2020-01-22'
  endDate = '2020-06-30'
  startDate = as.POSIXct(startDate)
  endDate = as.POSIXct(endDate)
  covidData <- CovidGlobalData::getData()

  covidData %>%
    filter(country == Country & lable != 'recoverd') %>%
    subset(date >= startDate & date <= endDate) -> data

  p <- ggplot(data, aes(x = date, y = count)) +
    geom_line(aes(color = lable), size = 1) +
    scale_color_manual(values = c("#00AFBB",  "#bf1313", "#E7B800")) +
    scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
    scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
    theme_minimal() +
    labs(
      x = 'Date',
      y = 'Count',
      title = 'COVID-19',
      color = 'Case')

  return(p)
}
