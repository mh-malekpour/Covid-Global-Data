getData <- function(){

  confirmedURL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  deathsURL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  recoveredURL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

  confirmed <- as_tibble(read_csv(confirmedURL, col_types = cols()) %>%
                           select(-'Province/State', -'Lat', -'Long') %>%
                           rename("country" = "Country/Region")%>%
                           mutate(country = as.factor(country)))

  recoverd <- as_tibble(read_csv(confirmedURL, col_types = cols()) %>%
                          select(-'Province/State', -'Lat', -'Long') %>%
                          rename("country" = "Country/Region")%>%
                          mutate(country = as.factor(country)))

  deaths <- as_tibble(read_csv(confirmedURL, col_types = cols()) %>%
                        select(-'Province/State', -'Lat', -'Long') %>%
                        rename("country" = "Country/Region")%>%
                        mutate(country = as.factor(country)))

  confirmed %>%
    gather(key = "date", value = "count", 2:ncol(confirmed)) %>%
    mutate(lable = "confirmed",
           count = c(count[1] , diff(count)),
           date=as.Date(date, format = "%m/%d/%y")) %>%
    select(country, date, lable, count) -> confirmed

  recoverd %>%
    gather(key = "date", value = "count", 2:ncol(confirmed)) %>%
    mutate(lable = "recoverd" ,
           count = c(count[1] , diff(count)),
           date=as.Date(date, format = "%m/%d/%y")) %>%
    select(country, date, lable, count) -> recoverd

  deaths %>%
    gather(key = "date", value = "count", 2:ncol(confirmed)) %>%
    mutate(lable = "death",
           count = c(count[1] , diff(count)),
           date=as.Date(date, format = "%m/%d/%y")) %>%
    select(country, date, lable, count) -> deaths

  covidData <- bind_rows(confirmed, recoverd, deaths) %>%
    mutate(date=as.Date(date, format = "%m/%d/%y"))

  return(covidData)
}
