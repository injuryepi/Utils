# get ED time series

get_ed_ts <- function(username, password, start_date, end_date, site_no, user_id) {
  
  
  require(httr, quietly = T)
  require(jsonlite, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  require(dplyr, quietly = T)

url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=30Sep2018&percentParam=noPercent&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate=1Jan2010&timeResolution=monthly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=000&site=000&hospFacilityType=emergency%20care&aqtTarget=TimeSeries"

start_date = format(as.Date(start_date) , "%d%b%Y")
end_date = format(as.Date(end_date) , "%d%b%Y")

url <- gsub("(endDate=)\\w+", glue("\\1{end_date}"), url)
url <- gsub("(startDate=)\\w+",glue("\\1{start_date}"), url)
url <- gsub("(&site=)\\d+", glue("\\1{site_no}"), url)
url <- gsub("(&userId=)\\d+", glue("\\1{user_id}"), url)

set_config(config(ssl_verifypeer = 0L))
resp <- httr::GET(url = url, authenticate(username , password ))

if (http_type(resp) != "application/json") {
  stop("API did not return json", call. = FALSE)
}

parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

transpose(parsed$timeSeriesData) %>% 
  map_df(unlist)

}
