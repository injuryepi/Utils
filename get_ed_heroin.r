# get heroin api

get_ed_heroin <- function(username, password, start_date, end_date, version = 4, site_no = 934, user_id = 515) {
  require(httr, quietly = T)
  require(jsonlite, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  
  
  url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=4Jul2018&ccddCategory=cdc%20heroin%20overdose%20v4&percentParam=noPercent&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate=4Jul2018&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=000&site=000&hospFacilityType=emergency%20care&aqtTarget=DataDetails"
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  
  url <- gsub("(endDate=)\\w+", glue("\\1{end_date}"), url)
  url <- gsub("(startDate=)\\w+",glue("\\1{start_date}"), url)
  
  url <- gsub("(%20v)\\d+", glue("\\1{version}"), url)
  url <- gsub("(&site=)\\d+", glue("\\1{site_no}"), url)
  url <- gsub("(&userId=)\\d+", glue("\\1{user_id}"), url)
  
  set_config(config(ssl_verifypeer = 0L))
  resp <- httr::GET(url = url, authenticate(username , password ))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  flatten_df(parsed)
}
