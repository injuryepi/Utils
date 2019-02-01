get_ed_county <- function(username, password, start_date, end_date, site_no, user_id, ...) {
  require(httr, quietly = T)
  require(jsonlite, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T) 
  
  url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=30Sep2018&percentParam=noPercent&geographySystem=hospitalregion&datasource=va_hosp&detector=nodetectordetector&startDate=1Sep2018&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=515&site=934&hospFacilityType=emergency%20care&aqtTarget=DataDetails"
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  
  url <- gsub("(endDate=)\\w+", glue("\\1{end_date}"), url)
  url <- gsub("(startDate=)\\w+",glue("\\1{start_date}"), url)
  url <- gsub("(&site=)\\d+", glue("\\1{site_no}"), url)
  url <- gsub("(&userId=)\\d+", glue("\\1{user_id}"), url)
  
    # selected counties
  
  f_geo <- function(url, ...){
    
    if(!length(list(...))){
      geo <- NULL
    } 
    else {
      geo <- tolower(paste0(paste0("geography=wa_", list(...), collapse = "&"), "&"))   
   }
    
    gsub("(percentParam=noPercent&)", paste0("\\1", geo), url)
    
    
  }
  
  url <- f_geo(url = url, ...)
  
  ##
  
  set_config(config(ssl_verifypeer = 0L))
  resp <- httr::GET(url = url, authenticate(username , password ))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  flatten_df(parsed)
  
}
