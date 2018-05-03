snake_case <- purrr::compose(tolower, function(x) gsub("(?<=(\\w))([A-Z])", "_\\2", x, perl = TRUE))
