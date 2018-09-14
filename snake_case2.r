snake_case2 <- purrr::compose(tolower, 
function(x) gsub("(?<=(\\w))([A-Z])", "_\\2", x, perl = TRUE),
function(x) gsub("(?>([A-Z]))([A-Z]+)", "\\1\\L\\2", x, perl = TRUE),
function(x) gsub("\\s+", "", x, perl = TRUE))
