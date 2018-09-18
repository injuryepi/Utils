snake_case2 <- purrr::compose(tolower, 
# remove repeat "_" and extreme "_"
function(x) gsub("(_)(?=_*\\1)|^_|_$", "", x, perl = T),
function(x) gsub("(?<=(\\w))([A-Z])", "_\\2", x, perl = TRUE),
function(x) gsub("(?>([A-Z]))([A-Z]+)", "\\1\\L\\2", x, perl = TRUE),
function(x) gsub("\\s+", "", x, perl = TRUE))
