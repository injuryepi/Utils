nodot_icd <- function(x) gsub("([a-zA-Z])(\\d+)\\.(\\d+)", "\\1\\2\\3", x, perl = TRUE)
