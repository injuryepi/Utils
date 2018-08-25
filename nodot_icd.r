nodot_icd <- function(x) gsub("([a-zA-Z]\\d{2})\\.(\\d+)", "\\1\\2", x, perl = T)
