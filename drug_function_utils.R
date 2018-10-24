## To  create fed fiscal years

fed_fiscal_year <- function(date) {
  # October 1, year_n to September 30, year_n+1
  require(lubridate, quietly = T)
  year(date %m+% months(3))
}

## creating a new variable based on the regular expression from other fields

od_create_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {
  #expr = regular expressions 
  # colvec = vector of the columns of interest (columns with the diagnoses). indices
  # or variable names without quotation marks
  colvec = enquo(colvec)
  # assign '1' if the regular expression matched 
  f1 = function(x) as.numeric(grepl(expr, x, ignore.case = ignore.case, perl = perl))
  # any one in the diagnosis field suffices
  f2 = function(x){as.numeric(rowSums(x, na.rm = TRUE) > 0)} 
  
  data %>% select(!!colvec) %>% 
    mutate_all(funs(as.character)) %>% 
    map_df(f1) %>% 
    mutate(new_diag = f2(.)) %>% 
    pull(new_diag)
}

## find the appropriate icd-9-cm in multiple fields to capture the definition of a drug in a newly created variable

od_create_diag_9 <- function(data, expr1, colvec1, expr2, colvec2, ignore.case = T, perl = T) {
  #expr = regular expressions
  # colvec = vector of the columns of interest (columns with the diagnoses). indices
  # or variable names without quotation marks
  colvec1 = enquo(colvec1)
  colvec2 = enquo(colvec2)
  # assign '1' if the regular expression matched
  f1 = function(x) as.numeric(grepl(expr1, x, ignore.case = ignore.case, perl = perl))
  
  # assign '1' if the regular expression matched
  f2 = function(x) as.numeric(grepl(expr2, x, ignore.case = ignore.case, perl = perl))
  
  # any 1 in the diagnosis field suffices
  g = function(x){as.numeric(rowSums(x, na.rm = TRUE) > 0)}
  
  data1 <- data %>% select(!!colvec1) %>%
    mutate_all(funs(as.character)) %>%
    map_df(f1) %>%
    mutate(new_diag1 = g(.)) %>%
    pull(new_diag1)
  
  
  data2 <- data %>% select(!!colvec2) %>%
    mutate_all(funs(as.character)) %>%
    map_df(f2) %>%
    mutate(new_diag2 = g(.)) %>%
    pull(new_diag2)
  
  tibble(new_diag1 = data1, new_diag2 = data2) %>% mutate(new_diag = ifelse(new_diag1 == 1 | new_diag2 == 1, 1, 0)) %>% pull(new_diag)
  
}


### Capture the definitions of any_drug, any_opioid, heroin and non-heroin-opioid based on icd-9-cm

od_drug_apr_icd9cm <- function(data, diag_col, ecode_col) {
  
  drugs_icd9cm1_ <- "^9[67]"
  drugs_icd9cm2_ <- "^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5]"
  
  opioid_icd9cm1_ <- "^9650[0129]"
  opioid_icd9cm2_ <- "^E850[012]"
  
  non_heroin_icd9cm1_ <- "^9650[029]"
  non_heroin_icd9cm2_ <- "^E850[12]"
  
  heroin_icd9cm1_ <- "^96501"
  heroin_icd9cm2_ <- "^E8500"
  
  data %>% mutate(any_drug = od_create_diag_9(., expr1 = drugs_icd9cm1_, colvec1 = diag_col, expr2 = drugs_icd9cm2_, colvec2 = ecode_col),
                  
                  any_opioid = od_create_diag_9(., expr1 = opioid_icd9cm1_, colvec1 = diag_col, expr2 = opioid_icd9cm2_, colvec2 = ecode_col),
                  
                  non_heroin_opioid = od_create_diag_9(., expr1 = non_heroin_icd9cm1_, colvec1 = diag_col, expr2 = non_heroin_icd9cm2_, colvec2 = ecode_col),
                  heroin = od_create_diag_9(., expr1 = heroin_icd9cm1_, colvec1 = diag_col, expr2 = heroin_icd9cm2_, colvec2 = ecode_col)) %>%
    mutate( non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid))
}


### Capture the definitions of any_drug, any_opioid, heroin and non-heroin-opioid based on icd-10-cm

od_drug_apr_icd10cm <- function(data, diag_ecode_col) {
  
  drugs_icd10cm_ <- "^(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"
  
  opioid_icd10cm_ <- "(T40[01234].|T406[09])[1-4](A|$)"
  
  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$)"
  
  heroin_icd10cm_ <- "T401.[1-4](A|$)"
  
  
  data %>% mutate(any_drug = od_create_diag(., expr = drugs_icd10cm_,
                                            colvec = diag_ecode_col),
                  
                  any_opioid = od_create_diag(., expr = opioid_icd10cm_,
                                              colvec = diag_ecode_col),
                  
                  non_heroin_opioid = od_create_diag(., expr = non_heroin_opioid_icd10cm_,
                                                     colvec = diag_ecode_col),
                  
                  heroin = od_create_diag(., expr = heroin_icd10cm_,
                                          colvec = diag_ecode_col)) %>%
    mutate(non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid))
}
