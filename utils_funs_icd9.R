

# load packages. Install if needed ----------------------------------------

#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl)
#install.packages("readxl")
library(classInt)

# Create a new variable based on a regular expression pattern -------------
# From https://github.com/injuryepi/overdoser/blob/master/R/od_create_diag.r
# This function has been used in the function od_drug_types_icd9cm()

od_create_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {
  require(dplyr, quietly = T)
  require(tidyr, quietly = T)
  sel <- names(data)[colvec]
  df <- as_data_frame(data[sel]) %>% mutate_all(funs(as.character))
  f <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl) + 
    0
  df <- sapply(df, f)
  df <- as_data_frame(df) %>% mutate(new_diag = rowSums(., na.rm = TRUE)) %>% 
    select(new_diag)
  as.factor(as.numeric((df[, 1] > 0)))
  
}


# Find drug types based on ICD-9-CM ---------------------------------------
# From https://github.com/injuryepi/overdoser/blob/master/R/od_drug_types_icd9cm.R

od_drug_types_icd9cm <- function(data, diag_ecode_col) {
  
  cdc_drugs_icd9cm_regex_ <- "^9[67]|^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5]"
  
  cdc_opioid_icd9cm_regex_ <- "^9650[0129]|^E850[012]"
  
  cdc_non_heroin_icd9cm_regex_ <- "^9650[029]|^E850[12]"
  
  cdc_heroin_icd9cm_regex_ <- "^96501|^E8500"
  
  data %>% mutate(
    any_drug_icd9cm = od_create_diag(., expr = cdc_drugs_icd9cm_regex_, 
                                                   colvec = diag_ecode_col),
    any_opioid_icd9cm = od_create_diag(., expr = cdc_opioid_icd9cm_regex_,
                                              colvec = diag_ecode_col),
    non_heroin_icd9cm = od_create_diag(., expr = cdc_non_heroin_icd9cm_regex_,
                                              colvec = diag_ecode_col),
    heroin_icd9cm = od_create_diag(., expr = cdc_heroin_icd9cm_regex_,
                                          colvec = diag_ecode_col)
    )
}


# Add the four age group to use with the dummy data -----------------------
# From https://github.com/injuryepi/injuryepi/blob/master/R/add_age4.r

add_age4 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(tidyverse)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(age)
  
  agecut4 <- c(0, 24,44, 64, max(age, na.rm = T))
  int4 <- classIntervals(age, n = 4, style="fixed", fixedBreaks=agecut4, intervalClosure = "right")
  
  agegrp4 <- as.factor(findCols(int4))
  
  mutate(data, 
         agegrp4 = agegrp4,
         age4 = fct_recode(agegrp4, "<25" = "1", 
                           "25-44" = "2",
                           "45-64" = "3",
                           "65+"   = "4"))
 }


# Copy to clipboard -------------------------------------------------------



onclip <- function (x, row.names = F, col.names = T)
{
  write.table(x, file = "clipboard", sep = "\t", row.names = row.names, col.names = col.names)
}
