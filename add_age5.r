add_age5 <- function(data, age) {
	
	suppressWarnings(suppressMessages(require(classInt)))
	suppressWarnings(suppressMessages(require(dplyr)))
	suppressWarnings(suppressMessages(require(forcats)))
	
	age <- enquo(age)
	
	age <- data %>% pull(!!age)
	
	agecut5 <- c(18, 34, 44, 54, 64, max(age, na.rm = T))
	int5 <- classIntervals(age, n = 5, style = "fixed", fixedBreaks = agecut5, intervalClosure = "right")
	
	agegrp5 <- as.factor(findCols(int5))
	
	data %>% mutate(agegrp5 = agegrp5, age5 = fct_recode(agegrp5, `18-34` = "1",`35-44` = "2", `45-54` = "3", `55-64` = "4",`65+` = "5"))

}
