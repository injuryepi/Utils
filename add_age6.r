add_age6 <- function(data, age) {
	
	suppressWarnings(suppressMessages(require(classInt)))
	suppressWarnings(suppressMessages(require(dplyr)))
	suppressWarnings(suppressMessages(require(forcats)))
	
	age <- enquo(age)
	
	age <- data %>% pull(!!age)
	
	agecut6 <- c(10, 17, 24, 34, 64, 84, max(age, na.rm = T))
	int6 <- classIntervals(age, n = 6, style = "fixed", fixedBreaks = agecut6, intervalClosure = "right")
	
	agegrp6 <- as.factor(findCols(int6))
	
	data %>% mutate(agegrp6 = agegrp6, age6 = fct_recode(agegrp6, `10_17` = "1",`18-24` = "2", `25-34` = "3", `35-64` = "4",`65-84` = "5", `85+` = "6"))

}
