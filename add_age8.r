add_age8 <- function(data, age) {
	
	suppressWarnings(suppressMessages(require(classInt)))
	suppressWarnings(suppressMessages(require(dplyr)))
	suppressWarnings(suppressMessages(require(forcats)))
	
	age <- enquo(age)
	
	age <- data %>% pull(!!age)
	
	agecut8 <- c(10, 17, 24, 34, 44, 54, 64, 84, max(age, na.rm = T))
	int8 <- classIntervals(age, n = 8, style = "fixed", fixedBreaks = agecut8, intervalClosure = "right")
	
	agegrp8 <- as.factor(findCols(int8))
	
	data %>% mutate(agegrp8 = agegrp8, age8 = fct_recode(agegrp8, `10_17` = "1",`18-24` = "2", `25-34` = "3", `35-44' = "4", '45-54' = "5", '55-64` = "6",`65-84` = "7", `85+` = "8"))

}
