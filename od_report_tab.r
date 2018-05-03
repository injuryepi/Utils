od_report_tab <- function(data, filtering, geography = NULL, expand_tab, drug_name, outcome, geography_type, ...){
	
	options(warn = -1)
	suppressWarnings(suppressMessages(require(dplyr)))
	
	filtering = enquo(filtering)
	grp_by = quos(...)
	
	dat = data %>% 
		filter((!!filtering) == 1) %>% 
		group_by(!!!grp_by) %>% 
		count %>% rename(count = n) %>% ungroup() %>% 
		right_join(expand_tab) %>% replace_na(list(count = 0)) %>%
		mutate(drug_type = rep(drug_name, nrow(.)),
					 outcome = rep(outcome, nrow(.)),
					 geography_type = rep(geography_type, nrow(.))) %>% 
		add_column(year = substr(.$year_quarter, 1, 4), .after = 1)
	
	if (is.null(geography)) {
		dat %>% 
			select(geography, year, year_quarter, count, drug_type, outcome, geography_type)
	} else {
		dat %>% 
			add_column(geography = rep(geography, nrow(.)), .before = 1) %>% 
			select(geography, year, year_quarter, count, drug_type, outcome, geography_type)
	} 
	
} 
