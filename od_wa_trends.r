od_wa_trends <- function(data, filtering, pop, dr_labels, ...){
  
  filtering = enquo(filtering)
  grp_by = quos(...)
    
  suppressWarnings(suppressMessages(require(dplyr)))
  data %>% filter((!!filtering) == 1) %>% 
    group_by(!!!grp_by) %>% count %>% 
    rename(counts = n) %>%
    right_join(pop) %>% 
    replace_na(list(counts = 0)) %>% ungroup() %>% 
    mutate(drug_type = rep(dr_labels, nrow(.)))
}

# example
# sex_drugsh9917xpop <- drug_hosp_1999_17_res %>% 
# od_wa_trends(filtering = any_drug,
#              pop = sex_age11pop9917, 
#              dr_labels = "any_drug", year, sex, agegrp11, age11)
