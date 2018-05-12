od_wa_dth_trends <- function(data, filtering, pop, dr_labels, ...){
  
  filtering = enquo(filtering)
  grp_by = quos(...)
  
  
  suppressWarnings(suppressMessages(require(dplyr)))
   data %>% filter((!!filtering) == 1) %>% 
    group_by(!!!grp_by) %>% count %>% 
    rename(deaths = n) %>%
    right_join(pop) %>% 
    replace_na(list(deaths = 0)) %>% ungroup() %>% 
    mutate(drug_type = rep(dr_labels, nrow(.)))
  
  
}
