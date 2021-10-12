cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/cw_df.RDS"))


summary_wealth <- cw_df %>% 
  dplyr::select(contains("pc"),contains("cwealth")) %>% 
  pivot_longer(cols=everything(),names_to="measure",values_to="value") %>% 
    mutate(year = str_extract(measure,"[0-9]+"),
           measure = str_extract(measure,"[a-z]+")) %>% 
  group_by(measure,year) %>% 
  dplyr::summarize(mean = mean(value),
                   sd = sd(value),
                   var = var(value))


table2 <- bind_rows(
  summary_wealth %>% 
    dplyr::filter(measure == "pc") %>% 
    mutate(statistic = paste0(mean %>% round(.,2)," pm ",
                              sd %>% round(.,2)) %>% as.character(.),
           column = "Wealth"),
  summary_wealth %>% 
    dplyr::filter(measure == "pc") %>% 
    arrange(year) %>% 
    mutate(statistic = round(mean - dplyr::lag(mean),2) %>% as.character(.),
           column = "Change in mean wealth"),
  
  summary_wealth %>% 
    dplyr::filter(measure == "pc") %>% 
    arrange(year) %>% 
    mutate(inequality = sqrt(var/sum(var)),
           statistic = round(inequality - dplyr::lag(inequality),2) %>% as.character(.),
           column = "Change in inequality"),
  
  summary_wealth %>% 
    dplyr::filter(year != 1983) %>% 
    group_by(year) %>% 
    arrange(desc(measure)) %>% 
    summarize(statistic = round(var/dplyr::lag(var),2) %>% as.character(.),
              column = "Proportion of variance"),
  summary_wealth %>% 
    dplyr::filter(measure == "cwealth") %>% 
    mutate(statistic = paste0(mean %>% round(.,2)," pm ",
                              sd %>% round(.,2))%>% as.character(.),
           column = "Conditional wealth")
  
  
) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(statistic)) %>% 
  dplyr::select(year,statistic,column) %>% 
  pivot_wider(names_from=column,values_from=statistic)


write_csv(table2,paste0(path_dissertation,"/aim 0/working/table 2 summary of wealth.csv"))
