formula1991 = "cwealth1991 ~ moscho + moage + chbirtho + male + rural1983 + rural1991"  
formula1994 = "cwealth1994 ~ moscho + moage + chbirtho + male + rural1983 + rural1991 + rural1994"  
formula1998 = "cwealth1998 ~ moscho + moage + chbirtho + male + rural1983 + rural1991 + rural1994 + rural1998"  
formula2002 = "cwealth2002 ~ moscho + moage + chbirtho + male + rural1983 + rural1991 + rural1994 + rural1998 + rural2002 + eduyr"  
formula2005 = "cwealth2005 ~ moscho + moage + chbirtho + male + rural1983 + rural1991 + rural1994 + rural1998 + rural2002 + eduyr + rural2005"  
formula2009 = "cwealth2009 ~ moscho + moage + chbirtho + male + rural1983 + rural1991 + rural1994 + rural1998 + rural2002 + eduyr + rural2005 + rural2009 + formal2009"  

cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/cw_df.RDS"))


predictors <- map_dfr(list(formula1991,
                           formula1994,
                           formula1998,
                           formula2002,
                           formula2005,
                           formula2009),
                      function(f){
                        lm(as.formula(f),
                           data=cw_df) %>% 
                          broom::tidy() %>% 
                          mutate(formula = f) %>% 
                          mutate(dv = str_extract(formula,"[a-z0-9]+\\s~") %>% 
                                   str_replace("\\s~",""))
                        
                      }) %>% 
  mutate(coef = paste0(round(estimate,2)," (",
                       round(estimate - 1.96*std.error,2),", ",
                       round(estimate + 1.96*std.error,2),")")) %>% 
  dplyr::select(dv,term,coef) %>% 
  pivot_wider(names_from="dv",values_from="coef") %>% 
  mutate_all(~case_when(is.na(.) ~ "",
                        TRUE ~ .))

write_csv(predictors,paste0(path_dissertation,"/aim 0/working/table 3 predictors.csv"))

