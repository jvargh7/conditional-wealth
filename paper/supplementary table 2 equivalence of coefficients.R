cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/cw_df.RDS")) %>% 
  mutate(bmi_ya = case_when(pregnant2009 == 1 ~ NA_real_,
                            TRUE ~ bmi_ya))


formula_bmi_0 = "bmi_ya ~ male + moscho + moage + chbirtho + rural1983_imp"  
formula_bmi_1 = "bmi_ya ~ pc1983 + male + moscho + moage + chbirtho + rural1983_imp"  
formula_bmi_2 = "bmi_ya ~ pc1991 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp"  
formula_bmi_3 = "bmi_ya ~ pc1994 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp + rural1994_imp"  
formula_bmi_4 = "bmi_ya ~ pc1983 + pc1991 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp"  
formula_bmi_5 = "bmi_ya ~ pc1983 + cwealth1991 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp"  
formula_bmi_6 = "bmi_ya ~ pc1983 + pc1991 + pc1994 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp + rural1994_imp"  
formula_bmi_7 = "bmi_ya ~ pc1983 + cwealth1991 + cwealth1994 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp + rural1994_imp"  
formula_bmi_8 = "bmi_ya ~ pc1983 + pc1991 + cwealth1994 + male + moscho + moage + chbirtho + rural1983_imp + rural1991_imp + rural1994_imp"  

lm_summary <- function(formula_y,data){
  lm_out = lm(as.formula(formula_y),data=data) 
  
  broom::tidy(lm_out) %>% 
    mutate(adj_rsq = summary(lm_out)$adj.r.squared) %>% 
    return(.)
  
  
}


stable2_associations <- bind_rows(
  lm_summary(as.formula(formula_bmi_0),data=cw_df) %>% 
    mutate(model = " ~ Early life covariates"),
  lm_summary(as.formula(formula_bmi_1),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983"),
  lm_summary(as.formula(formula_bmi_2),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1991"),
  lm_summary(as.formula(formula_bmi_3),data=cw_df) %>% 
    mutate(model = "~ Wealth 1994"),
  lm_summary(as.formula(formula_bmi_4),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983 + Wealth 1991"),
  lm_summary(as.formula(formula_bmi_5),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983 + Conditional 1991"),
  lm_summary(as.formula(formula_bmi_6),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983 + Wealth 1991 + Wealth 1994"),
  lm_summary(as.formula(formula_bmi_7),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983 + Conditional 1991 + Conditional 1994"),
  lm_summary(as.formula(formula_bmi_8),data=cw_df) %>% 
    mutate(model = " ~ Wealth 1983 + Wealth 1991 + Conditional 1994")
) %>% 
  dplyr::filter(term %in% c("(Intercept)","pc1983","pc1991","pc1994",
                            "cwealth1991","cwealth1994")) %>% 
  mutate(lci = estimate - 1.96*std.error,
         uci = estimate + 1.96*std.error) %>% 
  mutate(coef_ci = paste0(round(estimate,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")"),
         adj_rsq = round(adj_rsq,3)) 

stable2_associations %>% 
  mutate(term = str_replace(term,"(pc|cwealth)","")) %>% 
  dplyr::select(model,adj_rsq,term,coef_ci) %>% 
  pivot_wider(names_from = term,values_from=coef_ci) %>% 
write.csv(.,paste0(path_dissertation,"/aim 0/working/supplementary table2_equivalence association.csv"))


