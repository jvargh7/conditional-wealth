cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/cw_df.RDS")) %>% 
  mutate(bmi_ya = case_when(pregnant2009 == 1 ~ NA_real_,
                            TRUE ~ bmi_ya))


formula_bmi_p = "bmi_ya ~ male + moscho + moage + chbirtho + rural1983 + rural1991 + rural1994 + rural1998 + rural2002 + eduyr + rural2005 + rural2009 + formal2009 + pc1983 + cwealth1991 + cwealth1994 + cwealth1998 + cwealth2002 + cwealth2005 + cwealth2009"  
formula_bmi_s = "bmi_ya ~ moscho + moage + chbirtho + rural1983 + rural1991 + rural1994 + rural1998 + rural2002 + eduyr + rural2005 + rural2009 + formal2009 + pc1983 + cwealth1991 + cwealth1994 + cwealth1998 + cwealth2002 + cwealth2005 + cwealth2009"  


associations <- bind_rows(
  lm(as.formula(formula_bmi_p),data=cw_df) %>% 
    broom::tidy() %>% 
    mutate(sex = "Pooled"),
  lm(as.formula(formula_bmi_s),data=cw_df %>% dplyr::filter(male == 0)) %>% 
    broom::tidy() %>% 
    mutate(sex = "Female"),
  lm(as.formula(formula_bmi_s),data=cw_df %>% dplyr::filter(male == 1)) %>% 
    broom::tidy() %>% 
    mutate(sex = "Male")
) %>% 
  mutate(lci = estimate - 1.96*std.error,
        uci = estimate + 1.96*std.error)

associations %>% 
  dplyr::filter(term %in% c("pc1983",
                            "cwealth1991",
                            "cwealth1994",
                            "cwealth1998",
                            "cwealth2002",
                            "cwealth2005",
                            "cwealth2009")) %>% 
  mutate(iv = factor(term,levels=c("pc1983",
                                   "cwealth1991",
                                   "cwealth1994",
                                   "cwealth1998",
                                   "cwealth2002",
                                   "cwealth2005",
                                   "cwealth2009"),
                     labels = c("Wealth in 1983",
                                "Conditional Wealth 1991",
                                "Conditional Wealth 1994",
                                "Conditional Wealth 1998",
                                "Conditional Wealth 2002",
                                "Conditional Wealth 2005",
                                "Conditional Wealth 2009"
                                ))) %>% 
  ggplot(data=.,aes(x=iv,y=estimate,ymin=lci,ymax=uci,shape = sex)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),width=0) +
  geom_hline(yintercept=0,col="grey80",linetype=2) +
  ylab("BMI (kg/m2)") +
  xlab("") +
  theme_bw() +
  theme(legend.position="bottom",
        
        axis.text.x = element_text(size=8)) +
  scale_y_continuous(limits = c(-1,2)) +
  # scale_color_manual(name="",values=c("purple","darkblue","green4")) +
  scale_shape_manual(name="",values=c(1,2,3)) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))

