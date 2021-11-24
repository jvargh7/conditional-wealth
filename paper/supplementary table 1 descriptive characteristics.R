public_df <- readRDS(paste0(path_dissertation,"/aim 0/working/public_df.RDS"))

library(compareGroups)
tab1_df <- public_df %>% 
  mutate(formal_ya = case_when(!is.na(formal2005) & !is.na(formal2009) ~ formal2009,
                               !is.na(formal2009) ~ formal2009,
                               !is.na(formal2005) ~ formal2005,
                               TRUE ~ NA_real_),
         weight_ya = case_when(!is.na(weight2005) & !is.na(weight2009) ~ weight2009,
                               !is.na(weight2009) ~ weight2009,
                               !is.na(weight2005) ~ weight2005,
                               TRUE ~ NA_real_),
         bmi_ya = case_when(weight_ya < 0 | height2005 <0 ~ NA_real_,
                            !is.na(height2005) & !is.na(weight_ya) ~ weight_ya/(height2005/100)^2,
                            TRUE ~ NA_real_),
         
         pregnant_ya = case_when(male == 1 ~ -1,
                                 !is.na(weight2005) & !is.na(weight2009) ~ pregnant2009,
                                 !is.na(weight2009) ~ pregnant2009,
                                 !is.na(weight2005) ~ pregnant2005,
                                 TRUE ~ NA_real_)
  ) %>% 
  
  mutate(status = case_when(is.na(pc2009) ~ "2, Not in 2009",
                            !is.na(pc1991)&
                            !is.na(pc1994)&!is.na(pc1998)&
                            !is.na(pc2002)&!is.na(pc2005) ~ "1, Analytic sample",
                            TRUE ~ "3, Excluded from CC")
         ) 

tab1_df %>% 
  compareGroups(status ~ moscho + moage + chbirtho +
                  male + 
                  rural1983 + rural1991 + rural1994 +
                  rural1998 + rural2002 + rural2005 +
                  rural2009 + pregnant_ya + formal2009 +
                  eduyr + bmi_ya + 
                  pc1983 + pc1991 + pc1994 +
                  pc1998 + pc2002 + pc2005 +
                  pc2009,data=.,
                include.label = FALSE,include.miss = TRUE,
                method = c(2,2,2,
                           3,
                           3,3,3,
                           3,3,3,
                           3,3,3,
                           2, 1,
                           1,1,1,
                           1,1,1,
                           1)) %>% 
  createTable(.,digits=1,show.all=TRUE,show.n = TRUE,q.type = c(2,2),sd.type = 2,type = 2) %>% 
  export2xls(.,paste0(path_cwealth_repo,"/data/table1.xlsx"))

tab1_df %>% 
  compareGroups(status ~ moscho + moage + chbirtho +
                  male + 
                  rural1983 + rural1991 + rural1994 +
                  rural1998 + rural2002 + rural2005 +
                  rural2009 + pregnant_ya + formal2009 +
                  eduyr + bmi_ya + 
                  pc1983 + pc1991 + pc1994 +
                  pc1998 + pc2002 + pc2005 +
                  pc2009,data=.,
                include.label = FALSE,include.miss = FALSE,
                method = c(2,2,2,
                           3,
                           3,3,3,
                           3,3,3,
                           3,3,3,
                           2, 1,
                           1,1,1,
                           1,1,1,
                           1)) %>% 
  createTable(.,digits=1,show.all=TRUE,show.n = TRUE,q.type = c(2,2),sd.type = 2,type = 1) %>% 
  export2xls(.,paste0(path_dissertation,"/aim 0/working/table1_nomiss.xlsx"))
