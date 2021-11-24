
cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/public_df.RDS")) %>% 
  dplyr::filter(!is.na(pc1983),!is.na(pc1991),
                !is.na(pc1994),!is.na(pc1998),
                !is.na(pc2002),!is.na(pc2005),
                !is.na(pc2009)) %>% 
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
  
  # dplyr::filter(!is.na(pc2018)) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate_at(vars(contains("rural")),
            list(imp = function(x) case_when(is.na(x) ~ 1,
                                             TRUE ~ x)))


# CONDITIONAL WEALTH --------------

model_cwealth1991 <- '
pc1991 ~ pc1983
'
result_cwealth1991 <- with(cw_df,lm(as.formula(model_cwealth1991)))
cw_df$cwealth1991 <- residuals(result_cwealth1991,method="identity")

model_cwealth1994 <- '
pc1994 ~ pc1983 + pc1991
'
result_cwealth1994 <- with(cw_df,lm(as.formula(model_cwealth1994)))
cw_df$cwealth1994 <- residuals(result_cwealth1994,method="identity")

model_cwealth1998 <- '
pc1998 ~ pc1983 + pc1991 + pc1994
'
result_cwealth1998 <- with(cw_df,lm(as.formula(model_cwealth1998)))
cw_df$cwealth1998 <- residuals(result_cwealth1998,method="identity")

model_cwealth2002 <- '
pc2002 ~ pc1983 + pc1991 + pc1994 + pc1998
'
result_cwealth2002 <- with(cw_df,lm(as.formula(model_cwealth2002)))
cw_df$cwealth2002 <- residuals(result_cwealth2002,method="identity")

model_cwealth2005 <- '
pc2005 ~ pc1983 + pc1991 + pc1994 + pc1998 + pc2002
'
result_cwealth2005 <- with(cw_df,lm(as.formula(model_cwealth2005)))
cw_df$cwealth2005 <- residuals(result_cwealth2005,method="identity")


# 2009 ------
model_cwealth2009 <- '
pc2009 ~ pc1983 + pc1991 + pc1994 + pc1998 + pc2002 + pc2005
'
result_cwealth2009 <- with(cw_df,lm(as.formula(model_cwealth2009)))
cw_df$cwealth2009 <- residuals(result_cwealth2009,method="identity")


saveRDS(cw_df, paste0(path_dissertation,"/aim 0/working/cw_df.RDS"))

