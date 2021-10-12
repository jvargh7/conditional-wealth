
# This is an internal code to source scripts from other projects --------

pca_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
  dplyr::select(uncchdid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  dplyr::select(-pc2018)

source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
source(paste0(path_harmonization_repo,"/philippines/ph_region.R"))

ph_region <- ph_region %>% 
  pivot_wider(names_from="survey",values_from="cstratum",names_prefix = "cstratum")

source(paste0(path_mobility_repo,"/philippines/phaux01_covariates.R"))
source(paste0(path_gains_repo,"/philippines/phwgaux04_young adulthood covariates.R"))

public_df <- pca_df %>% 
  left_join(ph_region,
            by="uncchdid") %>% 
  mutate_at(vars(starts_with("cstratum")),function(x) case_when(!is.na(x) ~ x - 1,
                                                                TRUE ~ NA_real_)) %>% 
  rename_at(vars(starts_with("cstratum")), ~str_replace(.,"cstratum","rural")) %>% 
  dplyr::select(-rural2018) %>% 
  left_join(early_life %>% 
              # momarst: Only 2.44% are unmarried and 1.3% are missing
              dplyr::select(uncchdid,moage,moscho,chsex,chbirtho) %>% 
              mutate(
                chbirtho = case_when(chbirtho == "first" ~ 1,
                                     chbirtho == "second" ~ 2,
                                     chbirtho == "third" ~ 3,
                                     chbirtho == "fourth or more" ~ 4),
                chsex = case_when(chsex == "male" ~ 1,
                                  TRUE ~ 0)
              ) %>% 
              rename(
                male = chsex),
            by="uncchdid") %>% 
  left_join(philippines_dfa %>% 
              dplyr::select(uncchdid,adeduyr) %>% 
              rename(eduyr = adeduyr),
            by = "uncchdid") %>% 
  # Young adult covariates -----------
left_join(covariates05 %>% 
            dplyr::select(-sex),
          by="uncchdid") %>% 
  left_join(covariates09 %>% 
              dplyr::select(-icsex),
            by="uncchdid") 


saveRDS(public_df,paste0(path_dissertation,"/aim 0/working/public_df.RDS"))
