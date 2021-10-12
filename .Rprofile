
source("C:/code/ses_transitions/harmonization/.Rprofile")

path_gains_repo <- "C:/code/ses_transitions/wealth_gains"
path_mobility_repo <- "C:/code/ses_transitions/mobility"
path_cwealth_repo <- "C:/code/external/conditional-wealth"


expit = function(mu,se){
  out = case_when(!is.na(se) ~ paste0(exp(mu) %>% round(.,2)," (",
                                      exp(mu + qnorm(0.025)*se) %>% round(.,2),", ",
                                      exp(mu + qnorm(0.975)*se) %>% round(.,2),")"),
                  TRUE ~ "Ref (1.00)")
  return(out)
}
