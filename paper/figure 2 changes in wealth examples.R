plot_conditionals <- function(t_1,t_2,c_2,seed){
  
  c_2 = residuals(lm(t_2 ~ t_1))
  
  set.seed(seed)
  data.frame(t_1,t_2,c_2) %>% 
    arrange(t_1) %>% 
    mutate(id = 1:nrow(.),
           p_2 = t_2 - c_2) %>% 
    .[c(10,50,90),] %>% 
    bind_rows({.} %>% 
                dplyr::select(id,t_1,t_2) %>% 
                mutate(wealth = "wealth"),
              {.} %>% 
                dplyr::select(id,t_1,p_2) %>% 
                rename(t_2 = p_2) %>% 
                mutate(wealth = "predicted"))%>% 
    
    dplyr::filter(!is.na(wealth)) %>% 
    dplyr::select(id,t_1,t_2,wealth) %>% 
    mutate(wealth = factor(wealth,levels=c("wealth","predicted")),
           id = factor(id)) %>% 
    pivot_longer(cols=-one_of("id","wealth"),names_to="var",values_to="val") %>% 
    
    mutate(time = case_when(var == "t_1" ~ 1,
                            var == "t_2" ~ 2,
                            TRUE ~ NA_real_),
           id_wealth = paste0(as.character(id),"_",wealth)) %>% 
    mutate(time = factor(time)) %>% 
    ggplot(data=.,aes(x=time,y=val,shape=id)) +
    geom_point(size = 5) +
    geom_line(aes(group=id_wealth,linetype=wealth))
  
  
}



t_1 = rnorm(100,0,1)

# A  --------
set.seed(5000)
t_2 = 0.5 + 1.5*t_1 + rnorm(100,0,0.3)
plot_conditionals(t_1,t_2,seed = 5000)

# B ------------
set.seed(5000)
t_2 = 0.5 + 1.5*t_1 - rnorm(100,0,0.5)
plot_conditionals(t_1,t_2,seed = 5000)

# C --------
set.seed(5000)
t_2 = 0.0 - 1.5*t_1 - rnorm(100,0,2)
plot_conditionals(t_1,t_2,seed = 5000)

# D  --------
set.seed(5000)
t_2 = 1.5 + 1.5*t_1 - rnorm(100,0,0.3)
plot_conditionals(t_1,t_2,seed = 5000)

# E ---------
set.seed(5000)
t_2 = -1.5 + 1.5*t_1 + rnorm(100,0,0.3)
plot_conditionals(t_1,t_2,seed = 5000)

# F  --------
set.seed(5000)
t_2 = -0.2 + 0.3*t_1 - rnorm(100,0,0.3)
plot_conditionals(t_1,t_2,seed = 5000)


# G  --------
set.seed(5000)
t_2 = 0.0 - 3.5*t_1 + rnorm(100,0,2)
plot_conditionals(t_1,t_2,seed = 5000)

# H ----------
set.seed(5000)
t_2 = -0.5 + 1.5*t_1 - rnorm(100,0,1.0)
plot_conditionals(t_1,t_2,seed = 5000)
