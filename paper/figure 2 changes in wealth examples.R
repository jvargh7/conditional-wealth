require(tidyverse)
require(ggpubr)
plot_conditionals <- function(t_1,t_2,title=""){
  
  c_2 = residuals(lm(t_2 ~ t_1))
  
  fig = data.frame(t_1,t_2,c_2) %>% 
    arrange(t_1) %>% 
    mutate(id = 1:nrow(.),
           p_2 = t_2 - c_2) %>% 
    .[c(50,500,950),] %>% 
    bind_rows({.} %>% 
                dplyr::select(id,t_1,t_2) %>% 
                mutate(wealth = "wealth"),
              {.} %>% 
                dplyr::select(id,t_1,p_2) %>% 
                rename(t_2 = p_2) %>% 
                mutate(wealth = "predicted")) %>% 
    
    dplyr::filter(!is.na(wealth)) %>% 
    dplyr::select(id,t_1,t_2,wealth) %>% 
    mutate(wealth = factor(wealth,levels=c("wealth","predicted")),
           id = case_when(id == 50 ~ "05th",
                       id == 500 ~ "50th",
                       id == 950 ~ "95th")) %>% 
    pivot_longer(cols=-one_of("id","wealth"),names_to="var",values_to="val") %>% 
    
    mutate(time = case_when(var == "t_1" ~ 1,
                            var == "t_2" ~ 2,
                            TRUE ~ NA_real_),
           id_wealth = paste0(as.character(id),"_",wealth)) %>% 
    mutate(time = factor(time)) %>% 
    ggplot(data=.,aes(x=time,y=val,shape=id)) +
    geom_point(size = 5) +
    geom_line(aes(group=id_wealth,linetype=wealth)) +
    theme_bw() +
    ggtitle(title) +
    theme(title = element_text(size = 8)) +
    scale_linetype_discrete(name = "") +
    scale_shape_discrete(name="Percentile at \n t = 1") +
    xlab("Time") +
    ylab("Harmonized wealth")
  
  fig %>% 
    return(.)
  
  
}


plot_ranks <- function(t_1,t_2,title=""){
  
  c_2 = residuals(lm(t_2 ~ t_1))
  fig = data.frame(t_1,t_2,c_2) %>% 
    mutate(rank_1 = rank(t_1),
           rank_2 = rank(t_2)) %>%
    mutate(rank_change = rank_2 - rank_1) %>% 
    sample_n(size=100) %>% 
    ggplot(data=.,aes(x=c_2,y=rank_change)) +
    geom_point() +
    xlab("Conditional Wealth") +
    ylab("Rank at t=2 minus Rank at t=1") +
    theme_bw() +
    scale_x_continuous(limits=c(-1,1),breaks=seq(-1,1,by=0.5))
  
  fig %>% return(.)
  
}



set.seed(5000)
w_1 = rnorm(1000,0,1)

# A  --------

w_2 = 0.5 + 1.5*w_1 + rnorm(1000,0,0.3)
plot_conditionals(w_1,w_2)

# B ##------------
w_2 = 0.5 - 0.5*w_1 + rnorm(1000,0,0.2)
plot_conditionals(w_1,w_2)

# C --------
w_2 = 0.5 - 0.5*w_1 - rnorm(1000,0,0.5)
plot_conditionals(w_1,w_2)

# D  --------
w_2 = 1.0 + 0.2*w_1 - rnorm(1000,0,0.5)
plot_conditionals(w_1,w_2)

# E ---------
w_2 = -1.5 + 1.5*w_1 + rnorm(1000,0,0.3)
plot_conditionals(w_1,w_2)

# F  --------
w_2 = -0.5 - 0.3*w_1 - rnorm(1000,0,0.5)
plot_conditionals(w_1,w_2)


# G  --------
w_2 = -0.5 - 1.0*w_1 + rnorm(1000,0,0.5)
plot_conditionals(w_1,w_2)

# H ----------
w_2 = -0.5 + 1.0*w_1 - rnorm(1000,0,0.3)
plot_conditionals(w_1,w_2)


b0 = c(-1,-0.5,-0.3,0,0.3,0.5,1)
b1 = c(-1,-0.5,0,0.5,1)
sigma = c(0.2,0.5,1)

params <- expand.grid(b0,b1,sigma) %>% 
  rename(b0 = Var1,
         b1 = Var2,
         sigma = Var3)
pdf(file = "paper/Supplementary File 2.pdf")
for (i in 1:nrow(params)){
  w_2 = params[i,]$b0 + params[i,]$b1*w_1 + rnorm(1000,0,params[i,]$sigma)
  title = paste0("S2Fig ",i,": \nExample with ",
                 "w_1 = N(0,1)",
                 "\nw_2 = ",params[i,]$b0," + ",
                 params[i,]$b1,"*w_1 + N(0,",
                 params[i,]$sigma,")",
                 "\nRatio of variance at time 2: time 1 = ",round(var(w_2),1));
  figA <- plot_conditionals(w_1,w_2,title="");
  figB <- plot_ranks(w_1,w_2)
  ggarrange(figA,figB,nrow = 1,ncol=2,labels = LETTERS[1:2]) %>% 
    annotate_figure(., top = text_grob(title, 
                                          color = "black", face = "bold", size = 12)) %>% 
    print(.)
  
  
}

dev.off()

