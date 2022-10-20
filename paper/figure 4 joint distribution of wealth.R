
cw_df <- readRDS(paste0(path_dissertation,"/aim 0/working/cw_df.RDS"))

require(GGally)
conflict_prefer("wrap", "GGally")

source(paste0(path_incap_repo,"/2015-18/cor_func for GGAlly.R"))

panelA <- ggpairs(cw_df %>% 
          dplyr::select(starts_with("pc")),
        columnLabels = c("1983",
                         "1991",
                         "1994",
                         "1998",
                         "2002",
                         "2005",
                         "2009"
                         
        ),
        upper = list(continuous = wrap(cor_func,
                                       method = 'pearson', symbol = expression('\u03C1 ='))),
        lower = list(continuous = function(data, mapping, ...) {
          # ggally_smooth(data = data, mapping = mapping) +
          #   geom_point(size=0.2) +
          #   theme(panel.background = element_blank())
          ggplot(data=data,mapping=mapping) +
            geom_point(color="grey",alpha=0.3,size=0.5) +
            geom_smooth(color="black",method="lm",size=0.8) +
            theme(panel.background = element_blank())
          
        }),
        diag = list(continuous = function(data, mapping, ...) {
          ggally_barDiag(data = data, mapping = mapping) + 
            theme(panel.background = element_blank())}
        ))

ggsave(panelA,height=6,width=6,
       filename = paste0(path_dissertation,"/aim 0/figures/figure_joint distribution of wealth.tiff"))


panelB <- cw_df %>% 
  dplyr::select(uncchdid,starts_with("cwealth")) %>% 
  pivot_longer(cols=-uncchdid,names_to="year",values_to="cw",names_prefix = "cwealth") %>% 
  ggplot(data=.,aes(x=cw)) +
  geom_histogram() +
  theme_bw() +
  xlab("Conditional Wealth") +
  ylab("") +
  facet_wrap(~year)

panelB
