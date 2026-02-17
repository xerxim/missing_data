### Bias Boxplot Funktion
# Nimmt einen dataframe entgegen, wie er von mc_study() produziert wird
bias_boxplot <- function(df, xticks = c("β0", "β1", "β2", "μ(X3)"), title = ""){
  
  boxplot <- df%>% 
    ggplot(., aes(x = term, y = bias, fill = method))+
    geom_boxplot()+
    geom_hline(yintercept =  0, lty = 2) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    labs(x = "", y = "") +
    ggtitle(title) +
    scale_x_discrete(labels = xticks)
  boxplot
  
}




### coverage plot function
coverage_plot <- function(dfs, miss_perc, plot_names,
                          xlab, ylab) {
 

   
  
  
}