
library("patchwork")

### Bias Boxplot Funktion
# Nimmt einen dataframe entgegen, wie er von mc_study() produziert wird
bias_boxplot <- function(df, title = "", xticks){
  
  boxplot <- df%>% 
    ggplot(., aes(x = term, y = rel_bias, fill = method))+
    geom_boxplot()+
    geom_hline(yintercept =  0, lty = 2) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    labs(x = "", y = "") +
    ggtitle(title) +
    scale_x_discrete(labels = xticks)
  boxplot
  
}


### coverage plot function
# Funktioniert noch nicht für die Interaktioneffekte
coverage_plot <- function(dfs, miss_perc, plot_names = c("β0", "β1", "β2", "μ(X3)"),
                          xlab, ylab) {
 
  dfs_raw <- lapply(seq_along(dfs), function(k) {
    dfs[[k]] %>%
      dplyr::group_by(method, term) %>%
      dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                      .groups = "drop") %>%
      dplyr::mutate(missperc = miss_perc[k])
  })


  dfs_combined <- bind_rows(dfs_raw, .id = "source_id")

  #List with all data split by Parameter
  dfs_parameter_info <- dfs_combined %>% group_split(term, .keep = TRUE)

  plot_names <- plot_names

  #List with coverage plots for each parameter
  plots_dfs <- vector("list", length(dfs_parameter_info))

  for (i in seq_along(dfs_parameter_info)) {
    plots_dfs[[i]] <- ggplot(dfs_parameter_info[[i]]) +
      geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
      geom_point(aes(x = missperc, y = coverage, color = method)) +
      geom_hline(yintercept = 0.9, linetype = 2) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = plot_names[i], x = xlab, y = NULL) +
      theme_classic()
  }


  # 1) Add y-axis title only to the leftmost plot
  plots_dfs[[1]] <- plots_dfs[[1]] + labs(y = "Coverage")

  # 2) Add custom info to the rightmost graph (top-right corner)
  plots_dfs[[4]] <- plots_dfs[[4]] +
    scale_y_continuous(
      name = NULL,
      # Right side: duplicate scale but only use it for the title
      sec.axis = dup_axis(name = ylab)
   ) + theme(
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y.right = element_text(margin = margin(l = 8)))

  plots_finished <- plots_mcar_x3_missing[[1]] +  plots_mcar_x3_missing[[2]] +
    plots_mcar_x3_missing[[3]] + plots_mcar_x3_missing[[4]] +
    plot_layout(ncol = 4,
                guides = "collect",
                axis_titles = "collect") &
    theme(legend.position = "bottom")

  plots_finished 
  
}





### coverage plot function
##der will den 5. plot noch nicht....
coverage_plot_int <- function(dfs, miss_perc, plot_names = c("β0", "β1", "β2", "β1*β2", "μ(X3)"),
                              xlab, ylab) {
  
  dfs_raw <- lapply(seq_along(dfs), function(k) {
    dfs[[k]] %>%
      dplyr::group_by(method, term) %>%
      dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::mutate(missperc = miss_perc[k])
  })
  
  
  dfs_combined <- bind_rows(dfs_raw, .id = "source_id")
  
  #List with all data split by Parameter
  dfs_parameter_info <- dfs_combined %>% group_split(term, .keep = TRUE)
  
  plot_names <- plot_names
  
  #List with coverage plots for each parameter
  plots_dfs <- vector("list", length(dfs_parameter_info))
  
  for (i in seq_along(dfs_parameter_info)) {
    plots_dfs[[i]] <- ggplot(dfs_parameter_info[[i]]) +
      geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
      geom_point(aes(x = missperc, y = coverage, color = method)) +
      geom_hline(yintercept = 0.9, linetype = 2) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = plot_names[i], x = xlab, y = NULL) +
      theme_classic()
  }
  
  
  # 1) Add y-axis title only to the leftmost plot
  plots_dfs[[1]] <- plots_dfs[[1]] + labs(y = "Coverage")
  
  # 2) Add custom info to the rightmost graph (top-right corner)
  plots_dfs[[5]] <- plots_dfs[[5]] +
    scale_y_continuous(
      name = NULL,
      # Right side: duplicate scale but only use it for the title
      sec.axis = dup_axis(name = ylab)
    ) + theme(
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y.right = element_text(margin = margin(l = 8)))
  
  plots_finished <- plots_mcar_x3_missing[[1]] +  plots_mcar_x3_missing[[2]] +
    plots_mcar_x3_missing[[3]] + plots_mcar_x3_missing[[4]] + plots_mcar_x3_missing[[5]] +
    plot_layout(ncol = 5,
                guides = "collect",
                axis_titles = "collect") &
    theme(legend.position = "bottom")
  
  plots_finished 
  
}