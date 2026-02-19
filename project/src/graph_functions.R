# Function that feed on a vector with list names (each list should contain dfs that are an MC result) 
# and displays coverage plots - Tom

make_coverages_plot <- function(list_names,
                                  plot_names,
                                  miss_perc = c(10, 20, 30, 40, 50),
                                  row_labels = c("a","b","c","d"),
                                  xlab = expression("Missing % in " * X[3])) {
  
  
  # To store the final rows of plots (one per list in list_names)
  all_plots <- vector("list", length(list_names))
  
  # Loop over each list name
  for (L in seq_along(list_names)) {
    
    # Retrieve the actual list from the environment
    df_list <- get(list_names[L], envir = .GlobalEnv)
    
    # --- 1) Summaries per df in the list ---
    raw_list <- lapply(seq_along(df_list), function(k) {
      df_list[[k]] %>%
        group_by(method, term) %>%
        summarise(
          coverage = mean(as.numeric(cover), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(missperc = miss_perc[k])
    })
    
    # --- 2) Combine ---
    combined <- bind_rows(raw_list, .id = "source_id")
    
    # --- 3) Split by parameter ---
    param_list <- combined %>% group_split(term, .keep = TRUE)
    
    # --- 4) Build plots for this list ---
    plots <- vector("list", length(param_list))
    
    for (i in seq_along(param_list)) {
      plots[[i]] <- ggplot(param_list[[i]]) +
        geom_line(aes(missperc, coverage, color = method), alpha = 0.4) +
        geom_point(aes(missperc, coverage, color = method)) +
      #  paletteer::scale_colour_paletteer_d("wesanderson::AsteroidCity1")+
        geom_hline(yintercept = 0.9, linetype = 2) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(title = NULL, x = xlab, y = NULL, color = "Imputation Method") +
        theme_classic()

      #First row gets titles
      if(L == 1){
        plots[[i]] <- plots[[i]]+
          labs(title = plot_names[i] )
      }
    }
    
    # --- 5) First plot gets y-label ---
    plots[[1]] <- plots[[1]] + labs(y = "Coverage")
    
    # --- 6) Last plot gets right-side annotation ---
    last_idx <- length(plots)
    plots[[last_idx]] <- plots[[last_idx]] +
      scale_y_continuous(
        name = NULL,
        sec.axis = dup_axis(name = row_labels[L])
      ) +
      theme(
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right  = element_blank(),
        axis.title.y.right = element_text(margin = margin(l = 8))
      )
    
    # --- 7) Store in general plotlist
    
    all_plots[[L]] <- plots
  }
  
# --- 8) Combine all plots ---

  n_rows <- length(all_plots)
  n_cols <- unique(vapply(all_plots, length, integer(1)))

  # Flatten list-of-lists into a single list of plots (row-major order)
  flat_plots <- unlist(all_plots, recursive = FALSE)

  # Arrange as a grid: columns = number of parameters, rows = number of lists
  combined_plot <-
    wrap_plots(flat_plots, ncol = n_cols) +
    plot_layout(
      ncol = n_cols,
      nrow = n_rows,
      guides = "collect",
      axis_titles = "collect"
    ) &
    theme(legend.position = "bottom")

    
      return(combined_plot)
}

bias_boxplot <- function(df, xticks = c("β0", "β1", "β2", "μ(X3)"), title = "Relativer Bias", ylim = NULL){
  
 
p <- df %>% 
    ggplot(aes(x = term, y = rel_bias, fill = method)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, lty = 2) +
    scale_x_discrete(labels = xticks) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 20)
    ) +
    labs(x = "", y = "Relative Bias") +
    ggtitle(title) +
    theme_classic()
  
  # Only apply limits if user supplies them
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  return(p)
}

  
