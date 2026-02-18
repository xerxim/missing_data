# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse, mice,cowplot,gridExtra)
## Eigener Code:
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
sys.source("project/src/mc_helpers.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")


#MC Simulation Result import
load("project/raw_mc_results/mc_raw_results_s=300_interaction_effects.RData")

iia <- dfs_mar_X3_miss_nonlin
iib <- dfs_mcar_all_miss_nonlin
iic <- dfs_mar_X3_miss_nonlin
iid <- dfs_mar_all_miss_nonlin

miss_perc <- c(10, 20, 30, 40, 50)

plot_names <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(beta[3]),
  expression(mu[3])
)

make_coverages_plot <- function(list_names,
                                  miss_perc,
                                  plot_names,
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

make_coverages_plot(c("iia", "iib", "iic", "iid"), 
  miss_perc, plot_names)
