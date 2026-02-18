# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse, mice,cowplot,gridExtra)
## Eigener Code:
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
sys.source("project/src/mc_helpers.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")

# Working constants:
plot_path <- "project/plots/" 

# Activate multithreading.
future::plan(future::multicore, workers = (future::availableCores() - 2)) 
# Activate progress bar.
progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_cli(
  format = "{cli::pb_bar} {cli::pb_percent} | ETA {cli::pb_eta}"
))

cycles <- 500

dfs_mcar_X3_miss_nonlin <- list()
i <- 1


## 1) MCAR X3
## ---- timed MC Start ---
time_start_mcar_x3 <- Sys.time()
paste("MC run 1, started at", time_start_mcar_x3)
for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- cycles
  
  dfs_mcar_X3_miss_nonlin[[i]] <- mc$mc_study_furrr(
    methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
    true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
    data_generator = mc$generate_data_nonlinear_weak,
    n = 500, cycles = mc_cycles, miss_vars = "X3", true_means = c("X3" = 4.55),
    miss = "MCAR", miss_rates =  mis_rate, miss_aux = NULL, seed = NA
  )
  
  i <- i+1
  
}
# --- Stop timer ---
time_end_mcar_x3 <- Sys.time()
runtime <- time_end_mcar_x3 - time_start_mcar_x3
paste("MC for MCAR, missings un x3 with ", cycles, " Simulation cycles ran for: ", runtime)


miss_perc <- c(10, 20, 30, 40, 50)

# process each df and return a list of summarised tibbles
mcar_x3_missing_raw <- lapply(seq_along(dfs_mcar_X3_miss_nonlin), function(k) {
  dfs_mcar_X3_miss_nonlin[[k]] %>%
    dplyr::group_by(method, term) %>%
    dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(missperc = miss_perc[k])
})


### Coverage Plots

mcar_x3_missing_combined <- bind_rows(mcar_x3_missing_raw, .id = "source_id")

#List with all data split by Parameter
mcar_x3_missing_parameter_info <- mcar_x3_missing_combined %>% group_split(term, .keep = TRUE)

plot_names <- c(expression(beta[0]), expression(beta[1]), 
  expression(beta[2]), expression(beta[3]), expression(mu[3]))

#List with coverage plots for each parameter
plots_mcar_x3_missing <- vector("list", length(mcar_x3_missing_parameter_info))

for (i in seq_along(mcar_x3_missing_parameter_info)) {
  plots_mcar_x3_missing[[i]] <- ggplot(mcar_x3_missing_parameter_info[[i]]) +
    geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
    geom_point(aes(x = missperc, y = coverage, color = method)) +
    geom_hline(yintercept = 0.9, linetype = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = plot_names[i], x = "Missing in X3 (%)", y = NULL) +
    theme_classic()
}


# 1) Add y-axis title only to the leftmost plot
plots_mcar_x3_missing[[1]] <- plots_mcar_x3_missing[[1]] + labs(y = "Coverage")

# 2) Add custom info to the rightmost graph (top-right corner)
plots_mcar_x3_missing[[5]] <- plots_mcar_x3_missing[[5]] +
  scale_y_continuous(
    name = NULL,
    # Right side: duplicate scale but only use it for the title
    sec.axis = dup_axis(name = "MCAR,\nonly missings in X3")
  ) +theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 8)))

# 3) Combine in a single row; collect a single legend below (optional)
(plots_mcar_x3_missing[[1]] | plots_mcar_x3_missing[[2]] | 
  plots_mcar_x3_missing[[3]]| plots_mcar_x3_missing[[4]]| plots_mcar_x3_missing[[5]]) +
  plot_layout(guides = "collect",
  axis_titles = "collect") &
  theme(legend.position = "bottom")



#2) MCAR, all have missings
dfs_mcar_all_miss_nonlin <- list()
i <- 1

time_start_mcar_all <- Sys.time()
for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- cycles
  
  dfs_mcar_all_miss_nonlin[[i]] <- mc$mc_study_furrr(
    methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
    true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
    data_generator = mc$generate_data_nonlinear_weak,
    n = 500, cycles = mc_cycles, miss_vars = c("X1","X2","X3"), true_means = c("X3" = 4.55),
    miss = "MCAR", miss_rates =  c(0.2,0.5,mis_rate), miss_aux = NULL, seed = NA
  )
  
  i <- i+1
  
}
# --- Stop timer ---
time_end_mcar_all <- Sys.time()
runtime_mcar_all <- time_end_mcar_all - time_start_mcar_all
paste("MC for MAR, missings in all with ", cycles, " Simulation cycles ran for: ", runtime_mcar_all)




# process each df and return a list of summarised tibbles
mcar_all_missing_raw <- lapply(seq_along(dfs_mcar_all_miss_nonlin), function(k) {
  dfs_mcar_all_miss_nonlin[[k]] %>%
    dplyr::group_by(method, term) %>%
    dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(missperc = miss_perc[k])
})


### Coverage Plots

mcar_all_missing_combined <- bind_rows(mcar_all_missing_raw, .id = "source_id")

#List with all data split by Parameter
mcar_all_missing_parameter_info <- mcar_all_missing_combined %>% group_split(term, .keep = TRUE)


#List with coverage plots for each parameter
plots_mcar_all_missing <- vector("list", length(mcar_all_missing_parameter_info))

for (i in seq_along(mcar_all_missing_parameter_info)) {
  plots_mcar_all_missing[[i]] <- ggplot(mcar_all_missing_parameter_info[[i]]) +
    geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
    geom_point(aes(x = missperc, y = coverage, color = method)) +
    geom_hline(yintercept = 0.9, linetype = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = NULL, x = "Missing in X3 (%)", y = NULL) +
    theme_classic()
}


# 1) Add y-axis title only to the leftmost plot
plots_mcar_all_missing[[1]] <- plots_mcar_all_missing[[1]] + labs(y = "Coverage")

# 2) Add custom info to the rightmost graph (top-right corner)
plots_mcar_all_missing[[5]] <- plots_mcar_all_missing[[5]] +
  scale_y_continuous(
    name = NULL,
    # Right side: duplicate scale but only use it for the title
    sec.axis = dup_axis(name = "MCAR,\n20% missings in X1,\n50% in X2")
  ) +theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 8)))


# 3) Combine in a single row; collect a single legend below (optional)
(plots_mcar_all_missing[[1]] | plots_mcar_all_missing[[2]] | plots_mcar_all_missing[[3]]|
   plots_mcar_all_missing[[4]] | plots_mcar_all_missing[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



## 3) MAR, missings in X3

dfs_mar_X3_miss_nonlin <- list()
i <- 1

## ---- timed MC Start ---
time_start_mar_x3 <- Sys.time()
paste("M3, start",time_start_mar_x3 )
for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- cycles
  
  dfs_mar_X3_miss_nonlin[[i]] <- mc$mc_study_furrr(
    methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
    true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
    data_generator = mc$generate_data_nonlinear_weak,
    n = 500, cycles = mc_cycles, miss_vars = "X3", true_means = c("X3" = 4.55),
    miss = "MAR", miss_rates =  mis_rate, miss_aux = "X1", seed = NA
  )
  
  i <- i+1
  
}

# --- Stop timer ---
time_end_mar_x3 <- Sys.time()
runtime_mar_x3 <- time_end_mar_x3 - time_start_mar_x3
paste("MC for MAR, missings in X3 with ", cycles, " Simulation cycles ran for: ", runtime_mar_x3)

# process each df and return a list of summarised tibbles
mar_x3_missing_raw <- lapply(seq_along(dfs_mar_X3_miss_nonlin), function(k) {
  dfs_mar_X3_miss_nonlin [[k]] %>%
    dplyr::group_by(method, term) %>%
    dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(missperc = miss_perc[k])
})


### Coverage Plots

mar_x3_missing_combined <- bind_rows(mar_x3_missing_raw, .id = "source_id")

#List with all data split by Parameter
mar_x3_missing_parameter_info <- mar_x3_missing_combined %>% group_split(term, .keep = TRUE)

#List with coverage plots for each parameter
plots_mar_x3_missing <- vector("list", length(mar_x3_missing_parameter_info))

for (i in seq_along(mar_x3_missing_parameter_info)) {
  plots_mar_x3_missing[[i]] <- ggplot(mar_x3_missing_parameter_info[[i]]) +
    geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
    geom_point(aes(x = missperc, y = coverage, color = method)) +
    geom_hline(yintercept = 0.9, linetype = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = NULL, x = "Missing in X3 (%)", y = NULL) +
    theme_classic()
}


# 1) Add y-axis title only to the leftmost plot
plots_mar_x3_missing[[1]] <- plots_mar_x3_missing[[1]] + labs(y = "Coverage")

# 2) Add custom info to the rightmost graph (top-right corner)
plots_mar_x3_missing[[5]] <- plots_mar_x3_missing[[5]] +
  scale_y_continuous(
    name = NULL,
    # Right side: duplicate scale but only use it for the title
    sec.axis = dup_axis(name = "MAR,\nOnly missings in X3")
  ) +theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 8)))


# 3) Combine in a single row; collect a single legend below (optional)
(plots_mar_x3_missing[[1]] | plots_mar_x3_missing[[2]] | 
  plots_mar_x3_missing[[3]]| plots_mar_x3_missing[[4]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")





## 4) MAR, missings in all

dfs_mar_all_miss_nonlin <- list()
i <- 1

## ---- timed MC Start ---
time_start_mar_all <- Sys.time()
paste("M4, start",time_start_mar_all )
for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- cycles
  
  dfs_mar_all_miss_nonlin[[i]] <- mc$mc_study_furrr(
    methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
    true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
    data_generator = mc$generate_data_nonlinear_weak,
    n = 500, cycles = mc_cycles, miss_vars = c("X1","X2","X3"), true_means = c("X3" = 4.55),
    miss = c("MCAR","MCAR","MAR"), miss_rates =  c(0.2,0.5,mis_rate), miss_aux = c(NULL, NULL, "X1"), seed = NA
  )
  
  i <- i+1
  
}
# --- Stop timer ---
time_end_mar_all <- Sys.time()
runtime_mar_all <- time_end_mar_all - time_start_mar_all
paste("MC for MAR, missings in all with ", cycles, " Simulation cycles ran for: ", runtime_mar_x3)

# process each df and return a list of summarised tibbles
mar_all_missing_raw <- lapply(seq_along(dfs_mar_all_miss_nonlin), function(k) {
  dfs_mar_all_miss_nonlin[[k]] %>%
    dplyr::group_by(method, term) %>%
    dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(missperc = miss_perc[k])
})


### Coverage Plots

mar_all_missing_combined <- bind_rows(mar_all_missing_raw, .id = "source_id")

#List with all data split by Parameter
mar_all_missing_parameter_info <- mar_all_missing_combined %>% group_split(term, .keep = TRUE)

#List with coverage plots for each parameter
plots_mar_all_missing <- vector("list", length(mar_all_missing_parameter_info))

for (i in seq_along(mar_all_missing_parameter_info)) {
  plots_mar_all_missing[[i]] <- ggplot(mar_all_missing_parameter_info[[i]]) +
    geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4) +
    geom_point(aes(x = missperc, y = coverage, color = method)) +
    geom_hline(yintercept = 0.9, linetype = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = NULL, x = "Missing in X3 (%)", y = NULL) +
    theme_classic()
}


# 1) Add y-axis title only to the leftmost plot
plots_mar_all_missing[[1]] <- plots_mar_all_missing[[1]] + labs(y = "Coverage")

# 2) Add custom info to the rightmost graph (top-right corner)
plots_mar_all_missing[[5]] <- plots_mar_all_missing[[5]] +
  scale_y_continuous(
    name = NULL,
    # Right side: duplicate scale but only use it for the title
    sec.axis = dup_axis(name = "MAR,\n20% missings in X1,\n50% in X2")
  ) +theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 8)))


# 3) Combine in a single row; collect a single legend below (optional)
(plots_mar_all_missing[[1]] | plots_mar_all_missing[[2]] |
  plots_mar_all_missing[[3]]| plots_mar_all_missing[[4]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



(plots_mcar_x3_missing[[1]] | 
 plots_mcar_x3_missing[[2]] | 
 plots_mcar_x3_missing[[3]] |
  plots_mcar_x3_missing[[4]] |
    plots_mcar_x3_missing[[5]] |
  plots_mcar_all_missing[[1]] | 
 plots_mcar_all_missing[[2]] | 
 plots_mcar_all_missing[[3]]|
  plots_mcar_all_missing[[4]]|
    plots_mcar_all_missing[[5]]|
  plots_mar_x3_missing[[1]] |
  plots_mar_x3_missing[[2]] |
  plots_mar_x3_missing[[3]] |
  plots_mar_x3_missing[[4]] |
  plots_mar_x3_missing[[5]] |
  plots_mar_all_missing[[1]] | 
  plots_mar_all_missing[[2]] |
  plots_mar_all_missing[[3]] |
  plots_mar_all_missing[[4]] |
  plots_mar_all_missing[[5]]
  ) +
  plot_layout(
    ncol = 5,     #
    nrow = 4,     
    guides = "collect",
    axis_titles = "collect"
  ) &
  theme(legend.position = "bottom")




'save(dfs_mcar_X3_miss_nonlin,
  dfs_mcar_all_miss_nonlin,
  dfs_mar_X3_miss_nonlin,
  dfs_mar_all_miss_nonlin,
  file = "project/raw_mc_results/mc_raw_results_s=300.RData")
'
