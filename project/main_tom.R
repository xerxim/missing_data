#main R von Tom (;


# Place for main analysis code.
# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse, mice,cowplot,gridExtra)
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.


mc_cycles <- 5

###### MCAR, Missings in X3
t_10mX3 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles, 
  miss_vars =c("X3"), 
  miss = "MCAR", miss_rates = c(0.1), miss_aux = NULL, seed = 161
)

t_20mX3 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles, 
  miss_vars =c("X3"), 
  miss = "MCAR", miss_rates = c(0.2), miss_aux = NULL, seed = 161
)

t_30mX3 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles, 
  miss_vars =c("X3"), 
  miss = "MCAR", miss_rates = c(0.3), miss_aux = NULL, seed = 161
)

t_40mX3 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles, 
  miss_vars =c("X3"), 
  miss = "MCAR", miss_rates = c(0.4), miss_aux = NULL, seed = 161
)

t_50mX3 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles, 
  miss_vars =c("X3"), 
  miss = "MCAR", miss_rates = c(0.5), miss_aux = NULL, seed = 161
)


# put your data frames in a list
dfs <- list(
  rbind(t_10mX3[[1]],t_10mX3[[2]]),
  rbind(t_20mX3[[1]],t_20mX3[[2]] ),
  rbind(t_30mX3[[1]],t_30mX3[[2]] ),
  rbind(t_40mX3[[1]],t_40mX3[[2]] ),
  rbind(t_50mX3[[1]],t_50mX3[[2]] )
)

rm(t_10mX3, t_20mX3, t_30mX3, t_40mX3, t_50mX3)

miss_perc <- c(10, 20, 30, 40, 50)

# process each df and return a list of summarised tibbles
mcar_x3_missing_raw <- lapply(seq_along(dfs), function(k) {
  dfs[[k]] %>%
    dplyr::group_by(method, term) %>%
    dplyr::summarise(coverage = mean(as.numeric(cover), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(missperc = miss_perc[k])
})


### Coverage Plots

mcar_x3_missing_combined <- bind_rows(mcar_x3_missing_raw, .id = "source_id")

#List with all data split by Parameter
mcar_x3_missing_parameter_info <- mcar_x3_missing_combined %>% group_split(term, .keep = TRUE)

plot_names <- c("Beta 0", "Beta 1", "Beta 2", "Mu 3")

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
plots_mcar_x3_missing[[4]] <- plots_mcar_x3_missing[[4]] +
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
  plots_mcar_x3_missing[[3]]| plots_mcar_x3_missing[[4]]) +
  plot_layout(guides = "collect",
  axis_titles = "collect") &
  theme(legend.position = "bottom")



####### MCAR, Missings in all
mcar_all_missing_X3_10 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MCAR", miss_rates = c(0.2,0.5,0.1), miss_aux = NULL, seed = 161
)

mcar_all_missing_X3_20 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MCAR", miss_rates = c(0.2,0.5,0.2), miss_aux = NULL, seed = 161
)

mcar_all_missing_X3_30 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MCAR", miss_rates = c(0.2,0.5,0.3), miss_aux = NULL, seed = 161
)

mcar_all_missing_X3_40 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MCAR", miss_rates = c(0.2,0.5,0.4), miss_aux = NULL, seed = 161
)

mcar_all_missing_X3_50 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MCAR", miss_rates = c(0.2,0.5,0.5), miss_aux = NULL, seed = 161
)

##### PLOTTING


dfs <- list(
  rbind(mcar_all_missing_X3_10[[1]],mcar_all_missing_X3_10[[2]]),
  rbind(mcar_all_missing_X3_20[[1]],mcar_all_missing_X3_20[[2]]),
  rbind(mcar_all_missing_X3_30[[1]],mcar_all_missing_X3_30[[2]]),
  rbind(mcar_all_missing_X3_40[[1]],mcar_all_missing_X3_40[[2]]),
  rbind(mcar_all_missing_X3_50[[1]],mcar_all_missing_X3_50[[2]])
)

rm(mcar_all_missing_X3_10,
  mcar_all_missing_X3_20,
  mcar_all_missing_X3_30,
  mcar_all_missing_X3_40,
  mcar_all_missing_X3_50)
# process each df and return a list of summarised tibbles
mcar_all_missing_raw <- lapply(seq_along(dfs), function(k) {
  dfs[[k]] %>%
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
plots_mcar_all_missing[[4]] <- plots_mcar_all_missing[[4]] +
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
(plots_mcar_all_missing[[1]] | plots_mcar_all_missing[[2]] | plots_mcar_all_missing[[3]]| plots_mcar_all_missing[[4]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")





(plots_mcar_x3_missing[[1]] | 
 plots_mcar_x3_missing[[2]] | 
 plots_mcar_x3_missing[[3]] |
  plots_mcar_x3_missing[[4]] |
  plots_mcar_all_missing[[1]] | 
 plots_mcar_all_missing[[2]] |
 plots_mcar_all_missing[[3]] | 
 plots_mcar_all_missing[[4]] ) +
  plot_layout(
    ncol = 4,     # 3 columns per row
    nrow = 2,     # 2 rows total
    guides = "collect"
  ) &
  theme(legend.position = "bottom")

##### MAR X1 missing

mar_x3_missing_X3_10 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars ="X3", 
  miss = "MAR", miss_rates = 0.1, miss_aux = c("X2"), seed = 161
)

mar_x3_missing_X3_20 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars ="X3", 
  miss = "MAR", miss_rates = 0.2, miss_aux = c("X2"), seed = 161
)

mar_x3_missing_X3_30 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars ="X3", 
  miss = "MAR", miss_rates = 0.3, miss_aux = c("X2"), seed = 161
)

mar_x3_missing_X3_40 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars ="X3", 
  miss = "MAR", miss_rates = 0.4, miss_aux = c("X2"), seed = 161
)

mar_x3_missing_X3_50 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars ="X3", 
  miss = "MAR", miss_rates = 0.5, miss_aux = c("X2"), seed = 161
)


##### PLOTTING

# put your data frames in a list
dfs <- list(
  rbind(mar_x3_missing_X3_10[[1]],mar_x3_missing_X3_10[[2]]),
  rbind(mar_x3_missing_X3_20[[1]],mar_x3_missing_X3_20[[2]]),
  rbind(mar_x3_missing_X3_30[[1]],mar_x3_missing_X3_30[[2]]),
  rbind(mar_x3_missing_X3_40[[1]],mar_x3_missing_X3_40[[2]]),
  rbind(mar_x3_missing_X3_50[[1]],mar_x3_missing_X3_50[[2]])
)

# process each df and return a list of summarised tibbles
mar_x3_missing_raw <- lapply(seq_along(dfs), function(k) {
  dfs[[k]] %>%
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
plots_mar_x3_missing[[4]] <- plots_mar_x3_missing[[4]] +
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



##### MAR all missing

mar_all_missing_X3_10 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MAR", miss_rates = c(0.2,0.5,0.1), miss_aux = c("X1","X2","X3"), seed = 161
)

mar_all_missing_X3_20 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MAR", miss_rates = c(0.2,0.5,0.2), miss_aux = c("X1","X2","X3"), seed = 161
)

mar_all_missing_X3_30 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MAR", miss_rates = c(0.2,0.5,0.3), miss_aux = c("X1","X2","X3"), seed = 161
)

mar_all_missing_X3_40 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MAR", miss_rates = c(0.2,0.5,0.4), miss_aux = c("X1","X2","X3"), seed = 161
)

mar_all_missing_X3_50 <- mc$mc_study(
  methods = c("cart","cart_boot", "pmm"), m = 30, 
  formula = "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
  c("X3" = 12.8), 
  n = 500, cycles =  mc_cycles,
  miss_vars =c("X1","X2","X3"), 
  miss = "MAR", miss_rates = c(0.2,0.5,0.5), miss_aux = c("X1","X2","X3"), seed = 161
)


##### PLOTTING

# put your data frames in a list
dfs <- list(
  rbind(mar_all_missing_X3_10[[1]],mar_all_missing_X3_10[[2]]),
  rbind(mar_all_missing_X3_20[[1]],mar_all_missing_X3_20[[2]]),
  rbind(mar_all_missing_X3_30[[1]],mar_all_missing_X3_30[[2]]),
  rbind(mar_all_missing_X3_40[[1]],mar_all_missing_X3_40[[2]]),
  rbind(mar_all_missing_X3_50[[1]],mar_all_missing_X3_50[[2]])
)

# process each df and return a list of summarised tibbles
mar_all_missing_raw <- lapply(seq_along(dfs), function(k) {
  dfs[[k]] %>%
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
plots_mar_all_missing[[4]] <- plots_mar_all_missing[[4]] +
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
  plots_mcar_all_missing[[1]] | 
 plots_mcar_all_missing[[2]] | 
 plots_mcar_all_missing[[3]]|
  plots_mcar_all_missing[[4]]|
  plots_mar_x3_missing[[1]] |
  plots_mar_x3_missing[[2]] |
  plots_mar_x3_missing[[3]] |
  plots_mar_x3_missing[[4]] |
  plots_mar_all_missing[[1]] | 
  plots_mar_all_missing[[2]] |
  plots_mar_all_missing[[3]] |
  plots_mar_all_missing[[4]]
  ) +
  plot_layout(
    ncol = 4,     #
    nrow = 4,     
    guides = "collect",
    axis_titles = "collect"
  ) &
  theme(legend.position = "bottom")

